namespace Tim.TryFSharp.Service

open System
open System.Threading
open Tim.TryFSharp.Core

type ServiceState =
    {
        Mailbox : MailboxProcessor<Command>
        CancellationTokenSource : CancellationTokenSource
    } with
    static member Create (config : ServiceConfig) =
        let mailbox =
            let app : App =
                {
                    OwnServerId = string (Guid.NewGuid())
                    BaseUri = config.BaseUri
                    OwnSessions = Map.empty
                    SlowStop = false
                }

            MailboxProcessor.Start (App.run app)

        let subscribe =
            let rec impl lastSeq =
                async {
                    try
                        let! lastSeq, (results : Change<Message> array) = CouchDB.changes config.BaseUri (Some "app/stdin") lastSeq
                        for result in results do
                            let message =
                                match result.Doc with
                                | Some message -> message
                                | None -> TryFSharpDB.getMessage config.BaseUri result.Id

                            if Option.isNone message.QueueStatus then
                                mailbox.Post (StdIn (result.Id, message))

                        return! impl (Some lastSeq)
                    with ex ->
                        Log.info "%O" ex
                        return! impl None
                }

            impl None

        let cts = new CancellationTokenSource()
        Async.Start(subscribe, cts.Token)

        {
            Mailbox = mailbox
            CancellationTokenSource = cts
        }

    member this.SlowStop () =
        this.Mailbox.Post SlowStop

    interface IDisposable with
        member this.Dispose() =
            this.CancellationTokenSource.Cancel()
            this.CancellationTokenSource.Dispose()
            ignore (App.shutdown (this.Mailbox.PostAndReply Exit))

type Service() =
    [<DefaultValue>]
    val mutable state : ServiceState option

    interface IService with
        member this.Start config =
            this.state <- Some (ServiceState.Create config)

        member this.SlowStop () =
            match this.state with
            | Some state -> state.SlowStop()
            | None -> ()

    interface IDisposable with
        member this.Dispose() =
            match this.state with
            | Some state -> (state :> IDisposable).Dispose()
            | None -> ()

            this.state <- None
