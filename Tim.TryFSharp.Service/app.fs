namespace Tim.TryFSharp.Service

open System
open System.Diagnostics
open System.Net
open Tim.TryFSharp.Core

type App =
    {
        OwnServerId : string
        BaseUri : Uri
        OwnSessions : Map<string, string * FsiProcess>
        SlowStop : bool
    }

type Command =
    | Exit of AsyncReplyChannel<App>
    | StdIn of string * Message
    | StdOut of Message
    | SlowStop
    | Recycle of string

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module App =
    let emptySession : Session =
        { 
            Rev = None
            Owner = ""
            Host = Dns.GetHostName()
            ServicePid = int64 (Process.GetCurrentProcess().Id)
            FsiPid = None
            InitNames = [| |]
            InitTexts = [| |]
        }

    type Claim =
        | OwnSession of FsiProcess
        | OtherSession of string * int64
        | ProcessFailed of Exception
        | SlowStopIgnore

    let claimSession (app : App) (inbox : MailboxProcessor<_>) (id : string) : App * Claim =
        let rec impl () =
            match TryFSharpDB.getSession app.BaseUri id with
            | Some session when session.Owner <> app.OwnServerId ->
                app, OtherSession(session.Host, session.ServicePid)

            | s ->
                let session =
                    match s with
                    | Some session -> session
                    | None -> { emptySession with Owner = app.OwnServerId }

                match TryFSharpDB.safePutSession app.BaseUri id session with
                | Some rev ->
                    let session = { session with Rev = rev.Rev }

                    try
                        let info : FsiProcessInfo =
                            {
                                Name = id
                                InitTexts = Array.zip session.InitNames session.InitTexts

                                Print = fun s ->
                                    let message : Message =
                                        {
                                            Rev = None
                                            Date = Some (DateTime.UtcNow.ToString("o"))
                                            MessageType = "out"
                                            SessionId = id
                                            Message = s
                                            QueueStatus = None
                                        }

                                    inbox.Post (StdOut message)
    
                                Recycle = fun () -> inbox.Post (Recycle id)
                            }

                        let proc = new FsiProcess(info)
                        proc.Start()

                        let session = { session with FsiPid = Some (int64 proc.Process.Id) }
                        let rev = TryFSharpDB.putSession app.BaseUri id session
                        { app with OwnSessions = Map.add id (Option.get rev.Rev, proc) app.OwnSessions }, OwnSession proc
                    with ex ->
                        CouchDB.deleteDocument app.BaseUri id (Option.get rev.Rev)
                        app, ProcessFailed ex

                | None ->
                    impl ()

        match Map.tryFind id app.OwnSessions, app.SlowStop with
        | Some (_, proc), _ -> app, OwnSession proc
        | None, true -> app, SlowStopIgnore
        | None, false -> impl ()

    let killSession (app : App) (id : string) (rev : string) (proc : FsiProcess) : App =
        try
            CouchDB.deleteDocument app.BaseUri id rev
        with _ -> ()

        (proc :> IDisposable).Dispose()
        { app with OwnSessions = Map.remove id app.OwnSessions }

    let rec run (app : App) (inbox : MailboxProcessor<_>) : Async<unit> =
        let impl =
            function
            | Exit reply ->
                reply.Reply app
                async { return () }

            | StdIn (id, message) ->
                let app, claim = claimSession app inbox message.SessionId
                match claim with
                | OtherSession (host, pid) ->
                    Log.info "Ignoring %s - owned by session %s on %s (pid %d)" id message.SessionId host pid

                | OwnSession proc ->
                    let rev = TryFSharpDB.putMessage app.BaseUri id { message with QueueStatus = Some "done" }
                    let message = { message with Rev = rev.Rev }
                    proc.Process.StandardInput.WriteLine message.Message

                | ProcessFailed ex ->
                    Log.info "%O" ex

                | SlowStopIgnore ->
                    Log.info "Ignoring %s - in slow stop mode" id

                run app inbox

            | StdOut message ->
                ignore (TryFSharpDB.postMessage app.BaseUri message)
                run app inbox

            | SlowStop ->
                run { app with SlowStop = true } inbox

            | Recycle id ->
                let app =
                    match Map.tryFind id app.OwnSessions with
                    | Some (rev, proc) -> killSession app id rev proc
                    | None -> app

                run app inbox

        async {
            try
                let! command = inbox.Receive()
                return! impl command
            with ex ->
                Log.info "%O" ex
                return! run app inbox
        }

    let shutdown (app : App) =
        (app, app.OwnSessions)
        ||> Map.fold (fun app id (rev, proc) -> killSession app id rev proc)

