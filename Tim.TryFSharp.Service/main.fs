namespace Tim.TryFSharp.Service

open System
open System.Threading
open System.Runtime.InteropServices
open Mono.Unix
open Mono.Unix.Native
open Tim.TryFSharp.Core

type ICtrlCHandler =
    inherit IDisposable
    abstract WaitHandle : WaitHandle

type MonoCtrlCHandler() =
    let signal = new UnixSignal(Signum.SIGINT)
    interface ICtrlCHandler with
        member this.WaitHandle = signal :> WaitHandle
        member this.Dispose() = signal.Dispose()

type ConsoleCtrlDelegate = delegate of int -> bool

type WindowsCtrlCHandler() =
    [<DllImport("kernel32.dll")>]
    static extern bool SetConsoleCtrlHandler(ConsoleCtrlDelegate handlerRoutine, bool add)

    let event = new ManualResetEvent(false)
    let handler = ConsoleCtrlDelegate(fun _ -> ignore (event.Set()); true)
    do ignore (SetConsoleCtrlHandler(handler, true))

    interface ICtrlCHandler with
        member this.WaitHandle = event :> WaitHandle
        member this.Dispose() = ignore (SetConsoleCtrlHandler(handler, false))

module Main =
    let isMono =
        match Type.GetType ("Mono.Runtime") with
        | null -> false
        | _ -> true

    [<EntryPoint>]
    let main args =
        try
            let baseUri =
                match args with
                [| uri |] -> Uri(uri)
                | _ -> Uri("http://tryfs.net/tryfs/")

            use ctrlCHandler =
                if isMono then
                    new MonoCtrlCHandler() :> ICtrlCHandler
                else
                    new WindowsCtrlCHandler() :> ICtrlCHandler

            let mailbox =
                let app : App =
                    {
                        OwnServerId = string (Guid.NewGuid())
                        BaseUri = baseUri
                        OwnSessions = Map.empty
                    }

                MailboxProcessor.Start (App.run app)

            let subscribe =
                let rec impl lastSeq =
                    async {
                        try
                            let! lastSeq, (results : Change<Message> array) = CouchDB.changes baseUri (Some "app/stdin") lastSeq
                            for result in results do
                                let message =
                                    match result.Doc with
                                    | Some message -> message
                                    | None -> TryFSharpDB.getMessage baseUri result.Id

                                if Option.isNone message.QueueStatus then
                                    mailbox.Post (StdIn (result.Id, message))

                            return! impl (Some lastSeq)
                        with ex ->
                            Log.info "%O" ex
                            return! impl None
                    }

                impl None

            try
                Async.Start subscribe
                ignore (ctrlCHandler.WaitHandle.WaitOne())
            finally
                ignore (App.shutdown (mailbox.PostAndReply Exit))

            0
        with ex ->
            Log.info "%O" ex
            1
