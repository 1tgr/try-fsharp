namespace Tim.TryFSharp.Monitor

open System
open System.Configuration
open System.IO
open System.Net
open System.Reflection
open System.Runtime.InteropServices
open System.Threading
open Mono.Unix
open Mono.Unix.Native
open Tim.TryFSharp.Core

type Attachment =
    {
        [<JsonName("length")>] Length : int64
    }

type DesignDoc =
    {
        [<JsonName("_id")>]          Id : string
        [<JsonName("_rev")>]         Rev : string
        [<JsonName("_attachments")>] Attachments : Map<string, Attachment>
        [<JsonName("launcher")>]     Launcher : string
    }

type Command =
    | Exit of AsyncReplyChannel<unit>
    | Load of DesignDoc

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
                | _ ->
                    match ConfigurationManager.AppSettings.["BaseUri"] with
                    | "" | null -> Uri("http://tryfs.net/tryfs/")
                    | s -> Uri(s)

            use ctrlCHandler =
                if isMono then
                    new MonoCtrlCHandler() :> ICtrlCHandler
                else
                    new WindowsCtrlCHandler() :> ICtrlCHandler

            let config : ServiceConfig = { BaseUri = baseUri }

            let rec run (state : (AppDomain * IService) option) (inbox : MailboxProcessor<Command>) =
                let impl =
                    function
                    | Exit r ->
                        match state with
                        | Some (_, service) -> service.Dispose()
                        | None -> ()

                        r.Reply ()
                        async { return () }

                    | Load doc ->
                        let serviceName =
                            match doc.Id.Split([| '/' |], 2) with
                            | [| s; name |] when s = "_design" -> name
                            | _ -> doc.Id

                        let typeName, assemblyName =
                            match doc.Launcher.Split([| ',' |], 2) with
                            | [| typeName; assemblyName |] -> typeName, assemblyName
                            | _ -> doc.Launcher, serviceName

                        let dir = Path.Combine(Path.GetTempPath(), serviceName, doc.Rev)
                        ignore (Directory.CreateDirectory(dir))

                        use client = new WebClient()
                        for name, _ in Map.toSeq doc.Attachments do
                            let uri = Uri(config.BaseUri, sprintf "%s/%s?rev=%s" doc.Id name doc.Rev)
                            let filename = Path.Combine(dir, name)
                            Log.info "download %O => %s" uri filename
                            client.DownloadFile(uri, filename)

                        let setup = AppDomainSetup()
                        setup.ApplicationBase <- dir

                        let ad = AppDomain.CreateDomain(serviceName + " " + doc.Rev, null, setup)
                        let service : IService = unbox (ad.CreateInstanceAndUnwrap(assemblyName, typeName))
                        service.Start config

                        match state with
                        | Some (_, service) -> service.Dispose()
                        | None -> ()

                        run (Some (ad, service)) inbox

                async {
                    try
                        let! command = inbox.Receive()
                        return! impl command
                    with ex ->
                        Log.info "%O" ex
                        return! run state inbox
                }

            let mailbox = MailboxProcessor.Start (run None)

            let subscribe lastSeq =
                let rec impl lastSeq =
                    async {
                        try
                            let! lastSeq, (results : Change<DesignDoc> array) = CouchDB.changes baseUri (Some "app/deploy") lastSeq
                            for result in results do
                                mailbox.Post (Load (Option.get result.Doc))

                            return! impl (Some lastSeq)
                        with ex ->
                            Log.info "%O" ex
                            return! impl None
                    }

                impl (Some lastSeq)

            let lastSeq = CouchDB.updateSeq baseUri

            "_design/Tim.TryFSharp.Service"
            |> CouchDB.getDocument baseUri
            |> Load
            |> mailbox.Post

            try
                Async.Start (subscribe lastSeq)
                ignore (ctrlCHandler.WaitHandle.WaitOne())
            finally
                mailbox.PostAndReply Exit

            0
        with ex ->
            Log.info "%O" ex
            1
