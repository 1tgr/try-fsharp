namespace Tim.TryFSharp.Service

open System
open System.Diagnostics
open System.IO
open System.Net
open System.Threading
open System.Runtime.InteropServices
open Mono.Unix
open Mono.Unix.Native

type Session =
    {
        [<JsonName("_rev")>]       Rev        : string option
        [<JsonName("owner")>]      Owner      : string
        [<JsonName("host")>]       Host       : string
        [<JsonName("servicePid")>] ServicePid : int64
        [<JsonName("fsiPid")>]     FsiPid     : int64 option
    }

type Message =
    {
        [<JsonName("_rev")>]        Rev         : string option
        [<JsonName("date")>]        Date        : string option
        [<JsonName("messageType")>] MessageType : string
        [<JsonName("sessionId")>]   SessionId   : string
        [<JsonName("message")>]     Message     : string
        [<JsonName("queueStatus")>] QueueStatus : string option
    }

module TryFSharpDB =
    let getSession (baseUri : Uri) (id : string) : Session option =
        try
            Some (CouchDB.getDocument baseUri id)
        with :? WebException as ex ->
            match ex.Response with
            | :? HttpWebResponse as response when response.StatusCode = HttpStatusCode.NotFound -> None
            | _ -> reraise ()

    let putSession : Uri -> string -> Session -> SaveResponse =
        CouchDB.putDocument

    let safePutSession (baseUri : Uri) (id : string) (session : Session) : SaveResponse option =
        CouchDB.conflicted (putSession baseUri id) session

    let getMessage : Uri -> string -> Message =
        CouchDB.getDocument

    let putMessage : Uri -> string -> Message -> SaveResponse =
        CouchDB.putDocument
    
    let postMessage : Uri -> Message -> SaveResponse =
        CouchDB.postDocument

type Claim =
    | OwnSession of Process
    | OtherSession of string
    | ProcessFailed of Exception

type MailboxMessage =
    | Exit of AsyncReplyChannel<App>
    | StdIn of string * Message
    | StdOut of Message
    | Recycle of string * string * Process
    
and App =
    {
        OwnServerId : string
        BaseUri : Uri
        OwnSessions : Map<string, string * Process>
    } with
    member private this.ClaimSession (inbox : MailboxProcessor<_>) (sessionId : string) : App * Claim =
        let id = sprintf "session-%s" sessionId
        match Map.tryFind id this.OwnSessions with
        | Some (_, proc) ->
            this, OwnSession proc

        | None ->
            match TryFSharpDB.getSession this.BaseUri id with
            | Some session ->
                this, OtherSession session.Owner

            | None ->
                let session : Session =
                    { 
                        Rev = None
                        Owner = this.OwnServerId
                        Host = Dns.GetHostName()
                        ServicePid = int64 (Process.GetCurrentProcess().Id)
                        FsiPid = None
                    }

                match TryFSharpDB.safePutSession this.BaseUri id session with
                | Some rev ->
                    let programFiles =
                        match Environment.GetEnvironmentVariable("ProgramFiles(x86)") with
                        | null -> Environment.GetFolderPath(Environment.SpecialFolder.ProgramFiles)
                        | s -> s

                    let filename = Path.Combine(programFiles, @"Microsoft F#\v4.0\fsi.exe")

                    let startInfo = new ProcessStartInfo()
                    startInfo.FileName <-
                        if File.Exists(filename) then
                            filename
                        else
                            "fsi"

                    startInfo.WorkingDirectory <- Path.GetTempPath()
                    startInfo.RedirectStandardError <- true
                    startInfo.RedirectStandardInput <- true
                    startInfo.RedirectStandardOutput <- true
                    startInfo.UseShellExecute <- false

                    let proc = new Process()
                    proc.StartInfo <- startInfo

                    (* let resetRecycleTimer =
                        let callback _ = inbox.Post (Recycle (id, rev.Rev, proc))
                        let timer = new Timer(TimerCallback(callback))
                        fun () ->
                            ignore (timer.Change(TimeSpan.FromHours(1.0), TimeSpan.FromMilliseconds(-1.0))) *)

                    let post =
                        function
                        | null -> ()
                        | s ->
                            let message : Message =
                                {
                                    Rev = None
                                    Date = Some (DateTime.UtcNow.ToString("o"))
                                    MessageType = "out"
                                    SessionId = sessionId
                                    Message = s
                                    QueueStatus = None
                                }

                            //resetRecycleTimer ()
                            inbox.Post (StdOut message)

                    //resetRecycleTimer ()
                    proc.OutputDataReceived.Add <| fun args -> post args.Data
                    proc.ErrorDataReceived.Add <| fun args -> post args.Data

                    try
                        ignore (proc.Start())

                        proc.BeginErrorReadLine()
                        proc.BeginOutputReadLine()

                        let rev = TryFSharpDB.putSession this.BaseUri id { session with Rev = Some rev.Rev; FsiPid = Some (int64 proc.Id) }
                        { this with OwnSessions = Map.add id (rev.Rev, proc) this.OwnSessions }, OwnSession proc
                    with ex ->
                        CouchDB.deleteDocument this.BaseUri id rev.Rev
                        this, ProcessFailed ex

                | None ->
                    this.ClaimSession inbox sessionId

    member private this.KillSession (id : string) (rev : string) (proc : Process) : App =
        try
            CouchDB.deleteDocument this.BaseUri id rev
        with _ -> ()

        if not proc.HasExited then
            try
                proc.Kill()
                ignore (proc.WaitForExit(5000))
            with _ -> ()

        proc.Dispose()
        { this with OwnSessions = Map.remove id this.OwnSessions }

    member this.Run (inbox : MailboxProcessor<_>) : Async<unit> =
        async {
            let! message = inbox.Receive()
            match message with
            | Exit reply ->
                reply.Reply this
                return ()

            | StdIn (id, message) ->
                let app, claim = this.ClaimSession inbox message.SessionId
                match claim with
                | OtherSession otherServerId ->
                    fprintfn Console.Error "Ignoring %s - owned by session %s on %s" id message.SessionId otherServerId

                | OwnSession proc ->
                    let rev = TryFSharpDB.putMessage this.BaseUri id { message with QueueStatus = Some "done" }
                    let message = { message with Rev = Some rev.Rev }
                    proc.StandardInput.WriteLine message.Message

                | ProcessFailed ex ->
                    fprintfn Console.Error "%O" ex

                return! app.Run inbox

            | StdOut message ->
                ignore (TryFSharpDB.postMessage this.BaseUri message)
                return! this.Run inbox

            | Recycle (id, rev, proc) ->
                let app = this.KillSession id rev proc
                return! app.Run inbox
        }

    interface IDisposable with
        member this.Dispose() =
            (this, this.OwnSessions)
            ||> Map.fold (fun app id (rev, proc) -> app.KillSession id rev proc)
            |> ignore

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

                MailboxProcessor.Start app.Run

            let subscribe =
                let rec impl lastSeq =
                    async {
                        let! lastSeq, (results : Change<Message> array) = CouchDB.changes baseUri (Some "app/stdin") lastSeq
                        for result in results do
                            let message =
                                match result.Doc with
                                | Some message -> message
                                | None -> TryFSharpDB.getMessage baseUri result.Id

                            if Option.isNone message.QueueStatus then
                                mailbox.Post (StdIn (result.Id, message))

                        return! impl (Some lastSeq)
                    }

                impl None

            try
                Async.Start subscribe
                ignore (ctrlCHandler.WaitHandle.WaitOne())
            finally
                let app = mailbox.PostAndReply Exit :> IDisposable
                app.Dispose()

            0
        with ex ->
            fprintfn Console.Error "%O" ex
            1
