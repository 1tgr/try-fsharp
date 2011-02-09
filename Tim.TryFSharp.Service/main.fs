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

type MailboxMessage =
    | Exit of AsyncReplyChannel<App>
    | StdIn of string * Message
    | StdOut of Message
    
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

                    let startInfo = new ProcessStartInfo()
                    startInfo.FileName <- Path.Combine(programFiles, @"Microsoft F#\v4.0\fsi.exe")
                    startInfo.WorkingDirectory <- Path.GetTempPath()
                    startInfo.RedirectStandardError <- true
                    startInfo.RedirectStandardInput <- true
                    startInfo.RedirectStandardOutput <- true
                    startInfo.UseShellExecute <- false

                    let proc = new Process()
                    proc.StartInfo <- startInfo

                    let post (s : string) =
                        let message : Message =
                            {
                                Rev = None
                                Date = Some (DateTime.UtcNow.ToString("o"))
                                MessageType = "out"
                                SessionId = sessionId
                                Message = s
                                QueueStatus = None
                            }

                        inbox.Post (StdOut message)

                    proc.OutputDataReceived.Add <| fun args -> post args.Data
                    proc.ErrorDataReceived.Add <| fun args -> post args.Data

                    ignore (proc.Start())
                    proc.BeginErrorReadLine()
                    proc.BeginOutputReadLine()

                    let rev = TryFSharpDB.putSession this.BaseUri id { session with Rev = Some rev.Rev; FsiPid = Some (int64 proc.Id) }
                    { this with OwnSessions = Map.add id (rev.Rev, proc) this.OwnSessions }, OwnSession proc

                | None ->
                    this.ClaimSession inbox sessionId

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

                return! app.Run inbox

            | StdOut message ->
                ignore (TryFSharpDB.postMessage this.BaseUri message)
                return! this.Run inbox
        }

    interface IDisposable with
        member this.Dispose() =
            for id, (rev, proc) in Map.toSeq this.OwnSessions do
                try
                    CouchDB.deleteDocument this.BaseUri id rev
                with _ -> ()

                if not proc.HasExited then
                    try
                        proc.Kill()
                        proc.WaitForExit()
                    with _ -> ()

                proc.Dispose()

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
                        let! lastSeq, results = CouchDB.changes baseUri (Some "app/stdin") lastSeq
                        for result in results do
                            match TryFSharpDB.getMessage baseUri result.Id with
                            | { QueueStatus = None } as message -> mailbox.Post (StdIn (result.Id, message))
                            | _ -> ()

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
