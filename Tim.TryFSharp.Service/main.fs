namespace Tim.TryFSharp.Service

open System
open System.Diagnostics
open System.IO
open System.Net
open System.Runtime.InteropServices

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

type Claim =
    | OwnSession of Process
    | OtherSession of string

module Main =
    let baseUri = Uri("http://www.partario.com/couchdb/tryfs/")
    let ownSessions : Map<string, string * Process> ref = ref Map.empty

    let getSession (id : string) : Session option =
        try
            Some (CouchDB.getDocument baseUri id)
        with :? WebException as ex ->
            match ex.Response with
            | :? HttpWebResponse as response when response.StatusCode = HttpStatusCode.NotFound -> None
            | _ -> reraise ()

    let putSession : string -> Session -> SaveResponse =
        CouchDB.putDocument baseUri

    let safePutSession (id : string) : Session -> SaveResponse option =
        CouchDB.conflicted (putSession id)

    let getMessage : string -> Message =
        CouchDB.getDocument baseUri

    let putMessage (id : string) : Message -> SaveResponse =
        CouchDB.putDocument baseUri id
    
    let postMessage : Message -> SaveResponse =
        CouchDB.postDocument baseUri
    
    let claimSession : string -> Claim =
        let ownServerId = string (Guid.NewGuid())

        let rec impl sessionId =
            let id = sprintf "session-%s" sessionId
            match Map.tryFind id !ownSessions with
            | Some (_, proc) ->
                OwnSession proc

            | None ->
                match getSession id with
                | Some session ->
                    OtherSession session.Owner

                | None ->
                    let session : Session =
                        { 
                            Rev = None
                            Owner = ownServerId
                            Host = Dns.GetHostName()
                            ServicePid = int64 (Process.GetCurrentProcess().Id)
                            FsiPid = None
                        }

                    match safePutSession id session with
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

                            ignore (postMessage message)

                        proc.OutputDataReceived.Add <| fun args -> post args.Data
                        proc.ErrorDataReceived.Add <| fun args -> post args.Data

                        ignore (proc.Start())
                        proc.BeginErrorReadLine()
                        proc.BeginOutputReadLine()

                        let rev = putSession id { session with Rev = Some rev.Rev; FsiPid = Some (int64 proc.Id) }
                        ownSessions := Map.add id (rev.Rev, proc) !ownSessions
                        OwnSession proc

                    | None ->
                        impl id

        impl

    let dequeue (id : string) : Message option =
        match getMessage id with
        | { Message.QueueStatus = None } as message ->
            match claimSession message.SessionId with
            | OtherSession otherServerId ->
                fprintfn Console.Error "Ignoring %s - owned by session %s on %s" id message.SessionId otherServerId
                None

            | OwnSession proc ->
                let rev = putMessage id { message with QueueStatus = Some "done" }
                let message = { message with Rev = Some rev.Rev }
                proc.StandardInput.WriteLine message.Message
                Some message

        | _ ->
            None

    let rec subscribe (lastSeq : int64 option) =
        let lastSeq, results = CouchDB.changes baseUri (Some "app/stdin") lastSeq
        for result in results do
            try
                ignore (dequeue result.Id)
            with ex ->
                fprintfn Console.Error "%O" ex

        subscribe (Some lastSeq)

    let cleanup () =
        for id, (rev, proc) in Map.toSeq !ownSessions do
            try
                CouchDB.deleteDocument baseUri id rev
            with _ -> ()

            if not proc.HasExited then
                try
                    proc.Kill()
                    proc.WaitForExit()
                with _ -> ()

            proc.Dispose()

    type ConsoleCtrlDelegate = delegate of int -> bool

    [<DllImport("kernel32.dll")>]
    extern bool SetConsoleCtrlHandler(ConsoleCtrlDelegate handlerRoutine, bool add)

    [<EntryPoint>]
    let main _ =
        try
            let handler = ConsoleCtrlDelegate(fun _ -> cleanup (); false)
            ignore (SetConsoleCtrlHandler(handler, true))

            try
                subscribe None
            finally
                ignore (SetConsoleCtrlHandler(handler, false))

            0
        with ex ->
            fprintfn Console.Error "%O" ex
            1
