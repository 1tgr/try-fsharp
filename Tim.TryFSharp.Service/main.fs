namespace Tim.TryFSharp.Service

open System
open System.Diagnostics
open System.IO
open System.Net

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
        [<JsonName("_rev")>]        Rev         : string
        [<JsonName("date")>]        Date        : string
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

    let putSession (id : string) : Session -> string option =
        CouchDB.conflicted (CouchDB.putDocument baseUri id)

    let getMessage : string -> Message =
        CouchDB.getDocument baseUri

    let putMessage (id : string) : Message -> string =
        CouchDB.putDocument baseUri id
    
    let claimSession : string -> Claim =
        let ownServerId = string (Guid.NewGuid())

        let rec impl id =
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

                    match putSession id session with
                    | Some rev ->
                        let programFiles =
                            match Environment.GetEnvironmentVariable("ProgramFiles(x86)") with
                            | null -> Environment.GetFolderPath(Environment.SpecialFolder.ProgramFiles)
                            | s -> s

                        let startInfo = new ProcessStartInfo()
                        startInfo.FileName <- Path.Combine(programFiles, @"Microsoft F#\v4.0\fsi.exe")
                        startInfo.WorkingDirectory <- Path.GetTempPath()
                        //startInfo.RedirectStandardError <- true
                        startInfo.RedirectStandardInput <- true
                        //startInfo.RedirectStandardOutput <- true
                        startInfo.UseShellExecute <- false

                        let proc = new Process()
                        proc.StartInfo <- startInfo
                        ignore (proc.Start())

                        let session = { session with FsiPid = Some (int64 proc.Id) }
                        ownSessions := Map.add id (rev, proc) !ownSessions
                        ignore (putSession id session)
                        OwnSession proc

                    | None ->
                        impl id

        impl

    let dequeue (id : string) : Message option =
        match getMessage id with
        | { Message.QueueStatus = None } as message ->
            match claimSession (sprintf "session-%s" message.SessionId) with
            | OtherSession otherServerId ->
                fprintfn Console.Error "Ignoring %s - owned by session %s on %s" id message.SessionId otherServerId
                None

            | OwnSession proc ->
                let message = { message with QueueStatus = Some "done" }
                let message = { message with Rev = putMessage id message }
                proc.StandardInput.WriteLine message.Message
                Some message

        | _ ->
            None

    let rec subscribe (lastSeq : int64 option) =
        let lastSeq, results = CouchDB.changes baseUri (Some "app/stdin") lastSeq
        for result in results do
            Option.iter (printfn "%A") (dequeue result.Id)

        subscribe (Some lastSeq)

    [<EntryPoint>]
    let main _ =
        try
            try
                subscribe None
            finally
                for id, (rev, proc) in Map.toSeq !ownSessions do
                    proc.Kill()
                    CouchDB.deleteDocument baseUri id rev
                    proc.WaitForExit()
                    proc.Dispose()

            0
        with ex ->
            fprintfn Console.Error "%O" ex
            1
