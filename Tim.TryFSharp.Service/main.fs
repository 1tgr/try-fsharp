namespace Tim.TryFSharp.Service

open System
open System.Diagnostics
open System.Net

type Session =
    {
        [<JsonName("_rev")>]       Rev        : string option
        [<JsonName("owner")>]      Owner      : string
        [<JsonName("host")>]       Host       : string
        [<JsonName("servicePid")>] ServicePid : int
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

module Main =
    let baseUri = Uri("http://www.partario.com/couchdb/tryfs/")
    let ownSessions : Map<string, string> ref = ref Map.empty

    let getSession (id : string) : Session option =
        try
            Some (CouchDB.getDocument baseUri id)
        with :? WebException as ex ->
            match ex.Response with
            | :? HttpWebResponse as response when response.StatusCode = HttpStatusCode.NotFound -> None
            | _ -> reraise ()

    let putSession id : Session -> string option =
        CouchDB.conflicted (CouchDB.putDocument baseUri id)

    let getMessage : string -> Message =
        CouchDB.getDocument baseUri

    let putMessage id : Message -> string option =
        CouchDB.conflicted (CouchDB.putDocument baseUri id)
    
    let claimSession : string -> string option =
        let ownServerId = string (Guid.NewGuid())

        let rec impl id =
            if Map.containsKey id !ownSessions then
                None
            else
                match getSession id with
                | Some session when session.Owner = ownServerId ->
                    ownSessions := Map.add id (Option.get session.Rev) !ownSessions
                    None

                | Some session ->
                    Some session.Owner

                | None ->
                    let session : Session =
                        { 
                            Rev = None
                            Owner = ownServerId
                            Host = Dns.GetHostName()
                            ServicePid = Process.GetCurrentProcess().Id
                        }

                    match putSession id session with
                    | Some rev ->
                        ownSessions := Map.add id rev !ownSessions
                        None

                    | None ->
                        impl id

        impl

    let dequeue (id : string) : Message option =
        match getMessage id with
        | { Message.QueueStatus = None } as message ->
            match claimSession (sprintf "session-%s" message.SessionId) with
            | Some otherServerId ->
                fprintfn Console.Error "Ignoring %s - owned by session %s on %s" id message.SessionId otherServerId
                None

            | None ->
                let message = { message with QueueStatus = Some "done" }
                match putMessage id message with
                | Some rev ->
                    Some { message with Rev = rev }

                | None ->
                    fprintfn Console.Error "Ignoring %s - put conflict" id
                    None

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
                for id, rev in Map.toSeq !ownSessions do
                    CouchDB.deleteDocument baseUri id rev

            0
        with ex ->
            fprintfn Console.Error "%O" ex
            1
