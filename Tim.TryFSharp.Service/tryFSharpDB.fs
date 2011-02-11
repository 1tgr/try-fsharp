namespace Tim.TryFSharp.Service

open System
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
