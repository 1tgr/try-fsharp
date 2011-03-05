﻿namespace Tim.TryFSharp.Service

open System
open System.Net
open Tim.TryFSharp.Core

type Session =
    {
        [<JsonName("_rev")>]       Rev        : string option
        [<JsonName("type")>]       Type       : string
        [<JsonName("owner")>]      Owner      : string option
        [<JsonName("host")>]       Host       : string option
        [<JsonName("servicePid")>] ServicePid : int64 option
        [<JsonName("fsiPid")>]     FsiPid     : int64 option
        [<JsonName("initNames")>]  InitNames  : string array
        [<JsonName("initTexts")>]  InitTexts  : string array
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
    let getSession (baseUri : Uri) : string -> Session option =
        CouchDB.notFound (CouchDB.getDocument baseUri)

    let putSession : Uri -> string -> Session -> SaveResponse =
        CouchDB.putDocument

    let safePutSession (baseUri : Uri) (id : string) : Session -> SaveResponse option =
        CouchDB.conflicted (putSession baseUri id)

    let getMessage : Uri -> string -> Message =
        CouchDB.getDocument

    let putMessage : Uri -> string -> Message -> SaveResponse =
        CouchDB.putDocument
    
    let postMessage : Uri -> Message -> SaveResponse =
        CouchDB.postDocument
