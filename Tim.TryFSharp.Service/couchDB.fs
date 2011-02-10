namespace Tim.TryFSharp.Service

open System
open System.IO
open System.Net
open System.Text
open System.Threading
open System.Web
open Newtonsoft.Json

type DB =
    {
        [<JsonName("update_seq")>] UpdateSeq : int64
    }

type ChangeRev =
    {
        [<JsonName("rev")>] Rev : string
    }

type Change<'a> =
    {
        [<JsonName("seq")>]     Seq     : int64
        [<JsonName("id")>]      Id      : string
        [<JsonName("changes")>] Changes : ChangeRev array
        [<JsonName("doc")>]     Doc     : 'a option
    }

type ChangesResponse<'a> =
    {
        [<JsonName("results")>]  Results : Change<'a> array
        [<JsonName("last_seq")>] LastSeq : int64
    }

type SaveResponse =
    {
        [<JsonName("id")>]  Id  : string
        [<JsonName("rev")>] Rev : string
    }

module CouchDB =
    let skipResponse (request : WebRequest) : unit =
        use response = request.GetResponse()
        use stream = response.GetResponseStream()
        use textReader = new StreamReader(stream)
        ignore (textReader.ReadToEnd())

    let writeRequest (request : WebRequest) (doc : 'a) : unit =
        request.ContentType <- "application/json"

        using (request.GetRequestStream()) <| fun stream ->
            use textWriter = new StreamWriter(stream)
            use jsonWriter = new JsonTextWriter(textWriter)
            Json.write jsonWriter doc

    let parseResponse (request : WebRequest) : 'a =
        use response = request.GetResponse()
        use stream = response.GetResponseStream()
        use textReader = new StreamReader(stream)
        use jsonReader = new JsonTextReader(textReader)
        Json.parse jsonReader

    let parseResponseAsync (request : WebRequest) : Async<'a> =
        async {
            use! response = request.AsyncGetResponse()
            use stream = response.GetResponseStream()
            use textReader = new StreamReader(stream)
            use jsonReader = new JsonTextReader(textReader)
            return Json.parse jsonReader
        }

    let get (uri : Uri) : 'a =
        Log.info "get %O" uri

        let request = WebRequest.Create(uri)
        request.Method <- WebRequestMethods.Http.Get
        parseResponse request

    let getAsync (uri : Uri) : Async<'a> =
        Log.info "get %O" uri

        let request = WebRequest.Create(uri)
        request.Method <- WebRequestMethods.Http.Get
        parseResponseAsync request

    let put (uri : Uri) (doc : 'a) : 'b =
        Log.info "put %O" uri

        let request = WebRequest.Create(uri)
        request.Method <- WebRequestMethods.Http.Put
        writeRequest request doc
        parseResponse request

    let post (uri : Uri) (doc : 'a) : 'b =
        Log.info "post %O" uri

        let request = WebRequest.Create(uri)
        request.Method <- WebRequestMethods.Http.Post
        writeRequest request doc
        parseResponse request

    let delete (uri : Uri) : unit =
        Log.info "delete %O" uri

        let request = WebRequest.Create(uri)
        request.Method <- "DELETE"
        skipResponse request

    let conflicted (fn : 'a -> 'b) (a : 'a) : 'b option =
        try
            Some (fn a)
        with :? WebException as ex ->
            match ex.Response with
            | :? HttpWebResponse as response when response.StatusCode = HttpStatusCode.Conflict -> None
            | _ -> reraise ()

    let changes (baseUri : Uri) (filter : string option) (lastSeq : int64 option) : Async<int64 * Change<'a> array> =
        let lastSeq =
            match lastSeq with
            | Some n -> n
            | None ->
                let db : DB = get baseUri
                db.UpdateSeq
                
        let query =
            seq {
                yield "feed", "longpoll"
                yield "heartbeat", "10000"
                yield "include_docs", "true"
                yield "since", (string lastSeq)

                match filter with
                | Some filter -> yield "filter", filter
                | None -> ()
            }

        let builder = UriBuilder(Uri(baseUri, "_changes"))

        builder.Query <-
            (StringBuilder(), query)
            ||> Seq.fold (fun sb (name, value) ->
                let sb =
                    match sb.Length with
                    | 0 -> sb
                    | _ -> sb.Append("&")

                sb.AppendFormat("{0}={1}", HttpUtility.UrlEncode(name), HttpUtility.UrlEncode(value)))
            |> string

        let rec impl retries : Async<ChangesResponse<'a>> =
            async {
                try
                    return! getAsync builder.Uri
                with ex ->
                    if retries > 0 then
                        Thread.Sleep 1000
                        return! impl (retries - 1)
                    else
                        return raise ex 
            }

        async {
            let! response = impl 10
            return response.LastSeq, response.Results
        }

    let getDocument (baseUri : Uri) (id : string) : 'a =
        get (Uri(baseUri, id))

    let putDocument (baseUri : Uri) (id : string) : 'a -> SaveResponse =
        put (Uri(baseUri, id))

    let postDocument : Uri -> 'a -> SaveResponse =
        post

    let deleteDocument (baseUri : Uri) (id : string) (rev : string) : unit =
        let builder = UriBuilder(Uri(baseUri, id))
        builder.Query <- sprintf "rev=%s" rev
        delete builder.Uri
