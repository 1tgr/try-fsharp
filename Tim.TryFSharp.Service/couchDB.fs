namespace Tim.TryFSharp.Service

open System
open System.IO
open System.Net
open System.Text
open System.Web
open Newtonsoft.Json

type ChangeRev =
    {
        [<JsonName("rev")>] Rev : string
    }

type Change =
    {
        [<JsonName("seq")>]     Seq     : int64
        [<JsonName("id")>]      Id      : string
        [<JsonName("changes")>] Changes : ChangeRev array
    }

type ChangesResponse =
    {
        [<JsonName("results")>]  Results : Change array
        [<JsonName("last_seq")>] LastSeq : int64
    }

module CouchDB =
    let skipResponse (request : WebRequest) : unit =
        use response = request.GetResponse()
        use stream = response.GetResponseStream()
        use textReader = new StreamReader(stream)
        ignore (textReader.ReadToEnd())

    let parseResponse (request : WebRequest) : 'a =
        use response = request.GetResponse()
        use stream = response.GetResponseStream()
        use textReader = new StreamReader(stream)
        use jsonReader = new JsonTextReader(textReader)
        Json.parse jsonReader

    let get (uri : Uri) : 'a =
        fprintfn Console.Error ">> get %O" uri

        let request = WebRequest.Create(uri)
        request.Method <- WebRequestMethods.Http.Get
        parseResponse request

    let put (uri : Uri) (doc : 'a) : 'b =
        fprintfn Console.Error ">> put %O" uri

        let request = WebRequest.Create(uri)
        request.Method <- WebRequestMethods.Http.Put
        using (request.GetRequestStream()) <| fun stream ->
            use textWriter = new StreamWriter(stream)
            use jsonWriter = new JsonTextWriter(textWriter)
            Json.write jsonWriter doc

        parseResponse request

    let delete (uri : Uri) : unit =
        fprintfn Console.Error ">> delete %O" uri

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

    let changes (baseUri : Uri) (filter : string option) (lastSeq : int64 option) : int64 * Change array =
        let query =
            seq {
                yield "feed", "longpoll"

                match filter with
                | Some filter -> yield "filter", filter
                | None -> ()

                match lastSeq with
                | Some lastSeq -> yield "since", (string lastSeq)
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

        let response : ChangesResponse = get builder.Uri
        response.LastSeq, response.Results

    let getDocument (baseUri : Uri) (id : string) : 'a =
        get (Uri(baseUri, id))

    let putDocument (baseUri : Uri) (id : string) (doc : 'a) : string =
        let response : ChangeRev = put (Uri(baseUri, id)) doc
        response.Rev

    let deleteDocument (baseUri : Uri) (id : string) (rev : string) : unit =
        let builder = UriBuilder(Uri(baseUri, id))
        builder.Query <- sprintf "rev=%s" rev
        delete builder.Uri
