open System
open System.Collections.Generic
open System.IO
open System.Net
open System.Text
open System.Web
open Microsoft.FSharp.Reflection
open Newtonsoft.Json
open Newtonsoft.Json.Linq

[<AttributeUsage(AttributeTargets.Property, AllowMultiple = false)>]
type JsonNameAttribute(name : string) =
    inherit Attribute()
    member this.Name = name

module Json =
    let rec makeSerializer : Type -> (JToken -> obj) * (obj -> JToken) =
        function
        | t when FSharpType.IsRecord t ->
            let get = FSharpValue.PreComputeRecordReader t
            let make = FSharpValue.PreComputeRecordConstructor t

            let serializers =
                [|
                    for field in FSharpType.GetRecordFields t ->
                        let name =
                            match field.GetCustomAttributes(typeof<JsonNameAttribute>, true) with
                            | [| :? JsonNameAttribute as attrib |] -> attrib.Name
                            | _ -> field.Name

                        let reader, writer = makeSerializer field.PropertyType
                        name, reader, writer
                |]

            let reader (raw : JToken) : obj =
                let dict : JObject = unbox raw
                let values = Array.map (fun (name, reader, _) -> reader (dict.[name])) serializers
                make values

            let writer (record : obj) : JToken =
                let dict = JObject()
                (get record, serializers)
                ||> Array.iter2 (fun value (name, _, writer) ->
                    dict.Add(name, writer value))
                dict :> JToken

            reader, writer

        | t when FSharpType.IsUnion t ->
            let cases = FSharpType.GetUnionCases t
            let nullCase = cases |> Array.tryFind (fun c -> Array.isEmpty (c.GetFields()))
            let notNullCase = cases |> Array.tryPick (fun c ->
                match c.GetFields() with
                | [| f |] -> Some (c, f)
                | _ -> None)

            match nullCase, notNullCase with
            | Some nullCase, Some (notNullCase, field) ->
                let getTag = FSharpValue.PreComputeUnionTagReader t

                let makeNull =
                    let ctor = FSharpValue.PreComputeUnionConstructor nullCase
                    fun () -> ctor [| |]

                let makeNotNull =
                    let ctor = FSharpValue.PreComputeUnionConstructor notNullCase
                    fun value -> ctor [| value |]

                let getNotNull =
                    let reader = FSharpValue.PreComputeUnionReader notNullCase
                    fun union -> let values = reader union in values.[0]

                let t = field.PropertyType
                let reader, writer = makeSerializer t

                let reader : JToken -> obj =
                    function
                    | null -> makeNull ()
                    | raw -> raw |> reader |> makeNotNull

                let writer (union : obj) : JToken =
                    match getTag union with
                    | tag when tag = nullCase.Tag -> null
                    | tag when tag = notNullCase.Tag -> union |> getNotNull |> writer
                    | tag -> failwithf "Didn't expect tag %d for F# union %s" tag t.Name

                reader, writer

            | _ ->
                failwith "F# union %s cannot be serialized to JSON. Only unions with one null case and one non-null case are supported." t.Name

        | t when t.IsArray ->
            let t = t.GetElementType()
            let reader, writer = makeSerializer t

            let reader (raw : JToken) : obj =
                let array : JArray = unbox raw
                let values : Array = Array.CreateInstance(t, array.Count)

                let mutable i = 0
                for value in array do
                    values.SetValue(reader value, i)
                    i <- i + 1

                box values

            let writer (array : obj) : JToken =
                let values = Array.map writer (unbox array)
                JArray(values) :> JToken

            reader, writer

        | t when t = typeof<bool> || t = typeof<int> || t = typeof<int64> || t = typeof<string> ->
            let reader (raw : JToken) : obj =
                let value : JValue = unbox raw
                value.Value

            let writer (value : obj) : JToken =
                JValue(value) :> JToken

            reader, writer

        | t when t = typeof<JToken> ->
            box, unbox

        | t ->
            failwithf "F# type %s cannot be serialized to JSON" t.Name

    let parse<'a> : JsonReader -> 'a =
        let serializer = JsonSerializer()
        let reader, _ = makeSerializer typeof<'a>
        serializer.Deserialize >> unbox >> reader >> unbox

    let write<'a> : JsonWriter -> 'a -> unit =
        let serializer = JsonSerializer()
        let _, writer = makeSerializer typeof<'a>
        fun jsonWriter doc ->
            let token = writer doc
            serializer.Serialize(jsonWriter, token)

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

module CouchDB =
    type ChangesResponse =
        {
            [<JsonName("results")>]  Results : Change array
            [<JsonName("last_seq")>] LastSeq : int64
        }

    let get (uri : Uri) : 'a =
        fprintfn Console.Error ">> get %O" uri
        use client = new WebClient()
        let s = client.DownloadString(uri)
        use textReader = new StringReader(s)
        use jsonReader = new JsonTextReader(textReader)
        Json.parse jsonReader

    let put (uri : Uri) (doc : 'a) : unit =
        fprintfn Console.Error ">> put %O" uri

        let request = WebRequest.Create(uri)
        request.Method <- WebRequestMethods.Http.Put
        using (request.GetRequestStream()) <| fun stream ->
            use textWriter = new StreamWriter(stream)
            use jsonWriter = new JsonTextWriter(textWriter)
            Json.write jsonWriter doc

        use response = request.GetResponse()
        use stream = response.GetResponseStream()
        use textReader = new StreamReader(stream)
        ignore (textReader.ReadToEnd())

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

    let putDocument (baseUri : Uri) (id : string) (doc : 'a) : unit =
        put (Uri(baseUri, id)) doc

type Message =
    {
        [<JsonName("_rev")>]        Rev         : string
        [<JsonName("date")>]        Date        : string
        [<JsonName("messageType")>] MessageType : string
        [<JsonName("sessionId")>]   SessionId   : string
        [<JsonName("message")>]     Message     : string
        [<JsonName("queueStatus")>] QueueStatus : string option
    }

[<EntryPoint>]
let Main _ =
    try
        let baseUri = Uri("http://www.partario.com/couchdb/tryfs/")

        let rec subscribe (lastSeq : int64 option) =
            let lastSeq, results = CouchDB.changes baseUri (Some "app/stdin") lastSeq
            for result in results do
                let message =
                    match CouchDB.conflicted (CouchDB.getDocument baseUri) result.Id with
                    | Some ({ Message.QueueStatus = None } as message) ->
                        let message = { message with QueueStatus = Some "done" }
                        match CouchDB.conflicted (CouchDB.putDocument baseUri result.Id) message with
                        | Some () -> Some message
                        | None -> None

                    | Some { Message.QueueStatus = Some _ } | None -> None

                Option.iter (printfn "%A") message

            subscribe (Some lastSeq)

        subscribe None
        0
    with ex ->
        fprintfn Console.Error "%O" ex
        1
