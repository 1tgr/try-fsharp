open System
open System.Collections.Generic
open System.IO
open System.Net
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

        | t when t = typeof<string> || t = typeof<int> || t = typeof<int64> ->
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

module CouchDB =
    type ChangeRev =
        {
            [<JsonName("rev")>] Rev : string
        }

    type Change =
        {
            [<JsonName("seq")>]     Seq : int64
            [<JsonName("id")>]      Id : string
            [<JsonName("changes")>] Changes : ChangeRev array
        }

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

    let changes (baseUri : Uri) (lastSeq : int64 option) : int64 * Change array =
        let builder = UriBuilder(Uri(baseUri, "_changes"))

        builder.Query <-
            match lastSeq with
            | Some lastSeq -> sprintf "feed=longpoll&since=%d" lastSeq
            | None -> "feed=longpoll"

        let response : ChangesResponse = get builder.Uri
        response.LastSeq, response.Results

    let getDocument (baseUri : Uri) (id : string) : 'a =
        get (Uri(baseUri, id))

[<EntryPoint>]
let Main _ =
    try
        let baseUri = Uri("http://www.partario.com/couchdb/tryfs/")

        let rec subscribe (lastSeq : int64 option) =
            let lastSeq, results = CouchDB.changes baseUri lastSeq
            for result in results do
                let t : JToken = CouchDB.getDocument baseUri result.Id
                printfn "%O" t

            subscribe (Some lastSeq)

        subscribe None
        0
    with ex ->
        fprintfn Console.Error "%O" ex
        1
