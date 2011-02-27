namespace Tim.TryFSharp.Core

open System
open System.Globalization
open Microsoft.FSharp.Reflection
open Newtonsoft.Json
open Newtonsoft.Json.Linq

[<AttributeUsage(AttributeTargets.Property, AllowMultiple = false)>]
type JsonNameAttribute(name : string) =
    inherit Attribute()
    member this.Name = name

type ISerializer =
    abstract Read : JToken -> obj
    abstract Write : obj -> JToken

module Json =
    type MapSerializer<'Value>() =
        let valueReader, valueWriter = Serializer.Create typeof<'Value>

        interface ISerializer with
            member this.Read (raw : JToken) : obj =
                let dict : JObject = unbox raw
                let map : Map<string, 'Value> =
                    (Map.empty, dict.Properties())
                    ||> Seq.fold (fun map prop ->
                        let value : 'Value = unbox (valueReader prop.Value)
                        Map.add prop.Name value map)

                box map

            member this.Write (map : obj) : JToken =
                failwithf "not implemented %A" map

    and Serializer() =
        static member Create : Type -> (JToken -> obj) * (obj -> JToken) =
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

                            let reader, writer = Serializer.Create field.PropertyType
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
                        let token = writer value
                        if token <> null then
                            dict.Add(name, token))

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
                    let reader, writer = Serializer.Create t

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
                let reader, writer = Serializer.Create t

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

            | t when t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<Map<_, _>> ->
                let serializerType = 
                    match t.GetGenericArguments() with
                    | [| k; v |] when k = typeof<string> ->
                        typedefof<MapSerializer<_>>.MakeGenericType([| v |])

                    | _ ->
                        failwithf "Cannot serialize %s. Only string keys are supported." t.Name

                let serializer : ISerializer = unbox (Activator.CreateInstance(serializerType))
                serializer.Read, serializer.Write

            | t when t = typeof<DateTime> ->
                let reader (raw : JToken) : obj =
                    let value : JValue = unbox raw
                    let value : string = unbox value.Value
                    let date = DateTime.ParseExact(value, "s", CultureInfo.InvariantCulture)
                    box date

                let writer (value : obj) : JToken =
                    let date : DateTime = unbox value
                    let s = date.ToString("s", CultureInfo.InvariantCulture)
                    JValue(s) :> JToken

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
        let reader, _ = Serializer.Create typeof<'a>
        serializer.Deserialize >> unbox >> reader >> unbox

    let write<'a> : JsonWriter -> 'a -> unit =
        let serializer = JsonSerializer()
        let _, writer = Serializer.Create typeof<'a>
        fun jsonWriter doc ->
            let token = writer doc
            serializer.Serialize(jsonWriter, token)
