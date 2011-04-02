namespace Tim.TryFSharp.Core

open System
open System.Collections.Generic
open System.Globalization
open System.IO
open System.Text
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

type DateTimeSerializer() =
    static member Instance = DateTimeSerializer()

    interface ISerializer with
        member this.Read (raw : JToken) : obj =
            let value : JValue = unbox raw
            let value : string = unbox value.Value
            let date = DateTime.ParseExact(value, "s", CultureInfo.InvariantCulture)
            box date

        member this.Write (value : obj) : JToken =
            let date : DateTime = unbox value
            let s = date.ToString("s", CultureInfo.InvariantCulture)
            JValue(s) :> JToken

type ValueSerializer(t : Type) =
    interface ISerializer with
        member this.Read (raw : JToken) =
            match raw with
            | null -> failwithf "Expected %s, not null" t.Name
            | :? JValue as value ->
                match value.Value with
                | null -> failwithf "Expected %s, not null" t.Name
                | value when value.GetType() <> t -> failwithf "Expected %s, not %O" t.Name value
                | value -> value
            | token -> failwithf "Expected %s, not %O" t.Name token

        member this.Write (value : obj) : JToken =
            JValue(value) :> JToken

type RecordSerializer(t : Type) =
    let get = FSharpValue.PreComputeRecordReader t
    let make = FSharpValue.PreComputeRecordConstructor t

    let serializers =
        [|
            for field in FSharpType.GetRecordFields t ->
                let name =
                    match field.GetCustomAttributes(typeof<JsonNameAttribute>, true) with
                    | [| :? JsonNameAttribute as attrib |] -> attrib.Name
                    | _ -> field.Name.Substring(0, 1).ToLowerInvariant() + field.Name.Substring(1)

                name, Serializer.Create field.PropertyType
        |]

    interface ISerializer with
        member this.Read (raw : JToken) : obj =
            match raw with
            | null -> failwith "Expected object, not null"
            | :? JObject as dict ->
                let values =
                    [|
                        for name, serializer in serializers ->
                            try
                                Serializer.Read serializer (dict.[name])
                            with ex ->
                                raise (InvalidOperationException(sprintf "%s: %s" name ex.Message, ex))
                    |]

                make values
            | token -> failwithf "Expected object, not %O" token

        member this.Write (record : obj) : JToken =
            let dict = JObject()

            (get record, serializers)
            ||> Array.iter2 (fun value (name, serializer) ->
                let token = Serializer.Write serializer value
                if token <> null then
                    dict.Add(name, token))

            dict :> JToken

and UnionSerializer(t : Type) =
    let nullCase, notNullCase, field =
        let cases = FSharpType.GetUnionCases t
        let nullCase = cases |> Array.tryFind (fun c -> Array.isEmpty (c.GetFields()))
        let notNullCase = cases |> Array.tryPick (fun c ->
            match c.GetFields() with
            | [| f |] -> Some (c, f)
            | _ -> None)

        match nullCase, notNullCase with
        | Some nullCase, Some (notNullCase, field) ->
            nullCase, notNullCase, field

        | _ ->
            failwithf "F# union %s cannot be serialized to JSON. Only unions with one null case and one non-null case are supported." t.Name

    let makeNull =
        let ctor = FSharpValue.PreComputeUnionConstructor nullCase
        fun () -> ctor [| |]

    let makeNotNull =
        let ctor = FSharpValue.PreComputeUnionConstructor notNullCase
        fun value -> ctor [| value |]

    let getNotNull =
        let reader = FSharpValue.PreComputeUnionReader notNullCase
        fun union -> let values = reader union in values.[0]

    let serializer = Serializer.Create field.PropertyType
    let getTag = FSharpValue.PreComputeUnionTagReader t

    interface ISerializer with
        member this.Read (raw : JToken) : obj =
            match raw with
            | null -> makeNull ()
            | :? JValue as value when value.Value = null -> makeNull ()
            | raw -> raw |> Serializer.Read serializer |> makeNotNull

        member this.Write (union : obj) : JToken =
            match getTag union with
            | tag when tag = nullCase.Tag -> null
            | tag when tag = notNullCase.Tag -> union |> getNotNull |> Serializer.Write serializer
            | tag -> failwithf "Didn't expect tag %d for F# union %s" tag t.Name

and ArraySerializer(t : Type) =
    let t = t.GetElementType()
    let serializer = Serializer.Create t

    interface ISerializer with
        member this.Read (raw : JToken) : obj =
            match raw with
            | null -> box (Array.CreateInstance(t, 0))
            | :? JArray as array ->
                let values : Array = Array.CreateInstance(t, array.Count)

                let mutable i = 0
                for value in array do
                    values.SetValue(Serializer.Read serializer value, i)
                    i <- i + 1

                box values

            | a -> failwithf "expected array, got %A" a

        member this.Write (array : obj) : JToken =
            let values = Array.map (Serializer.Write serializer) (unbox array)
            JArray(values) :> JToken

and MapSerializer<'Value>() =
    let serializer = Serializer.Create typeof<'Value>

    interface ISerializer with
        member this.Read (raw : JToken) : obj =
            let dict : JObject = unbox raw
            let map : Map<string, 'Value> =
                (Map.empty, dict.Properties())
                ||> Seq.fold (fun map prop ->
                    let value : 'Value = unbox (Serializer.Read serializer prop.Value)
                    Map.add prop.Name value map)

            box map

        member this.Write (map : obj) : JToken =
            failwithf "not implemented %A" map

and Serializer() =
    static member Create : Type -> Lazy<ISerializer> =
        let impl =
            function
            | t when FSharpType.IsRecord t ->
                lazy (RecordSerializer(t) :> ISerializer)

            | t when FSharpType.IsUnion t ->
                lazy (UnionSerializer(t) :> ISerializer)

            | t when t.IsArray ->
                lazy (ArraySerializer(t) :> ISerializer)

            | t when t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<Map<_, _>> ->
                let serializerType = 
                    match t.GetGenericArguments() with
                    | [| k; v |] when k = typeof<string> ->
                        typedefof<MapSerializer<_>>.MakeGenericType([| v |])

                    | _ ->
                        failwithf "Cannot serialize %s. Only string keys are supported." t.Name

                lazy (unbox (Activator.CreateInstance(serializerType)))

            | t when t = typeof<DateTime> ->
                let s = DateTimeSerializer.Instance :> ISerializer
                lazy s

            | t when t = typeof<bool> || t = typeof<int> || t = typeof<int64> || t = typeof<string> ->
                let s = ValueSerializer(t) :> ISerializer
                lazy s

            | t when t = typeof<JToken> ->
                lazy { new ISerializer with
                    member this.Read token = box token
                    member this.Write token = unbox token }

            | t ->
                failwithf "F# type %s cannot be serialized to JSON" t.Name

        let syncRoot = obj()
        let dict = Dictionary<_, _>()
        fun t ->
            let opt =
                lock syncRoot <| fun () ->
                    match dict.TryGetValue(t) with
                    | true, l -> Some l
                    | false, _ -> None

            match opt with
            | Some l -> l
            | None ->
                let l = impl t
                lock syncRoot <| fun () ->
                    dict.[t] <- l
                l

    static member Read (l : Lazy<ISerializer>) token =
        l.Value.Read token

    static member Write (l : Lazy<ISerializer>) value =
        l.Value.Write value

module Json =
    let parse<'a> : JsonReader -> 'a =
        let serializer = JsonSerializer()
        let reader = Serializer.Create typeof<'a>
        serializer.Deserialize >> unbox >> reader.Value.Read >> unbox

    let write<'a> : JsonWriter -> 'a -> unit =
        let serializer = JsonSerializer()
        let writer = Serializer.Create typeof<'a>
        fun jsonWriter doc ->
            let token = writer.Value.Write doc
            serializer.Serialize(jsonWriter, token)

    let parseString (s : string) : 'a =
        use sr = new StringReader(s)
        use jr = new JsonTextReader(sr)
        parse jr

    let writeString (value : 'a) : string =
        let sb = StringBuilder()
        using (new StringWriter(sb)) <| fun sw ->
            use jw = new JsonTextWriter(sw)
            write jw value

        string sb
