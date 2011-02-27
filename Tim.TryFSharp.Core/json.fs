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

type JsonWrapper<'json, 'fsharp> =
    {
        Pack : 'json -> 'fsharp
        Unpack : 'fsharp -> 'json
    }

type JsonSchema =
    | Array of JsonSchema * JsonWrapper<obj array, Array>
    | Dictionary of (string * JsonSchema) array * JsonWrapper<obj array, obj>
    | Map of JsonSchema * JsonWrapper<(string * obj) array, obj>
    | Scalar of Type * JsonWrapper<obj, obj>
    | Token

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module JsonSchema =
    type IJsonWrapper<'json, 'fsharp> =
        abstract Pack : 'json -> 'fsharp
        abstract Unpack : 'fsharp -> 'json

    type MapSerializer<'Value>() =
        interface IJsonWrapper<(string * obj) array, obj> with
            member this.Pack (fields : (string * obj) array) : obj =
                fields
                |> Map.ofArray
                |> Map.map (fun _ -> unbox<'Value>)
                |> box

            member this.Unpack (map : obj) : (string * obj) array =
                map
                |> unbox<Map<string, 'Value>>
                |> Map.map (fun _ -> box)
                |> Map.toArray

    let rec raw : Type -> JsonSchema =
        function
        | t when FSharpType.IsRecord t ->
            let wrapper : JsonWrapper<obj array, obj> =
                {
                    Pack = FSharpValue.PreComputeRecordConstructor t
                    Unpack = FSharpValue.PreComputeRecordReader t
                }

            let fields =
                [|
                    for field in FSharpType.GetRecordFields t ->
                        let name =
                            match field.GetCustomAttributes(typeof<JsonNameAttribute>, true) with
                            | [| :? JsonNameAttribute as attrib |] -> attrib.Name
                            | _ -> field.Name

                        name, raw field.PropertyType
                |]

            Dictionary(fields, wrapper)

        | t when t.IsArray ->
            let t = t.GetElementType()

            let wrapper : JsonWrapper<obj array, Array> =
                {
                    Pack = fun (values : obj array) ->
                        let array : Array = Array.CreateInstance(t, Array.length values)
                        Seq.iteri (fun i value -> array.SetValue(value, i)) values
                        array

                    Unpack = fun (array : Array) ->
                        Array.init array.Length (fun i -> array.GetValue(i))
                }

            Array(raw t, wrapper)

        | t when t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<Map<_, _>> ->
            let schema, serializerType = 
                match t.GetGenericArguments() with
                | [| k; v |] when k = typeof<string> ->
                    raw v, typedefof<MapSerializer<_>>.MakeGenericType([| v |])

                | _ ->
                    failwithf "Cannot serialize %s. Only string keys are supported." t.Name

            let serializer : IJsonWrapper<(string * obj) array, obj> = unbox (Activator.CreateInstance(serializerType, [| box schema |]))
            let wrapper : JsonWrapper<_, _> = { Pack = serializer.Pack; Unpack = serializer.Unpack }
            Map(schema, wrapper)

        | t when t = typeof<JToken> ->
            Token

        | t ->
            let wrapper : JsonWrapper<_, _> =
                {
                    Pack = fun (value : obj) -> Convert.ChangeType(value, t)
                    Unpack = id
                }

            Scalar(t, wrapper)

    let changeScalar (fn : Type -> JsonSchema option) : JsonSchema -> JsonSchema =
        let mem = System.Collections.Generic.Dictionary<Type, JsonSchema option>()

        let rec impl =
            function
            | Array(element, wrapper) -> Array(impl element, wrapper)
            | Dictionary(fields, wrapper) -> Dictionary(Array.map (fun (name, field) -> name, impl field) fields, wrapper)
            | Map(value, wrapper) -> Map(impl value, wrapper)
            | Token -> Token

            | Scalar(t, wrapper) as schema ->
                let opt =
                    match mem.TryGetValue t with
                    | (true, schema) ->
                        schema

                    | (false, _) ->
                        let schema = fn t
                        mem.[t] <- schema
                        schema

                defaultArg opt schema

        impl

    let withOption : JsonSchema -> JsonSchema =
        changeScalar <|
            function
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

                    let wrapper : JsonWrapper<obj, obj>=
                        {
                            Pack =
                                function
                                | null -> makeNull ()
                                | raw -> makeNotNull (Convert.ChangeType(raw, t))

                            Unpack = fun (union : obj) ->
                                match getTag union with
                                | tag when tag = nullCase.Tag -> null
                                | tag when tag = notNullCase.Tag -> getNotNull union
                                | tag -> failwithf "Didn't expect tag %d for F# union %s" tag t.Name
                        }

                    Some (Scalar(t, wrapper))

                | _ -> None
            | _ -> None

    let withUnion : JsonSchema -> JsonSchema =
        changeScalar <|
            function
            | t when FSharpType.IsUnion t ->
                let cases = 
                    [|
                        for c in FSharpType.GetUnionCases t ->
                            let schemas = [| for f in c.GetFields() -> raw f.PropertyType |]
                            let get = FSharpValue.PreComputeUnionReader c
                            let make = FSharpValue.PreComputeUnionConstructor c
                            c.Tag, (schemas, get, make)
                    |] |> Map.ofArray

                let getTag = FSharpValue.PreComputeUnionTagReader t

                let wrapper : JsonWrapper<obj array, obj> =
                    {
                        Pack =
                            function
                            | [| :? int as tag; :? JArray as tokens |] ->
                                let tokens : JToken array = Array.ofSeq tokens
                                let schemas, _, make = cases.[tag]
                                let values = (schemas, tokens) ||> Array.map2 (fun schema -> schema.Reader)
                                make values

                            | a -> failwithf "Expected [int, array], not %A" a

                        Unpack = fun (union : obj) ->
                            let tag = getTag union
                            let schemas, get, _ = cases.[tag]
                            let values = get union
                            let tokens = (schemas, values) ||> Array.map2 (fun schema -> schema.Writer)
                            [| tag; JArray(tokens) |]
                    }

                Some (Dictionary([| "Tag", raw typeof<int>; "Value", raw typeof<JToken>|], wrapper))
            | _ -> None

    let ofType : Type -> JsonSchema =
        raw >> withOption >> withUnion

module Json =
    type ISerializer =
        abstract Read : JToken -> obj
        abstract Write : obj -> JToken

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
