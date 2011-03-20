namespace Tim.FSIndex.Console

open System
open System.IO
open System.Security.Cryptography
open System.Text
open Mono.Cecil
open Tim.TryFSharp.Core

type AssemblyName =
    {
        Name : string
        Version : string
        PublicKeyToken : string
    }

type TypeName =
    {
        Assembly : AssemblyName option
        Namespace : string option
        Name : string
    }

type Method =
    {
        Name : string
        Prototype : TypeName array
    }

type Property =
    {
        Name : string
        Type : TypeName
    }

type Type =
    {
        [<JsonName("_rev")>] Rev : string option
        Type : string
        Assembly : AssemblyName
        Namespace : string
        Name : string
        Methods : Method array
        Properties : Property array
    }

type TypeKey =
    {
        [<JsonName("_rev")>] Rev : string option
        Namespace : string
        Name : string
    }

module Program =
    let hex (data : byte array) : string =
        (StringBuilder(), data)
        ||> Array.fold (fun sb b -> sb.Append(b.ToString("x2")))
        |> string

    let md5 (data : byte array) : string =
        use md5 = MD5.Create()
        hex (md5.ComputeHash(data))

    let assemblyName (assembly : AssemblyDefinition) : AssemblyName =
        {
            Name = assembly.Name.Name
            Version = string assembly.Name.Version
            PublicKeyToken = hex assembly.Name.PublicKeyToken
        }

    let sysAssemblyName (assembly : System.Reflection.Assembly) : AssemblyName =
        let assemblyName = assembly.GetName()
        
        {
            Name = assemblyName.Name
            Version = string assemblyName.Version
            PublicKeyToken = hex (assemblyName.GetPublicKeyToken())
        }

    let typeId (assembly : AssemblyName) (nspace : string) (name : string) : string =
        [| assembly.Name; assembly.Version; assembly.PublicKeyToken; nspace; name |]
        |> String.concat ","
        |> Encoding.UTF8.GetBytes
        |> md5

    let resolve (typ : TypeReference) : TypeDefinition =
        match typ.Resolve() with
        | null -> failwithf "Unable to resolve %s %s" (typ.GetType().Name) typ.FullName
        | t -> t

    let decodeName<'a when 'a :> ICustomAttributeProvider and 'a :> IMemberDefinition> (mem : 'a) : string =
        let name =
            match mem.CustomAttributes |> Seq.tryFind (fun a -> a.AttributeType.FullName = "Microsoft.FSharp.Core.CompilationRepresentationAttribute") with
            | Some cra ->
                let flags : CompilationRepresentationFlags = unbox cra.ConstructorArguments.[0].Value
                match int (flags &&& CompilationRepresentationFlags.ModuleSuffix) with
                | 0 -> None
                | _ -> Some (mem.Name.Substring(0, mem.Name.Length - "Module".Length))
            
            | None ->
                None

        let name =
            match name, mem.CustomAttributes |> Seq.tryFind (fun a -> a.AttributeType.FullName = "Microsoft.FSharp.Core.CompilationSourceNameAttribute") with
            | None, Some csna ->
                Some (unbox csna.ConstructorArguments.[0].Value)

            | opt, _ ->
                opt

        defaultArg name mem.Name

    let rec typeRefName (g : int) (typeRef : TypeReference) : int * TypeName =
        match typeRef with
        | :? ArrayType as at ->
            let g, elementType = typeRefName g at.ElementType
            g, {
                TypeName.Assembly = None
                Namespace = None
                Name = elementType.Name + " array"
            }

        | :? ByReferenceType as brt ->
            let g, elementType = typeRefName g brt.ElementType
            g, {
                TypeName.Assembly = None
                Namespace = None
                Name = sprintf "byref<%s>" elementType.Name
            }

        | :? GenericParameter as gp ->
            g + 1, {
                TypeName.Assembly = None
                Namespace = None
                Name = sprintf "'%c" (char ((int 'a') + g))
            }

        | _ ->
            let typ = resolve typeRef
            g, {
                TypeName.Assembly = Some (assemblyName typ.Module.Assembly)
                Namespace = Some typ.Namespace
                Name = decodeName typ
            }

    let prototype (typeRefs : TypeReference array) : TypeName array =
        let _, typeNames =
            let folder (g, typeNames) typeRef =
                let g, typeName = typeRefName g typeRef
                g, typeName :: typeNames

            Array.fold folder (0, []) typeRefs

        typeNames
        |> List.rev
        |> Array.ofList

    let methd (methd : MethodDefinition) : Method =
        let prototype =
            prototype [|
                for parm in methd.Parameters do
                    yield parm.ParameterType

                yield methd.ReturnType
            |]

        let prototype =
            if methd.Parameters.Count = 0 then
                let unit = typeof<unit>
                let unit : TypeName =
                    {
                        Assembly = Some (sysAssemblyName unit.Assembly)
                        Namespace = Some unit.Namespace
                        Name = unit.Name
                    }

                Array.append [| unit |] prototype
            else
                prototype

        {
            Name = decodeName methd
            Prototype = prototype                
        }

    let property (property : PropertyDefinition) : Property =
        {
            Name = decodeName property
            Type = snd (typeRefName 0 property.PropertyType)
        }

    let typ (typ : TypeDefinition) : Type =
        let methods : Method array =
            [|
                for m in typ.Methods do
                    let ignore =
                        [|
                            m.IsRuntimeSpecialName
                            m.IsAddOn
                            m.IsRemoveOn
                            m.IsFire
                            m.IsPrivate
                        |]

                    if not (Array.exists id ignore) then
                        yield methd m
            |]

        let properties : Property array =
            [| for p in typ.Properties -> property p |]
                                
        {
            Rev = None
            Type = "type"
            Assembly = assemblyName typ.Module.Assembly
            Namespace = typ.Namespace
            Name = typ.Name
            Methods = methods
            Properties = properties
        }

    let types (assembly : AssemblyDefinition) : Type list =
        [
            for modul in assembly.Modules do
                for t in modul.Types do
                    if t.IsPublic then
                        yield typ t
        ]

    let rec changeDocument (baseUri : Uri) (id : string) (fn : 'a option -> 'a) : unit =
        let oldDoc : 'a option = CouchDB.notFound (CouchDB.getDocument baseUri) id
        let newDoc = fn oldDoc

        let changed =
            match oldDoc with
            | Some oldDoc -> oldDoc <> newDoc
            | None -> true

        if changed then
            if Option.isNone (CouchDB.conflicted (CouchDB.putDocument baseUri id) newDoc) then
                changeDocument baseUri id fn

    let index baseUri (filename : string) =
        let rec impl garbage (typs : Type list) =
            match typs with
            | [] -> garbage
            | typ :: typs ->
                let id = typeId typ.Assembly typ.Namespace typ.Name

                changeDocument baseUri id <| function
                    | Some t -> { typ with Rev = t.Rev }
                    | None -> typ

                let garbage = Map.remove (typ.Namespace, typ.Name) garbage
                impl garbage typs

        let assembly = AssemblyDefinition.ReadAssembly filename
        let typs = types assembly
        let assemblyName = assemblyName assembly

        let garbage =
            let view : (string * TypeKey) array = CouchDB.viewByKey baseUri "app/by-assembly" assemblyName
            Map.ofArray [| for id, typeKey in view -> (typeKey.Namespace, typeKey.Name), (id, typeKey.Rev) |]

        let garbage = impl garbage typs

        for _, (id, rev) in Map.toSeq garbage do
            let rev =
                match rev with
                | Some rev -> Some rev
                | None ->
                    let typ : Type option = CouchDB.notFound (CouchDB.getDocument baseUri) id
                    match typ with
                    | Some typ -> typ.Rev
                    | None -> None

            match rev with
            | Some rev -> CouchDB.deleteDocument baseUri id rev
            | None -> ()

    let glob pattern =
        let dir, wildcard =
            match Path.GetDirectoryName(pattern) with
            | "" -> ".", pattern
            | dir -> dir, Path.GetFileName(pattern)

        Directory.GetFiles(dir, wildcard)

    [<EntryPoint>]
    let Main args =
        let baseUri = Uri("http://veneto.partario.com:5984/fsindex/")

        let indexAsync filename =
            async {
                try
                    index baseUri filename
                    return 0
                with ex ->
                    Log.info "Failed to index %s. %O" filename ex
                    return 1
            }

        let codes =
            args
            |> Array.collect glob
            |> Array.map indexAsync
            |> Async.Parallel
            |> Async.RunSynchronously

        Array.max codes
