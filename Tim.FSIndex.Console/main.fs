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

type Method =
    {
        Name : string
    }

type Property =
    {
        Name : string
    }

type Type =
    {
        [<JsonName("_rev")>] Rev : string option
        Type : string
        AssemblyName : AssemblyName
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

    let typeId (assemblyName : AssemblyName) (nspace : string) (name : string) : string =
        [| assemblyName.Name; assemblyName.Version; assemblyName.PublicKeyToken; nspace; name |]
        |> String.concat ","
        |> Encoding.UTF8.GetBytes
        |> md5

    let types (assembly : AssemblyDefinition) : Type list =
        let assemblyName = assemblyName assembly

        [
            for modul in assembly.Modules do
                for typ in modul.Types do
                    if typ.IsPublic then
                        let methods : Method array =
                            [|
                                for methd in typ.Methods do
                                    let ignore =
                                        [|
                                            methd.IsRuntimeSpecialName
                                            methd.IsGetter
                                            methd.IsSetter
                                            methd.IsAddOn
                                            methd.IsRemoveOn
                                            methd.IsFire
                                            methd.IsPrivate
                                        |]

                                    if not (Array.exists id ignore) then
                                        yield { Name = methd.Name }
                            |]

                        let properties : Property array =
                            [| for property in typ.Properties -> { Name = property.Name } |]
                                
                        yield {
                            Rev = None
                            Type = "type"
                            AssemblyName = assemblyName
                            Namespace = typ.Namespace
                            Name = typ.Name
                            Methods = methods
                            Properties = properties
                        }
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
        let rec impl garbage typs =
            match typs with
            | [] -> garbage
            | typ :: typs ->
                let id = typeId typ.AssemblyName typ.Namespace typ.Name

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
        (0, Array.collect glob args)
        ||> Array.fold (fun code arg ->
            try
                index baseUri arg
                max code 0
            with ex ->
                Log.info "Failed to index %s. %O" arg ex
                max code 1)
