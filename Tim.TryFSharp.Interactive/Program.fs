namespace Tim.TryFSharp.Interactive

open System
open System.IO
open System.Reflection
open System.Threading
open Tim.TryFSharp.Core

module Program =

    type Type with
        member t.CheckedGetField(name : string, bf : BindingFlags) =
            match t.GetField(name, bf) with
            | null -> failwithf "No %s field on %O" name t
            | f -> f

        member t.CheckedGetProperty(name : string, bf : BindingFlags) =
            match t.GetProperty(name, bf) with
            | null -> failwithf "No %s property on %O" name t
            | p -> p

        member t.CheckedGetMethod(name : string, bf : BindingFlags) =
            match t.GetMethod(name, bf) with
            | null -> failwithf "No %s method on %O" name t
            | m -> m

        member t.GetFieldValue(inst : obj, name : string, bf : BindingFlags) =
            t.CheckedGetField(name, bf).GetValue(inst)

        member t.GetPropertyValue(inst : obj, name : string, bf : BindingFlags) =
            t.CheckedGetProperty(name, bf).GetValue(inst)

    let fsiAssembly =
        lazy
            let fsharpCore = typedefof<option<_>>.Assembly.GetName()
            let n = AssemblyName("fsi", Version = fsharpCore.Version, CultureInfo = fsharpCore.CultureInfo)
            n.SetPublicKeyToken(fsharpCore.GetPublicKeyToken())
            Assembly.Load(n)

    let fsiEvaluationSessionType =
        lazy fsiAssembly.Value.GetType("Microsoft.FSharp.Compiler.Interactive.Shell+FsiEvaluationSession", throwOnError = true)

    let fsiEvaluationSession(argv : string array, stdin : TextReader, stdout : TextWriter, stderr : TextWriter) =
        Activator.CreateInstance(fsiEvaluationSessionType.Value, [| box argv; box stdin; box stdout; box stderr |])

    [<EntryPoint>]
    let main _ =
        try
            let bf = BindingFlags.NonPublic ||| BindingFlags.Instance

            let () =
                let session = fsiEvaluationSession([| "fsi"; "--peekahead-" |], TextReader.Null, TextWriter.Null, TextWriter.Null)
                let fsiInteractionProcessor = fsiEvaluationSessionType.Value.GetFieldValue(session, "fsiInteractionProcessor", bf)
                let istateRef = fsiEvaluationSessionType.Value.GetFieldValue(session, "istateRef", bf)
                let istate = istateRef.GetType().GetPropertyValue(istateRef, "Value", BindingFlags.Public ||| BindingFlags.Instance)
                let m = fsiInteractionProcessor.GetType().CheckedGetMethod("LoadDummyInteraction", bf)
                ignore (m.Invoke(fsiInteractionProcessor, [| istate |]))

            let args = Json.parseString (stdin.ReadLine())
            let session = fsiEvaluationSession(Array.append [| "fsi" |] args, stdin, stdout, stderr)
            ignore (fsiEvaluationSessionType.Value.CheckedGetMethod("Run", bf).Invoke(session, [| |]))
            0
        with ex ->
            eprintfn "%O" ex
            1
