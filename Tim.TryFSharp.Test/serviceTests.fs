namespace Tim.TryFSharp.Test

open System
open System.IO
open System.Threading
open Tim.TryFSharp.Service
open Xunit

type ServiceTests() =

    [<Fact>]
    let ``Should start and stop fsi`` () =
        use proc = new FsiProcess({ Name = Path.GetRandomFileName(); InitTexts = [| |]; Arguments = [| |]; Print = Console.WriteLine; Recycle = id })
        ()

    [<Fact>]
    let ``Should interact with fsi`` () =
        let message = "hello world"
        let lines = ResizeArray()
        use gotMessage = new ManualResetEvent(false)

        let print s =
            lines.Add(s)
            if s = message then
                ignore (gotMessage.Set())

        let success =
            using (new FsiProcess({ Name = Path.GetRandomFileName(); InitTexts = [| |]; Arguments = [| |]; Print = print; Recycle = id })) <| fun fsi ->
                fsi.Process.StandardInput.WriteLine("printfn \"{0}\";;", message)
                gotMessage.WaitOne(TimeSpan.FromSeconds(30.0))

        Assert.True(success, sprintf "Should have '%s' in: %A" message (lines.ToArray()))
