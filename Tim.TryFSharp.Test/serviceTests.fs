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
        let lines = ResizeArray()
        using (new FsiProcess({ Name = Path.GetRandomFileName(); InitTexts = [| |]; Arguments = [| |]; Print = lines.Add; Recycle = id })) <| fun fsi ->
            fsi.Process.StandardInput.WriteLine("printfn \"hello world\";;")

        Assert.Contains("hello world", lines)
