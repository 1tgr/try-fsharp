namespace Tim.TryFSharp.Test

open System
open System.IO
open System.Threading
open Tim.TryFSharp.Service
open Xunit

type ServiceTests() =

    [<Fact>]
    let ``Should start and stop fsi`` () =
        use proc = FsiProcess.Start()
        use fsiProc = new FsiProcess({ Name = Path.GetRandomFileName(); InitTexts = [| |]; Arguments = [| |]; Print = Console.WriteLine; Recycle = id }, proc)
        ()

    let interact substring fn =
        let lines = ResizeArray()
        use gotMessage = new ManualResetEvent(false)

        let print s =
            lines.Add(s)
            if (s : string).Contains(substring) then
                ignore (gotMessage.Set())

        let success =
            use proc = FsiProcess.Start()
            use fsiProc = new FsiProcess({ Name = Path.GetRandomFileName(); InitTexts = [| |]; Arguments = [| |]; Print = print; Recycle = id }, proc)
            fn fsiProc
            gotMessage.WaitOne(TimeSpan.FromSeconds(10.0))

        Assert.True(success, sprintf "Should have '%s' in: %A" substring (lines.ToArray()))

    [<Fact>]
    let ``Should interact with fsi`` () =
        interact "hello world" <| fun fsiProc ->
            fprintfn fsiProc.StandardInput "printfn \"hello world\";;"
        
    [<Fact>]
    let ``Standard input and output should use UTF-8`` () =
        interact "“£10”, he said" <| fun fsiProc ->
            fprintfn fsiProc.StandardInput "printfn \"\\u201C\\u00A310\\u201D, he said\";;"
