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

    let interact message =
        let lines = ResizeArray()
        use gotMessage = new ManualResetEvent(false)

        let print s =
            lines.Add(s)
            if (s : string).Contains(message) then
                ignore (gotMessage.Set())

        let success =
            use proc = FsiProcess.Start()
            use fsiProc = new FsiProcess({ Name = Path.GetRandomFileName(); InitTexts = [| |]; Arguments = [| |]; Print = print; Recycle = id }, proc)
            fsiProc.Process.StandardInput.WriteLine("printfn \"{0}\";;", message)
            gotMessage.WaitOne(TimeSpan.FromSeconds(30.0))

        Assert.True(success, sprintf "Should have '%s' in: %A" message (lines.ToArray()))

    [<Fact>]
    let ``Should interact with fsi`` () =
        interact "hello world"
        
    [<Fact>]
    let ``Standard input and output should use UTF-8`` () =
        interact "“£10”, he said"
