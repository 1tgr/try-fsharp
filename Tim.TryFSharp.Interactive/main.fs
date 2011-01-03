namespace Tim.TryFSharp.Interactive

open System.IO
open Microsoft.FSharp.Compiler.Interactive
open Newtonsoft.Json

module Main =
    let private toJson<'a> (a : 'a) : string =
        let s = JsonSerializer()
        use writer = new StringWriter()
        s.Serialize(writer, box a)
        string (writer.GetStringBuilder())

    let init (fsi : InteractiveSession) =
        fsi.AddPrinter toJson<Shape>
        fsi.AddPrinter (execute >> toJson)
