namespace Tim.TryFSharp.Core

open System

module Log =
    let info format =
        Printf.ksprintf (fun s -> Console.WriteLine("[{0:o}] {1}", DateTime.Now, s)) format
