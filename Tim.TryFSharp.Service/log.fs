namespace Tim.TryFSharp.Service

open System

module Log =
    let info format =
        Printf.ksprintf (fun s -> Console.WriteLine("[{0:o}] {1}", DateTime.Now, s)) format
