namespace Tim.TryFSharp.Core

open System

module Log =
    let info format =
        Printf.ksprintf (fun s -> Console.WriteLine("[{0:o} {1}] {2}", DateTime.Now, AppDomain.CurrentDomain.FriendlyName, s)) format
