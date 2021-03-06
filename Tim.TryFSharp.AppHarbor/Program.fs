﻿namespace Tim.TryFSharp.AppHarbor

open System
open System.Configuration
open System.Threading
open Tim.TryFSharp.Core
open Tim.TryFSharp.Service

module Program =

    [<EntryPoint>]
    let main _ =
        try
            use exitEvent = new ManualResetEvent(false)

            AppDomain.CurrentDomain.UnhandledException.Add <| fun e ->
                eprintf "%O" e.ExceptionObject
                ignore (exitEvent.Set())

            let config : ServiceConfig =
                match ConfigurationManager.AppSettings.["database"] with
                | null | "" -> failwithf "Missing 'database' config setting. Got: %A" [| for k in ConfigurationManager.AppSettings.Keys -> k |]
                | s -> { BaseUri = Uri(s) }

            use state = ServiceState.Create(config)
            ignore (exitEvent.WaitOne())
        with ex ->
            eprintfn "%O" ex

        1
