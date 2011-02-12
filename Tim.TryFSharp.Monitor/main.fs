namespace Tim.TryFSharp.Monitor

open System
open System.Threading
open System.Reflection
open System.Runtime.InteropServices
open Mono.Unix
open Mono.Unix.Native
open Tim.TryFSharp.Core

type ICtrlCHandler =
    inherit IDisposable
    abstract WaitHandle : WaitHandle

type MonoCtrlCHandler() =
    let signal = new UnixSignal(Signum.SIGINT)
    interface ICtrlCHandler with
        member this.WaitHandle = signal :> WaitHandle
        member this.Dispose() = signal.Dispose()

type ConsoleCtrlDelegate = delegate of int -> bool

type WindowsCtrlCHandler() =
    [<DllImport("kernel32.dll")>]
    static extern bool SetConsoleCtrlHandler(ConsoleCtrlDelegate handlerRoutine, bool add)

    let event = new ManualResetEvent(false)
    let handler = ConsoleCtrlDelegate(fun _ -> ignore (event.Set()); true)
    do ignore (SetConsoleCtrlHandler(handler, true))

    interface ICtrlCHandler with
        member this.WaitHandle = event :> WaitHandle
        member this.Dispose() = ignore (SetConsoleCtrlHandler(handler, false))

module Main =
    let isMono =
        match Type.GetType ("Mono.Runtime") with
        | null -> false
        | _ -> true

    [<EntryPoint>]
    let main args =
        try
            let baseUri =
                match args with
                [| uri |] -> Uri(uri)
                | _ -> Uri("http://tryfs.net/tryfs/")

            use ctrlCHandler =
                if isMono then
                    new MonoCtrlCHandler() :> ICtrlCHandler
                else
                    new WindowsCtrlCHandler() :> ICtrlCHandler

            let config : ServiceConfig = { BaseUri = baseUri }
            let s =
                let a = Assembly.LoadFrom(string (Uri(baseUri, "_design/Tim.TryFSharp.Service/Tim.TryFSharp.Service.dll")))
                [| for t in a.GetTypes() do
                    if not t.IsAbstract && typeof<IService>.IsAssignableFrom(t) then
                        let s : IService = unbox (Activator.CreateInstance(t))
                        s.Start config
                        yield s |]

            try
                ignore (ctrlCHandler.WaitHandle.WaitOne())
            finally
                for s in s do
                    s.Dispose()

            0
        with ex ->
            Log.info "%O" ex
            1
