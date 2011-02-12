namespace Tim.TryFSharp.Core

open System

type ServiceConfig =
    {
        BaseUri : Uri
    }

type IService =
    inherit IDisposable
    abstract Start : ServiceConfig -> unit
    abstract SlowStop : unit -> unit
