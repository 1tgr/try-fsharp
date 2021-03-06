﻿namespace Tim.TryFSharp.Service

open System
open System.Threading

module Timer =
    let timer (callback : unit -> unit) =
        new Timer(TimerCallback(fun _ -> callback ()))

    let at (dueTime : TimeSpan) (timer : Timer) =
        ignore (timer.Change(dueTime = dueTime, period = TimeSpan.FromMilliseconds(-1.0)))
        timer

    let every (period : TimeSpan) (timer : Timer) =
        ignore (timer.Change(dueTime = TimeSpan.FromTicks(0L), period = period))
        timer

    let never (timer : Timer) =
        ignore (timer.Change(dueTime = Timeout.Infinite, period = Timeout.Infinite))
        timer
