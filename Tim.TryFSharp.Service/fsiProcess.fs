namespace Tim.TryFSharp.Service

open System
open System.Diagnostics
open System.Threading
open System.IO
open Tim.TryFSharp.Core

type FsiProcessInfo =
    {
        Name : string
        InitTexts : (string * string) array
        Print : string -> unit
        Recycle : unit -> unit
    }

type FsiProcess(info : FsiProcessInfo) =
    let path = sprintf "%s\\%s" (Path.GetTempPath()) info.Name

    let proc =
        let programFiles =
            match Environment.GetEnvironmentVariable("ProgramFiles(x86)") with
            | null -> Environment.GetFolderPath(Environment.SpecialFolder.ProgramFiles)
            | s -> s

        let filename = Path.Combine(programFiles, "Microsoft F#\\v4.0\\fsi.exe")

        let startInfo = new ProcessStartInfo()
        startInfo.FileName <-
            if File.Exists(filename) then
                filename
            else
                "fsi"

        startInfo.WorkingDirectory <- path
        startInfo.RedirectStandardError <- true
        startInfo.RedirectStandardInput <- true
        startInfo.RedirectStandardOutput <- true
        startInfo.UseShellExecute <- false

        let proc = new Process()
        proc.StartInfo <- startInfo
        proc

    let syncRoot = obj()

    [<DefaultValue>]
    val mutable timer : Timer option

    member this.ResetRecycleTimer () =
        let callback _ = info.Recycle ()

        lock syncRoot <| fun _ ->
            match this.timer with
            | Some timer -> timer.Dispose()
            | None -> ()

        this.timer <- Some (new Timer(TimerCallback(callback), null, TimeSpan.FromMinutes(10.0), TimeSpan.FromMilliseconds(-1.0)))

    member this.Start() =
        let post (args : DataReceivedEventArgs) =
            if args.Data <> null then
                this.ResetRecycleTimer()
                info.Print args.Data

        this.ResetRecycleTimer()
        proc.OutputDataReceived.Add post
        proc.ErrorDataReceived.Add post

        ignore (Directory.CreateDirectory path)
        for name, text in info.InitTexts do
            let filename = Path.Combine(path, sprintf "%s.fsx" name)
            File.WriteAllText(filename, text)

        ignore (proc.Start())
        proc.BeginErrorReadLine()
        proc.BeginOutputReadLine()

    member this.Process = proc

    interface IDisposable with
        member this.Dispose() =
            Log.info "Exiting fsi process %d" proc.Id
            proc.StandardInput.WriteLine("#quit;;")

            if not (proc.WaitForExit(5000)) then
                Log.info "Killing fsi process %d" proc.Id
                try
                    proc.Kill()
                    ignore (proc.WaitForExit(5000))
                with _ -> ()

            if proc.HasExited then
                Log.info "Fsi process %d exited with code %d" proc.Id proc.ExitCode
            else
                Log.info "Fsi process %d has not exited" proc.Id

            proc.Dispose()

            lock syncRoot <| fun _ ->
                match this.timer with
                | Some timer -> timer.Dispose()
                | None -> ()
