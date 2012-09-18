namespace Tim.TryFSharp.Service

open System
open System.Diagnostics
open System.Text
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
    let path = Path.Combine(Path.GetTempPath(), info.Name)

    let initTexts =
        let chars = Path.GetInvalidFileNameChars()

        [| for name, text in info.InitTexts ->
            let name =
                (name, chars)
                ||> Array.fold (fun name char -> name.Replace(char, '_'))

            Path.ChangeExtension(name, ".fsx"), text |]

    let proc =
        let startInfo =
            new ProcessStartInfo(
                WorkingDirectory = path,
                RedirectStandardError = true,
                RedirectStandardInput = true,
                RedirectStandardOutput = true,
                UseShellExecute = false)

        let fileName = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, "fsi.exe")

        let fileName =
            if File.Exists(fileName) then
                fileName
            else
                let programFiles =
                    match Environment.GetEnvironmentVariable("ProgramFiles(x86)") with
                    | null -> Environment.GetFolderPath(Environment.SpecialFolder.ProgramFiles)
                    | s -> s

                Path.Combine(programFiles, "Microsoft F#\\v4.0\\fsi.exe")

        let fileName, arguments =
            match Type.GetType("Mono.Runtime") with
            | null -> fileName, ""
            | _ -> "mono", fileName

        let arguments =
            (StringBuilder(arguments), initTexts)
            ||> Array.fold (fun sb (name, _) ->
                let sb =
                    if sb.Length > 0 then
                        sb.Append(" ")
                    else
                        sb

                Printf.bprintf sb "--use:%s" name
                sb)
            |> string

        startInfo.FileName <- fileName
        startInfo.Arguments <- arguments

        let proc = new Process()
        proc.StartInfo <- startInfo
        proc

    let syncRoot = obj()
    let buffer = ResizeArray<string>()

    let flush () =
        let s = lock syncRoot <| fun _ ->
            let s = String.concat "\n" buffer
            buffer.Clear()
            s

        info.Print s

    let recycleTimer = Timer.timer info.Recycle
    let flushTimer = Timer.timer flush

    let resetRecycleTimer () =
        ignore (Timer.at (TimeSpan.FromMinutes(10.0)) recycleTimer)

    let post (args : DataReceivedEventArgs) =
        if args.Data <> null then
            let immediateFlush =
                lock syncRoot <| fun _ ->
                    buffer.Add(args.Data)
                    buffer.Count > 100

            resetRecycleTimer ()
            if immediateFlush then
                flush ()
                ignore (Timer.never flushTimer)
            else
                ignore (Timer.at (TimeSpan.FromMilliseconds(100.0)) flushTimer)

    do
        resetRecycleTimer ()
        proc.OutputDataReceived.Add post
        proc.ErrorDataReceived.Add post

        ignore (Directory.CreateDirectory path)
        for name, text in initTexts do
            File.WriteAllText(Path.Combine(path, name), text)

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
            recycleTimer.Dispose()
            flushTimer.Dispose()
            flush ()
