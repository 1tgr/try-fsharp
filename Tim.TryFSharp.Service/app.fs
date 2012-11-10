namespace Tim.TryFSharp.Service

open System
open System.Diagnostics
open System.Globalization
open System.IO
open System.Net
open System.Xml.XPath
open Tim.TryFSharp.Core

type App =
    {
        OwnServerId : string
        BaseUri : Uri
        OwnSessions : Map<string, string * FsiProcess>
        SlowStop : bool
    }

type Command =
    | Exit of AsyncReplyChannel<App>
    | Recycle of string
    | SlowStop
    | StdIn of string * Message
    | StdOut of Message

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module App =
    let emptySession : Session =
        { 
            Rev = None
            Type = "session"
            Owner = None
            Host = Some (Dns.GetHostName())
            ServicePid = Some (int64 (Process.GetCurrentProcess().Id))
            FsiPid = None
            InitNames = [| |]
            InitTexts = [| |]
            Quiet = None
            NoLogo = None
            Started = None
        }

    type Claim =
        | OwnSession of FsiProcess
        | OtherSession of string * int64
        | ProcessFailed of Exception
        | SlowStopIgnore

    let claimSession (app : App) (inbox : MailboxProcessor<_>) (id : string) : App * Claim =
        let rec impl () =
            match TryFSharpDB.getSession app.BaseUri id with
            | Some ({ Owner = Some owner } as session) when owner <> app.OwnServerId ->
                app, OtherSession(defaultArg session.Host "", defaultArg session.ServicePid -1L)

            | s ->
                let session =
                    match s with
                    | Some session ->
                        { session with
                            Host = emptySession.Host
                            ServicePid = emptySession.ServicePid
                        }

                    | None -> emptySession
                    
                let session = { session with Owner = Some app.OwnServerId }

                match TryFSharpDB.safePutSession app.BaseUri id session with
                | Some rev ->
                    let session = { session with Rev = rev.Rev }

                    let arguments =
                        [|
                            if defaultArg session.Quiet false then
                                yield "--quiet"

                            if defaultArg session.NoLogo false then
                                yield "--nologo"
                        |]

                    try
                        let info : FsiProcessInfo =
                            {
                                Name = id
                                InitTexts = Array.zip session.InitNames session.InitTexts
                                Arguments = arguments

                                Print = fun s ->
                                    let message : Message =
                                        {
                                            Rev = None
                                            Date = Some (DateTime.UtcNow.ToString("o"))
                                            MessageType = "out"
                                            SessionId = id
                                            Message = s
                                            QueueStatus = None
                                        }

                                    inbox.Post (StdOut message)
    
                                Recycle = fun () -> inbox.Post (Recycle id)
                            }

                        let proc = new FsiProcess(info)
                        let session =
                            { session with
                                FsiPid = Some (int64 proc.Process.Id)
                                Started = Some DateTime.Now
                            }

                        let rev = TryFSharpDB.putSession app.BaseUri id session
                        { app with OwnSessions = Map.add id (Option.get rev.Rev, proc) app.OwnSessions }, OwnSession proc
                    with ex ->
                        CouchDB.deleteDocument app.BaseUri id (Option.get rev.Rev)
                        app, ProcessFailed ex

                | None ->
                    impl ()

        match Map.tryFind id app.OwnSessions, app.SlowStop with
        | Some (_, proc), _ -> app, OwnSession proc
        | None, true -> app, SlowStopIgnore
        | None, false -> impl ()

    let killSession (app : App) (id : string) (rev : string) (proc : FsiProcess) : App =
        try
            CouchDB.deleteDocument app.BaseUri id rev
        with ex ->
            Log.info "Failed to delete (%s, %s) - %O" id rev ex

        (proc :> IDisposable).Dispose()
        { app with OwnSessions = Map.remove id app.OwnSessions }

    let rec run (app : App) (inbox : MailboxProcessor<_>) : Async<unit> =
        let impl =
            function
            | Exit reply ->
                reply.Reply app
                async { return () }

            | Recycle id ->
                let app =
                    match Map.tryFind id app.OwnSessions with
                    | Some (rev, proc) -> killSession app id rev proc
                    | None -> app

                run app inbox

            | SlowStop ->
                run { app with SlowStop = true } inbox

            | StdIn (id, message) ->
                let app, claim = claimSession app inbox message.SessionId
                match claim with
                | OtherSession (host, pid) ->
                    Log.info "Ignoring %s - owned by session %s on %s (pid %d)" id message.SessionId host pid

                | OwnSession proc ->
                    let message = { message with QueueStatus = Some "done" }

                    let message =
                        if Option.isNone message.Date then
                            { message with Date = Some (DateTime.UtcNow.ToString("o")) }
                        else
                            message

                    let rev = TryFSharpDB.putMessage app.BaseUri id message
                    let message = { message with Rev = rev.Rev }
                    proc.Process.StandardInput.WriteLine message.Message

                | ProcessFailed ex ->
                    Log.info "%O" ex

                | SlowStopIgnore ->
                    Log.info "Ignoring %s - in slow stop mode" id

                run app inbox

            | StdOut message ->
                ignore (TryFSharpDB.postMessage app.BaseUri message)
                run app inbox

        async {
            try
                let! command = inbox.Receive()
                return! impl command
            with ex ->
                Log.info "%O" ex
                return! run app inbox
        }

    let shutdown (app : App) =
        (app, app.OwnSessions)
        ||> Map.fold (fun app id (rev, proc) -> killSession app id rev proc)

