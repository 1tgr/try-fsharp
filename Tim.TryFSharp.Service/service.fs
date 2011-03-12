namespace Tim.TryFSharp.Service

open System
open System.Globalization
open System.IO
open System.Net
open System.Threading
open System.Xml.XPath
open Newtonsoft.Json
open Tim.TryFSharp.Core

module Service =
    type Gist =
        {
            [<JsonName("created_at")>]  CreatedAt : string
            [<JsonName("repo")>]        Repo : string
            [<JsonName("files")>]       Files : string array
            [<JsonName("owner")>]       Owner : string
            [<JsonName("description")>] Description : string
        }

    type Gists =
        {
            [<JsonName("gists")>] Gists : Gist array
        }

    let subscribeAsync (baseUri : Uri) (mailbox : MailboxProcessor<_>) : Async<unit> =
        let rec impl lastSeq =
            async {
                try
                    let! lastSeq, (results : Change<Message> array) = CouchDB.changes baseUri (Some "app/stdin") lastSeq
                    for result in results do
                        let message =
                            match result.Doc with
                            | Some message -> message
                            | None -> TryFSharpDB.getMessage baseUri result.Id

                        if Option.isNone message.QueueStatus then
                            mailbox.Post (StdIn (result.Id, message))

                    return! impl (Some lastSeq)
                with ex ->
                    Log.info "%O" ex
                    return! impl None
            }

        impl None
        
    let rec changeSnippet (baseUri : Uri) (id : string) (fn : string option -> Snippet) : unit =
        let oldSnippet : Snippet option = CouchDB.notFound (CouchDB.getDocument baseUri) id
        let rev =
            match oldSnippet with
            | Some snippet -> snippet.Rev
            | None -> None

        let newSnippet = fn rev

        let changed =
            match oldSnippet with
            | Some oldSnippet -> oldSnippet <> newSnippet
            | None -> true

        if changed then
            if Option.isNone (CouchDB.conflicted (CouchDB.putDocument baseUri id) newSnippet) then
                changeSnippet baseUri id fn

    let refreshFSSnip (baseUri : Uri) : Async<unit> =
        let select (nav : XPathNavigator) (xpath : string) : 'a =
            let child = nav.SelectSingleNode(xpath)
            unbox (child.ValueAs(typeof<'a>))

        async {
            try
                use client = new WebClient()
                let doc = (XPathDocument("http://fssnip.net/pages/Rss")).CreateNavigator()
                let items : seq<XPathNavigator> = Seq.cast (doc.Select("/rss/channel/item"))

                for item in items do
                    try
                        let link : string = select item "link"
                        let builder = UriBuilder(link)
                        let id = sprintf "snippet-%s" (builder.Path.Substring(1))
                        builder.Path <- "/raw" + builder.Path

                        let! code = client.AsyncDownloadString(builder.Uri)
                        changeSnippet baseUri id <| fun rev ->
                            {
                                Rev = rev
                                Type = "snippet"
                                Title = select item "title"
                                Date = DateTime.Parse(select item "pubDate", CultureInfo.InvariantCulture)
                                Author = select item "author"
                                UserId = Some "fssnip"
                                Private = None
                                Description = select item "description"
                                Link = Some (select item "link")
                                Code = code
                            }
                    with ex ->
                        Log.info "Failed to import RSS item. %O" ex
            with ex ->
                Log.info "Failed to import RSS feed. %O" ex
        }

    let refreshGists (baseUri : Uri) : Async<unit> =
        async {
            try
                use client = new WebClient()
                let gistUri = Uri("https://gist.github.com/")
                let user = "timrobinson"
                let! s = client.AsyncDownloadString(Uri(Uri(gistUri, "/api/v1/json/"), user))
                let gists : Gists =
                    using (new StringReader(s)) <| fun reader ->
                        use reader = new JsonTextReader(reader)
                        Json.parse reader

                for gist in gists.Gists do
                    for filename in gist.Files do
                        try
                            let id, link =
                                if Array.length gist.Files = 1 then
                                    sprintf "gist-%s" gist.Repo, Uri(gistUri, gist.Repo)
                                else
                                    let builder = UriBuilder(Uri(gistUri, gist.Repo))
                                    builder.Fragment <- filename
                                    sprintf "gist-%s-%s" gist.Repo filename, builder.Uri

                            let! code = client.AsyncDownloadString(Uri(Uri(Uri(gistUri, "/raw/"), gist.Repo), filename))

                            changeSnippet baseUri id <| fun rev ->
                                {
                                    Rev = rev
                                    Type = "snippet"
                                    Title = gist.Description
                                    Date = DateTime.Parse(gist.CreatedAt, CultureInfo.InvariantCulture)
                                    Author = user
                                    UserId = Some user
                                    Private = Some true
                                    Description = ""
                                    Link = Some (string link)
                                    Code = code
                                }
                        with ex ->
                            Log.info "Failed to import gist %s/%s. %O" gist.Repo filename ex
            with ex ->
                Log.info "Failed to import gists. %O" ex
        }

    let refreshFeedsAsync (baseUri : Uri) : Async<unit> =
        Async.Parallel [| refreshFSSnip baseUri; refreshGists baseUri |]
        |> Async.Ignore

type ServiceState =
    {
        Mailbox : MailboxProcessor<Command>
        CancellationTokenSource : CancellationTokenSource
        RefreshFeedsTimer : Timer
    } with
    static member Create (config : ServiceConfig) =
        let cts = new CancellationTokenSource()

        let mailbox =
            let app : App =
                {
                    OwnServerId = string (Guid.NewGuid())
                    BaseUri = config.BaseUri
                    OwnSessions = Map.empty
                    SlowStop = false
                }

            MailboxProcessor.Start(App.run app, cts.Token)

        Async.Start(Service.subscribeAsync config.BaseUri mailbox, cts.Token)

        let timer =
            fun _ ->
                let a = Service.refreshFeedsAsync config.BaseUri
                Async.RunSynchronously(a, cancellationToken = cts.Token)
            |> Timer.timer 
            |> Timer.every (TimeSpan.FromMinutes(10.0))

        {
            Mailbox = mailbox
            CancellationTokenSource = cts
            RefreshFeedsTimer = timer
        }

    member this.SlowStop () =
        this.Mailbox.Post SlowStop

    interface IDisposable with
        member this.Dispose() =
            ignore (App.shutdown (this.Mailbox.PostAndReply Exit))
            this.RefreshFeedsTimer.Dispose()
            this.CancellationTokenSource.Cancel()
            this.CancellationTokenSource.Dispose()

type Launcher() =
    inherit MarshalByRefObject()

    let mutable state : ServiceState option = None

    override this.InitializeLifetimeService() =
        null

    interface IService with
        member this.Start config =
            state <- Some (ServiceState.Create config)

        member this.SlowStop () =
            match state with
            | Some state -> state.SlowStop()
            | None -> ()

    interface IDisposable with
        member this.Dispose() =
            match state with
            | Some state -> (state :> IDisposable).Dispose()
            | None -> ()

            state <- None
