#I "bin/Debug"
#r "Tim.TryFSharp.Core.dll"

open System
open System.Globalization
open System.Net
open System.Xml.XPath
open Tim.TryFSharp.Core

type Snippet =
    {
        [<JsonName("_rev")>] Rev : string option
        [<JsonName("type")>] Type : string
        [<JsonName("title")>] Title : string
        [<JsonName("date")>] Date : DateTime
        [<JsonName("author")>] Author : string
        [<JsonName("description")>] Description : string
        [<JsonName("link")>] Link: string option
        [<JsonName("code")>] Code : string
    }

//let baseUri = Uri("http://veneto:5984/tryfs/")
let baseUri = Uri("http://tryfs.net/tryfs/")

let doc = (XPathDocument("http://fssnip.net/pages/Rss")).CreateNavigator()
let items = doc.Select("/rss/channel/item")

let select (nav : XPathNavigator) (xpath : string) : 'a =
    let child = nav.SelectSingleNode(xpath)
    unbox (child.ValueAs(typeof<'a>))

for item in items do
    let item : XPathNavigator = unbox item
    let link : string = select item "link"
    let builder = UriBuilder(link)
    let id = sprintf "snippet-%s" (builder.Path.Substring(1))
    builder.Path <- "/raw" + builder.Path

    use client = new WebClient()
    let code = client.DownloadString(builder.Uri)

    let oldSnippet : Snippet option = CouchDB.notFound (CouchDB.getDocument baseUri) id

    let newSnippet : Snippet =
        {
            Rev =
                match oldSnippet with
                | Some snippet -> snippet.Rev
                | None -> None

            Type = "snippet"
            Title = select item "title"
            Date = DateTime.Parse(select item "pubDate", CultureInfo.InvariantCulture)
            Author = select item "author"
            Description = select item "description"
            Link = Some (select item "link")
            Code = code
        }

    let changed =
        match oldSnippet with
        | Some oldSnippet -> oldSnippet <> newSnippet
        | None -> true

    if changed then        
        ignore (CouchDB.putDocument baseUri id newSnippet)
