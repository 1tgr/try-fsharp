namespace Tim.TryFSharp.Test

open System.IO
open Tim.TryFSharp.Core
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open Xunit

module Json =
    let roundTrip (value : 'a) =
        let schema = JsonSchema.ofType typeof<'a>
        let token = schema.Writer (box value)

        let s =
            if token = null then
                "null"
            else
                token.ToString(Formatting.Indented, Array.empty)

        let token' =
            use reader = new StringReader(s)
            use reader = new JsonTextReader(reader)
            JToken.ReadFrom reader

        let value' : 'a = unbox (schema.Reader token')
        Assert.Equal(value, value')
        ()

    [<Fact>]
    let ``Can round trip bool`` () = roundTrip true

    [<Fact>]
    let ``Can round trip double`` () = roundTrip 6.0

    [<Fact>]
    let ``Can round trip int`` () = roundTrip 6

    [<Fact>]
    let ``Can round trip string`` () = roundTrip "six"

    [<Fact>]
    let ``Can round trip bool array`` () = roundTrip [| true; false |]

    [<Fact>]
    let ``Can round trip double array`` () = roundTrip [| 6.0; 7.0 |]

    [<Fact>]
    let ``Can round trip int array`` () = roundTrip [| 6; 7 |]

    [<Fact>]
    let ``Can round trip string array`` () = roundTrip [| "six"; "seven" |]

    [<Fact>]
    let ``Can round trip int option array`` () = roundTrip [| Some 6; None |]

    type Record =
        {
            Number : int
            Text : string
        }

    [<Fact>]
    let ``Can round trip record`` () = roundTrip { Record.Number = 6; Text = "six" }

    [<Fact>]
    let ``Can round trip None`` () = let o : int option = None in roundTrip o

    [<Fact>]
    let ``Can round trip Some bool`` () = roundTrip (Some true)

    [<Fact>]
    let ``Can round trip Some double`` () = roundTrip (Some 6.0)

    [<Fact>]
    let ``Can round trip Some int`` () = roundTrip (Some 6)

    [<Fact>]
    let ``Can round trip Some string`` () = roundTrip (Some "six")

    type Union =
        | Case1
        | Case2 of int * string

    [<Fact>]
    let ``Can round trip union case 1`` () = roundTrip Case1

    [<Fact>]
    let ``Can round trip union case 2`` () = roundTrip (Case2 (6, "six"))

    [<Fact>]
    let ``Can round trip empty map`` () = let m : Map<string, int> = Map.empty in roundTrip m

    [<Fact>]
    let ``Can round trip int map`` () = roundTrip (Map.ofArray [| "First", 6; "Second", 7 |])

    [<Fact>]
    let ``Can round trip int option map`` () = roundTrip (Map.ofArray [| "First", Some 6; "Second", None |])
