namespace Tim.TryFSharp.Test

open Xunit

type ServiceTests() =

    [<Fact>]
    let dummy () =
        Assert.True(false, "This is a dummy test for AppHarbor")
