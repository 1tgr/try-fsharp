namespace Tim.TryFSharp.Test

open Tim.TryFSharp.Interactive
open Xunit

module Draw =
    [<Fact>]
    let ``Can draw nothing`` () =
        let d = draw { return () }
        let shapes = Draw.execute d
        Assert.Equal([| |], shapes)

    [<Fact>]
    let ``Can draw one line`` () =
        let d = line (0, 0) (100, 100)
        let shapes = Draw.execute d
        Assert.Equal([| Line((0, 0), (100, 100)) |], shapes)

    [<Fact>]
    let ``Can draw two lines`` () =
        let d = draw {
            do! line (0, 0) (100, 100)
            do! line (100, 0) (0, 100)
        }
        let shapes = Draw.execute d
        Assert.Equal([| Line((0, 0), (100, 100))
                        Line((100, 0), (0, 100)) |], shapes)
