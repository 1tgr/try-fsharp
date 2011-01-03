namespace Tim.TryFSharp.Interactive

open Newtonsoft.Json

type Shape = Line of (int * int) * (int * int)

type Graphic =
    {
        Shapes : Shape list
    }
    with
        static member Empty : Graphic =
            {
                Shapes = []
            }

type State<'a> = State of (Graphic -> 'a * Graphic)

type DrawBuilder() = 
        member b.Bind(m, f) = State (fun s ->
            let (State g) = m
            let v, s = g s
            let (State h) = f v
            h s)
        member b.Return(x) = State (fun s -> x, s)

[<AutoOpen>]
module Draw =
    let draw = DrawBuilder()

    let execute (m : State<unit>) =
        let (State f) = m
        let _, graphics = f Graphic.Empty
        List.toArray (List.rev graphics.Shapes)

    let shape (shape : Shape) : State<unit> = State (fun g -> (), { Graphic.Shapes = shape :: g.Shapes })
    let line a b = shape (Line (a, b))
