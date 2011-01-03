namespace Tim.TryFSharp.Interactive

type Shape = Line of (int * int) * (int * int)

type State<'a> = State of (Shape list ->'a * Shape list)

type DrawBuilder() = 
        member b.Bind(m, f) = State (fun s -> let r = match m with
                                                      | State f -> f s
                                              match r with
                                              | (v,s) -> match f v with
                                                         | State f -> f s)    
        member b.Return(x) = State (fun s -> x, s)

[<AutoOpen>]
module Draw =
    let draw = DrawBuilder()

    let getState = State (fun s -> s, s)
    let setState s = State (fun _ -> (), s)  

    let execute (State f) =
        snd (f [])

    let line a b = (State (fun s -> (), Line (a, b) :: s))
