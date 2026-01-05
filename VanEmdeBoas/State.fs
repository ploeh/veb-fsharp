namespace Ploeh.CS.VanEmdeBoas

module State =
    type State<'s, 'a> = 's -> ('a * 's)
    let run s stateFn = stateFn s
    let eval s stateFn = stateFn s |> fst
    let exec s stateFn = stateFn s |> snd
    let returnState a s = (a, s)
    let bind f stateFn s =
        let a, s' = stateFn s
        f a s'
    let map f stateFn s = bind (f >> returnState) stateFn s
    let get s = (s, s)
    let gets f s = (f s, s)
    let put newState _ = ((), newState)
    let modify f s = ((), f s)

type StateBuilder() =
    member _.Return a = State.returnState a
    member _.ReturnFrom stateFn = stateFn
    member _.Bind (stateFn, f) = State.bind f stateFn

[<AutoOpen>]
module StateComputationExpression =
    let state = StateBuilder ()