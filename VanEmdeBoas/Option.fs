namespace Ploeh.CS.VanEmdeBoas

type OptionBuilder() =
    member _.Bind (x, f) = Option.bind f x
    member _.Return x = Some x
    member _.ReturnFrom x = x
    member _.Zero() = None

[<AutoOpen>]
module OptionComputationExpression =
    let option = OptionBuilder ()