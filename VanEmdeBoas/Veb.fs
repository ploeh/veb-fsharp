namespace Ploeh.CS.VanEmdeBoas

type VebNode = {
    UniverseSize : int
    Min : int option
    Max : int option
    Summary : VebNode option
    Cluster : VebNode array }

module Veb =
    let minimum tree = tree.Min
    let maximum tree = tree.Max

    let private high tree x = x / (int (sqrt (float tree.UniverseSize)))

    let private low tree x = x % (int (sqrt (float tree.UniverseSize)))

    // Would normally be called `contains` in F#
    let rec isMember tree x =
        match tree.Min, tree.Max with
        | None, None -> false
        | Some min, Some max when x = min || x = max -> true
        | _ when tree.UniverseSize = 2 -> false
        | _ -> isMember (tree.Cluster[high tree x]) (low tree x)