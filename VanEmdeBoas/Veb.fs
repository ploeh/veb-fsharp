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

    let private index tree x y = x * (int (sqrt (float tree.UniverseSize))) + y

    // Would normally be called `contains` in F#
    let rec isMember tree x =
        match tree.Min, tree.Max with
        | None, None -> false
        | Some min, Some max when x = min || x = max -> true
        | _ when tree.UniverseSize = 2 -> false
        | _ -> isMember (tree.Cluster[high tree x]) (low tree x)

    let rec successor tree x =
        if tree.UniverseSize = 2 then
            if x = 0 && tree.Max = Some 1 then
                Some 1
            else None
        else
            match tree.Min with
            | Some min when x < min -> Some min
            | _ ->
                match maximum (tree.Cluster[high tree x]) with
                | Some maxLow when low tree x < maxLow ->
                    let offset =
                        successor (tree.Cluster[high tree x]) (low tree x)
                    offset |> Option.map (index tree (high tree x))
                | _ -> option {
                    let! summary = tree.Summary
                    let! succCluster = successor summary (high tree x)
                    let! offset = minimum (tree.Cluster[succCluster])
                    return index tree succCluster offset }