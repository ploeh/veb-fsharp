namespace Ploeh.CS.VanEmdeBoas

type VebNode = {
    UniverseSize : int
    Min : int option
    Max : int option
    Summary : VebNode option
    Cluster : VebNode array }

module Veb =
    let private log2 x = log (float x) / log 2.0

    let private upperSqrt u =
        int (2.0 ** (ceil (float (log2 (float u)) / 2.0)))

    let private lowerSqrt u =
        int (2.0 ** (floor (float (log2 (float u)) / 2.0)))

    let rec empty universeSize =
        if universeSize = 2 then
            {
                UniverseSize = universeSize
                Min = None
                Max = None
                Summary = None
                Cluster = [||]
            }
        else
            let usqr = upperSqrt universeSize
            let lwsqr = lowerSqrt universeSize
            {
                UniverseSize = universeSize
                Min = None
                Max = None
                Summary = empty usqr |> Some
                Cluster = Array.init usqr (fun _ -> empty lwsqr)
            }

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
                | Some maxLow when low tree x < maxLow -> option {
                     let! offset =
                         successor (tree.Cluster[high tree x]) (low tree x)
                     return index tree (high tree x) offset }
                | _ -> option {
                    let! summary = tree.Summary
                    let! succCluster = successor summary (high tree x)
                    let! offset = minimum (tree.Cluster[succCluster])
                    return index tree succCluster offset }

    let rec predecessor tree x =
        if tree.UniverseSize = 2 then
            if x = 1 && tree.Min = Some 0 then
                Some 0
            else None
        else
            match tree.Max with
            | Some max when x > max -> Some max
            | _ ->
                match minimum (tree.Cluster[high tree x]) with
                | Some minLow when low tree x > minLow -> option {
                     let! offset =
                         predecessor (tree.Cluster[high tree x]) (low tree x)
                     return index tree (high tree x) offset }
                | _ -> option {
                    let! summary = tree.Summary
                    match predecessor summary (high tree x) with
                    | None ->
                        let! min = tree.Min
                        if x > min then
                            return min
                        else
                            return! None
                    | Some predCluster ->
                        let! offset = maximum (tree.Cluster[predCluster])
                        return index tree predCluster offset }

    let private insertIntoEmpty tree x =
        { tree with Min = Some x; Max = Some x }

    let private modifySummary f x =
        let updatedSummary = f x
        State.modify (fun t -> { t with Summary = Some updatedSummary })

    let private modifyCluster f clusterIndex x = state {
        let! tree = State.get
        let arr = f tree.Cluster[clusterIndex] x
        let updatedCluster = tree.Cluster |> Array.updateAt clusterIndex arr
        do! State.put { tree with Cluster = updatedCluster }
    }

    let rec insert tree x =
        match tree.Min with
        | None -> insertIntoEmpty tree x
        | Some min ->
            let comp = state {
                let! x = state {
                    if x < min then
                        do! State.put { tree with Min = Some x }
                        return min
                    else return x
                }

                match! State.gets _.Summary with
                | None -> ()
                | Some summary ->
                    // This case only occurs when UniverseSize > 2, so this is
                    // effectively equivalent to V.u > 2
                    match minimum (tree.Cluster[high tree x]) with
                    | None ->
                        do! modifySummary (insert summary) (high tree x)
                        do! modifyCluster
                                insertIntoEmpty (high tree x) (low tree x)
                    | Some _ ->
                        do! modifyCluster insert (high tree x) (low tree x)

                let! max = State.gets _.Max
                if (max |> Option.exists ((>) x)) then
                    do! State.modify (fun t -> { t with Max = Some x })
            }

            comp |> State.exec tree