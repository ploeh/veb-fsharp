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