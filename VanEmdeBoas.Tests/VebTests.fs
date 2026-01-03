module Ploeh.CS.VanEmdeBoas.Tests.VebTests

open Ploeh.CS.VanEmdeBoas

// Example tree taken from CLRS 3rd ed., page 548
let private clrsTree = {
    UniverseSize = 16
    Min = Some 2
    Max = Some 15
    Summary = Some {
        UniverseSize = 4
        Min = Some 0
        Max = Some 3
        Summary = Some {
            UniverseSize = 2
            Min = Some 0
            Max = Some 1
            Summary = None
            Cluster = [||]
        }
        Cluster = [| {
            UniverseSize = 2
            Min = Some 1
            Max = Some 1
            Summary = None
            Cluster = [||]
        }; {
            UniverseSize = 2
            Min = Some 1
            Max = Some 1
            Summary = None
            Cluster = [||]
        } |]
    }
    Cluster = [| {
        UniverseSize = 4
        Min = Some 3
        Max = Some 3
        Summary = Some {
            UniverseSize = 2
            Min = None
            Max = None
            Summary = None
            Cluster = [||]
        }
        Cluster = [| {
            UniverseSize = 2
            Min = None
            Max = None
            Summary = None
            Cluster = [||]
        }; {
            UniverseSize = 2
            Min = None
            Max = None
            Summary = None
            Cluster = [||]
        } |]
    }; {
        UniverseSize = 4
        Min = Some 0
        Max = Some 3
        Summary = Some {
            UniverseSize = 2
            Min = Some 0
            Max = Some 1
            Summary = None
            Cluster = [||]
        }
        Cluster = [| {
            UniverseSize = 2
            Min = Some 1
            Max = Some 1
            Summary = None
            Cluster = [||]
        }; {
            UniverseSize = 2
            Min = Some 1
            Max = Some 1
            Summary = None
            Cluster = [||]
        } |]
    }; {
        UniverseSize = 4
        Min = None
        Max = None
        Summary = Some {
            UniverseSize = 2
            Min = None
            Max = None
            Summary = None
            Cluster = [||]
        }
        Cluster = [| {
            UniverseSize = 2
            Min = None
            Max = None
            Summary = None
            Cluster = [||]
        }; {
            UniverseSize = 2
            Min = None
            Max = None
            Summary = None
            Cluster = [||]
        } |]
    }; {
        UniverseSize = 4
        Min = Some 2
        Max = Some 3
        Summary = Some {
            UniverseSize = 2
            Min = Some 1
            Max = Some 1
            Summary = None
            Cluster = [||]
        }
        Cluster = [| {
            UniverseSize = 2
            Min = None
            Max = None
            Summary = None
            Cluster = [||]
        }; {
            UniverseSize = 2
            Min = Some 1
            Max = Some 1
            Summary = None
            Cluster = [||]
        } |]
    } |]
}