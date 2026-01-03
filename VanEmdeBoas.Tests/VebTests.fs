module Ploeh.CS.VanEmdeBoas.Tests.VebTests

open Ploeh.CS.VanEmdeBoas
open Xunit
open Swensen.Unquote

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

[<Fact>]
let ``Minimum of CLRS example tree`` () =
    let min = Veb.minimum clrsTree
    min =! Some 2

[<Fact>]
let ``Maximum of CLRS example tree`` () =
    let max = Veb.maximum clrsTree
    max =! Some 15

[<Theory>]
[<InlineData( 0, false)>]
[<InlineData( 1, false)>]
[<InlineData( 2,  true)>]
[<InlineData( 3,  true)>]
[<InlineData( 4,  true)>]
[<InlineData( 5,  true)>]
[<InlineData( 6, false)>]
[<InlineData( 7,  true)>]
[<InlineData( 8, false)>]
[<InlineData( 9, false)>]
[<InlineData(10, false)>]
[<InlineData(11, false)>]
[<InlineData(12, false)>]
[<InlineData(13, false)>]
[<InlineData(14,  true)>]
[<InlineData(15,  true)>]
let ``Membership tests of CLRS example tree`` candidate expected =
    let actual = Veb.isMember clrsTree candidate
    actual =! expected