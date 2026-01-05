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

[<Theory>]
[<InlineData( 0,  2)>]
[<InlineData( 1,  2)>]
[<InlineData( 2,  3)>]
[<InlineData( 3,  4)>]
[<InlineData( 4,  5)>]
[<InlineData( 5,  7)>]
[<InlineData( 6,  7)>]
[<InlineData( 7, 14)>]
[<InlineData( 8, 14)>]
[<InlineData( 9, 14)>]
[<InlineData(10, 14)>]
[<InlineData(11, 14)>]
[<InlineData(12, 14)>]
[<InlineData(13, 14)>]
[<InlineData(14, 15)>]
let ``Successor in CLRS example tree`` x expected =
    let actual = Veb.successor clrsTree x
    Some expected =! actual

[<Fact>]
let ``Successor of 15 in CLRS example tree`` () =
    let actual = Veb.successor clrsTree 15
    None =! actual

[<Theory>]
[<InlineData( 3,  2)>]
[<InlineData( 4,  3)>]
[<InlineData( 5,  4)>]
[<InlineData( 6,  5)>]
[<InlineData( 7,  5)>]
[<InlineData( 8,  7)>]
[<InlineData( 9,  7)>]
[<InlineData(10,  7)>]
[<InlineData(11,  7)>]
[<InlineData(12,  7)>]
[<InlineData(13,  7)>]
[<InlineData(14,  7)>]
[<InlineData(15, 14)>]
let ``Predecessor in CLRS example tree`` x expected =
    let actual = Veb.predecessor clrsTree x
    Some expected =! actual

[<Theory>]
[<InlineData( 0)>]
[<InlineData( 1)>]
[<InlineData( 2)>]
let ``Predecessor of low numbers in CLRS example tree`` x =
    let actual = Veb.predecessor clrsTree x
    None =! actual

[<Fact>]
let ``Insert into empty tree`` () =
    let sut = Veb.empty 16

    let actual = Veb.insert sut 9

    actual.Min =! Some 9
    actual.Max =! Some 9

[<Fact>]
let ``Build up example tree by inserts`` () =
    let actual = 
        Veb.empty 16
        |> (fun t -> Veb.insert t  2)
        |> (fun t -> Veb.insert t  3)
        |> (fun t -> Veb.insert t  4)
        |> (fun t -> Veb.insert t  5)
        |> (fun t -> Veb.insert t  7)
        |> (fun t -> Veb.insert t 14)
        |> (fun t -> Veb.insert t 15)
    clrsTree =! actual

[<Theory>]
[<InlineData( 0)>]
[<InlineData( 1)>]
[<InlineData( 2)>]
[<InlineData( 3)>]
[<InlineData( 4)>]
[<InlineData( 5)>]
[<InlineData( 6)>]
[<InlineData( 7)>]
[<InlineData( 8)>]
[<InlineData( 9)>]
[<InlineData(10)>]
[<InlineData(11)>]
[<InlineData(12)>]
[<InlineData(13)>]
[<InlineData(14)>]
[<InlineData(15)>]
let ``Insert and delete`` x =
    let sut = Veb.empty 16 |> (fun t -> Veb.insert t x)
    let actual = Veb.delete sut x
    Veb.empty 16 =! actual

[<Theory>]
[<InlineData( 0)>]
[<InlineData( 1)>]
[<InlineData( 2)>]
[<InlineData( 3)>]
[<InlineData( 4)>]
[<InlineData( 5)>]
[<InlineData( 6)>]
[<InlineData( 7)>]
[<InlineData( 8)>]
[<InlineData( 9)>]
[<InlineData(10)>]
[<InlineData(11)>]
[<InlineData(12)>]
[<InlineData(13)>]
[<InlineData(14)>]
[<InlineData(15)>]
let ``Delete from example tree`` x =
    let actual = Veb.delete clrsTree x
    test <@ not (Veb.isMember actual x) @>