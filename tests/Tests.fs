module Tests

open Xunit
open FsUnit.Xunit

open Year2021Day1
open Year2021Day2

[<Fact>]
let ``Day 1 Part 1: `` () = 
    SolveDay1Part1 |> should equal 1688

[<Fact>]
let ``Day 1 Part 2: `` () = 
    SolveDay1Part2 |> should equal 1728

[<Fact>]
let ``Day 2 Part 1: `` () = 
    SolveDay2Part1 |> should equal 1693300

[<Fact>]
let ``Day 2 Part 2: `` () = 
    SolveDay2Part2 |> should equal 1857958050