module Tests

open Xunit
open FsUnit.Xunit

open Year2021Day1
open Year2021Day2
open Year2021Day3

[<Fact>]
let ``Day 1 Part 1: Number of times a depth measurement increases`` () = 
    SolveDay1Part1 |> should equal 1688

[<Fact>]
let ``Day 1 Part 2: Number of times a three-measurement sliding window increases`` () = 
    SolveDay1Part2 |> should equal 1728

[<Fact>]
let ``Day 2 Part 1: Multiply final horizontal and depth (without aim)`` () = 
    SolveDay2Part1 |> should equal 1693300

[<Fact>]
let ``Day 2 Part 2: Mutiply final horizontal and depth (using aim)`` () = 
    SolveDay2Part2 |> should equal 1857958050

[<Fact>]
let ``Day 3 Part 1:`` () = 
    SolveDay3Part1 |> should equal 0

[<Fact>]
let ``Day 3 Part 1: `` () = 
    SolveDay3Part2 |> should equal 0