module Tests

open Xunit
open FsUnit.Xunit

open Year2021Day1
open Year2021Day2
open Year2021Day3
open Year2021Day4
open Year2021Day5
open Year2021Day6

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
let ``Day 3 Part 1: Power consumption`` () = 
    SolveDay3Part1 |> should equal 4001724

[<Fact>]
let ``Day 3 Part 1: Life support rating `` () = 
    SolveDay3Part2 |> should equal 587895

[<Fact>]
let ``Day 4 Part 1: Points for first winning grid`` () = 
    SolveDay4Part1 |> should equal 35711

[<Fact>]
let ``Day 4 Part 1: Points for last winning grid `` () = 
    SolveDay4Part2 |> should equal 5586

[<Fact>]
let ``Day 6 Part 1: Number of fishes after 80 days`` () = 
    SolveDay6Part1 |> should equal 377263L

[<Fact>]
let ``Day 6 Part 1: Number of fishes after 256 days`` () = 
    SolveDay6Part2 |> should equal 1695929023803L
