module Year2021Day1

open System.IO

let inputs = 
    File.ReadAllLines("inputs/year2021day1.txt")
    |> Seq.map int

let SolveDay1Part1 = 
    inputs
    |> Seq.pairwise
    |> Seq.filter (fun (x,y) -> x < y)
    |> Seq.length