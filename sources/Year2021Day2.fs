module Year2021Day2

open System.IO
open System.Text.RegularExpressions

let inputs = 
    File.ReadAllLines("inputs/year2021day2.txt")
    |> List.ofSeq

type Instruction =
    | Forward of int
    | Up of int
    | Down of int

type Position = {
    Horizontal: int
    Depth: int
}

let parse line = 
    let pattern = @"(?'direction'.*) (?'number'\d*)"
    let m = Regex.Match(line, pattern)

    match m.Groups["direction"].Value with
    | "forward" -> Forward(int m.Groups["number"].Value)
    | "down" -> Down(int m.Groups["number"].Value)
    | "up" -> Up(int m.Groups["number"].Value)
    | _ -> failwith "Invalid input"

let MoveFrom initialPosition instructions =
    let rec Move p instructions = 
        match instructions with
        | [] -> p
        | head :: tail -> 
            match head with
            | Forward x-> Move {p with Horizontal= p.Horizontal + x} tail
            | Up x -> Move {p with Depth = p.Depth - x} tail
            | Down x -> Move {p with Depth = p.Depth + x} tail
    
    Move initialPosition instructions

let Multiply p = 
    p.Horizontal * p.Depth

let SolveDay2Part1 = 
    inputs
    |> List.map parse
    |> MoveFrom {Horizontal= 0; Depth =0}
    |> Multiply

let SolveDay2Part2 = 
    -1