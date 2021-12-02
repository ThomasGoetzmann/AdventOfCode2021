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
    Aim : int
}

let parse line = 
    let pattern = @"(?'direction'.*) (?'number'\d*)"
    let m = Regex.Match(line, pattern)

    match m.Groups["direction"].Value with
    | "forward" -> Forward(int m.Groups["number"].Value)
    | "down" -> Down(int m.Groups["number"].Value)
    | "up" -> Up(int m.Groups["number"].Value)
    | _ -> failwith "Invalid input"
    
let Part1MovesFrom initialPosition instructions =
    let rec Move p instructions = 
        match instructions with
        | [] -> p
        | head :: tail -> 
            match head with
            | Up x -> Move {p with Depth = p.Depth - x} tail
            | Down x -> Move {p with Depth = p.Depth + x} tail
            | Forward x-> Move {p with Horizontal= p.Horizontal + x} tail
    
    Move initialPosition instructions

let Part2MovesFrom initialPosition instructions =
    let rec Move p instructions = 
        match instructions with
        | [] -> p
        | head :: tail  ->
            match head with
            | Up x -> Move {p with Aim= p.Aim - x} tail
            | Down x -> Move {p with Aim= p.Aim + x} tail
            | Forward x -> Move {p with Horizontal= p.Horizontal + x; Depth = p.Depth + (p.Aim * x)} tail
    
    Move initialPosition instructions

let Multiply p = 
    p.Horizontal * p.Depth

let SolveDay2Part1 = 
    inputs
    |> List.map parse
    |> Part1MovesFrom {Horizontal= 0; Depth =0; Aim=0}
    |> Multiply

let SolveDay2Part2 = 
    inputs
    |> List.map parse
    |> Part2MovesFrom { Horizontal=0; Depth=0; Aim=0}
    |> Multiply