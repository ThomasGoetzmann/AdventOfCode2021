module Year2021Day2

open System.IO

let inputs =
    File.ReadAllLines("inputs/year2021day2.txt")
    |> List.ofSeq

type Instruction =
    | Up of int
    | Down of int
    | Forward of int

type Position =
    { Horizontal: int
      Depth: int
      Aim: int }

let parse (line:string) =
    let lineSplit = line.Split(' ')
    let direction = lineSplit.[0]
    let value = (int)lineSplit.[1]

    match direction with
    | "up" -> Up value
    | "down" -> Down value
    | "forward" -> Forward value
    | _ -> failwith "Invalid input"

let Part1Rules instruction p =
    match instruction with
    | Up x -> { p with Depth = p.Depth - x }
    | Down x -> { p with Depth = p.Depth + x }
    | Forward x -> { p with Horizontal = p.Horizontal + x }

let Part2Rules instruction p =
    match instruction with
    | Up x -> { p with Aim = p.Aim - x }
    | Down x -> { p with Aim = p.Aim + x }
    | Forward x ->
        { p with
              Horizontal = p.Horizontal + x
              Depth = p.Depth + (p.Aim * x) }

let RunWith rules instructions =
    let rec Move position instructions =
        match instructions with
        | [] -> position
        | head :: tail -> Move (rules head position) tail

    let initialPosition = { Horizontal = 0; Depth = 0; Aim = 0 }
    Move initialPosition instructions

let Multiply p = 
    p.Horizontal * p.Depth

let SolveDay2Part1 =
    inputs
    |> List.map parse
    |> RunWith Part1Rules
    |> Multiply

let SolveDay2Part2 =
    inputs
    |> List.map parse
    |> RunWith Part2Rules
    |> Multiply
