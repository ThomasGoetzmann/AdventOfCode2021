module Year2021Day4

open System
open System.IO

let inputs =
    File.ReadAllText("inputs/year2021day4.txt")

type Square = {
    Row: int
    Column: int
    Number: int
    Marked: bool }

type Board = { Squares : Square list } with

    member this.rowComplete row = 
        this.Squares 
        |> List.filter (fun s -> s.Row = row) 
        |> List.forall (fun s -> s.Marked)

    member this.columnComplete column = 
        this.Squares
        |> List.filter (fun s -> s.Column = column)
        |> List.forall (fun s -> s.Marked)

    member this.isWinning =
        let rec isWin complete number = 
            if number > 5 then false
            elif complete number then true
            else isWin complete (number+1)
        isWin this.rowComplete 1 || isWin this.columnComplete 1

let markNumber number board = 
    board.Squares 
    |> List.map (fun s ->  if s.Number = number then {s with Marked = true} else s)
    |> fun squares -> { Squares = squares}

let toBoard (input:string) = 
    let squares = 
        input.Replace(System.Environment.NewLine, " ").Split(' ', StringSplitOptions.RemoveEmptyEntries)
        |> Seq.indexed
        |> Seq.map (fun (index, value) -> {Row = index / 5 + 1; Column = index % 5 + 1; Number=(int)value; Marked = false})
        |> Seq.toList
    
    { Squares = squares }

let rec extractBoards acc (inputs:string list) = 
        match inputs with
        | [] -> acc |> List.rev
        | head::tail -> extractBoards ((toBoard head)::acc) tail

let parse (input:string) =
    let splits = input.Split(System.Environment.NewLine + System.Environment.NewLine)
    let numbers = splits.[0].Split(',') |> Seq.map (fun n -> (int)n) |> Seq.toList
    let boards = extractBoards [] (splits.[1..] |> List.ofArray)
    numbers, boards

let rec playBingo (numbers,boards: Board list) = 
    match numbers with 
    | [] -> None
    | number :: tail -> 
        let updatedBoards = boards |> List.map (fun board -> markNumber number board)
        match (updatedBoards |> List.tryFind (fun b -> b.isWinning)) with
        | Some b -> Some (b, number)
        | None -> playBingo (tail,updatedBoards)

let calculatePoints (board,number) =
    let sum = 
        board.Squares
        |> List.filter (fun s -> not s.Marked )
        |> List.map (fun s -> s.Number)
        |> List.reduce (+) 
    
    let points = sum * number
    points
    

let SolveDay4Part1 = 
    let winningBoard = 
        inputs
        |> parse //parse inputs
        |> playBingo
    
    match winningBoard with
    | Some board -> board |> calculatePoints
    | None -> -1

let SolveDay4Part2 = 
    -1