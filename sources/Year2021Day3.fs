module Year2021Day3

open System.IO

let inputs =
    File.ReadAllLines("inputs/year2021day3.txt")
    |> Array.map Array.ofSeq

type Diagnostics =
    { GammaRate: int
      EpsilonRate: int }
      
      member this.Consumption =
        this.EpsilonRate * this.GammaRate

let toTransposedArray2D (array: char [] []) = 
    let x = array.[0].Length
    let y = array.Length

    Array2D.init x y (fun x y -> array.[y].[x])

let getMaxes (array2D : char[,]) =
    [
        let rowSize = array2D.GetLength 0
        for row in 0..rowSize-1 do
        yield 
            array2D.[row,*] 
            |> Seq.countBy id 
            |> Seq.maxBy (fun (bit,occurences) -> occurences) 
            |> fun (bit, occurences) -> bit
    ]

let invertBinaryString binaryString =
    let inv c = 
        match c with
        | '0' -> '1'
        | '1' -> '0'
        | _ -> failwith "Invalid value in BinaryString"

    binaryString
    |> Seq.map inv
    |> System.String.Concat

let extractDiagnostics (chars : char seq) =
    let binaryString = chars |> System.String.Concat
    let invertedBinaryString = binaryString |> invertBinaryString
    
    { 
        GammaRate = System.Convert.ToInt32(binaryString, 2);
        EpsilonRate = System.Convert.ToInt32(invertedBinaryString, 2) 
    }

let SolveDay3Part1 =
    inputs
    |> toTransposedArray2D
    |> getMaxes
    |> extractDiagnostics
    |> fun d -> d.Consumption

let SolveDay3Part2 =
    -1