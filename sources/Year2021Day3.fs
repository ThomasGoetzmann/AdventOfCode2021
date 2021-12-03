module Year2021Day3

open System.IO

let inputs =
    File.ReadAllLines("inputs/year2021day3.txt")
    |> Array.map Array.ofSeq

type Diagnostics =
    { GammaRate: int
      EpsilonRate: int }

    member this.Consumption = this.EpsilonRate * this.GammaRate

type LifeSupport =
    | Oxygen
    | CO2

let array2DTranspose (array: char [] []) =
    let width = array.[0].Length
    let rows = array.Length

    Array2D.init width rows (fun x y -> array.[y].[x])

let getMaxBitByPosition (array2D: char [,]) =
    let rowSize = array2D.GetLength 0

    [ for row in 0 .. rowSize - 1 do
        yield
            array2D.[row, *]
            |> Seq.countBy id
            |> Seq.maxBy snd
            |> fst ]

let invertBinaryString binaryString =
    let invert char =
        match char with
        | '0' -> '1'
        | '1' -> '0'
        | _ -> failwith "Invalid value in BinaryString"

    binaryString
    |> Seq.map invert
    |> System.String.Concat

let extractDiagnostics (chars: char seq) =
    let binaryString = chars |> System.String.Concat
    let invertedBinaryString = binaryString |> invertBinaryString

    { GammaRate = System.Convert.ToInt32(binaryString, 2)
      EpsilonRate = System.Convert.ToInt32(invertedBinaryString, 2) }

let charToInt c = int c - int '0'

let ratingFor lifeSupportType (array: char [] []) =
    let rec LifeSupportCriteria bitIndex prevail (array: char [] []) =
        let bitCriteria =
            array
            |> array2DTranspose
            |> fun array2D -> array2D.[bitIndex, *]
            |> Seq.countBy id
            |> Seq.sortBy (fun (bit, occurences) -> occurences)
            |> List.ofSeq
            |> fun counts ->
                if snd counts.[0] = snd counts.[1] then
                    prevail
                else
                    fst counts.[(prevail|> charToInt )]

        let filteredArray =
            array
            |> Array.filter (fun x -> x.[bitIndex] = bitCriteria)

        match filteredArray.Length with
        | 1 -> filteredArray.[0] |> System.String.Concat |> fun x -> System.Convert.ToInt32(x, 2)
        | 0 -> failwith "No number found"
        | _ -> LifeSupportCriteria (bitIndex + 1) prevail filteredArray

    match lifeSupportType with
    | Oxygen -> LifeSupportCriteria 0 '1' array 
    | CO2 -> LifeSupportCriteria 0 '0' array

let lifeSupportRating (array: char [] []) =
    (ratingFor Oxygen array) * (ratingFor CO2 array)

let SolveDay3Part1 =
    inputs
    |> array2DTranspose
    |> getMaxBitByPosition
    |> extractDiagnostics
    |> fun d -> d.Consumption

let SolveDay3Part2 = 
    inputs 
    |> lifeSupportRating
