module Year2021Day3

open System.IO

let inputs =
    File.ReadAllLines("inputs/year2021day3.txt")
    |> Array.map Array.ofSeq

type Diagnostics =
    { GammaRate: int
      EpsilonRate: int }

    member this.Consumption = this.EpsilonRate * this.GammaRate

let toTransposedArray2D (array: char [] []) =
    let x = array.[0].Length
    let y = array.Length

    Array2D.init x y (fun x y -> array.[y].[x])

let rec OxygenGeneratorCriteria (position: int) (array: char [] []) =
    let bitCriteria =
        array
        |> toTransposedArray2D
        |> fun array2D -> array2D.[position, *]
        |> Seq.countBy id
        |> Seq.sortByDescending (fun (bit, occurences) -> occurences)
        |> List.ofSeq
        |> fun counts ->
            if snd counts.[0] = snd counts.[1] then
                '1'
            else
                fst counts.[0]

    let filteredArray =
        array
        |> Array.filter (fun x -> x.[position] = bitCriteria)

    match filteredArray.Length with
    | 1 -> filteredArray.[0] |> System.String.Concat |> fun x -> System.Convert.ToInt32(x, 2)
    | 0 -> failwith "No number found"
    | _ -> OxygenGeneratorCriteria (position + 1) filteredArray


let rec CO2ScrubberRating (position: int) (array: char [] []) =
    let bitCriteria =
        array
        |> toTransposedArray2D
        |> fun array2D -> array2D.[position, *]
        |> Seq.countBy id
        |> Seq.sortBy (fun (bit, occurences) -> occurences)
        |> List.ofSeq
        |> fun counts ->
            if snd counts.[0] = snd counts.[1] then
                '0'
            else
                fst counts.[0]

    let filteredArray =
        array
        |> Array.filter (fun x -> x.[position] = bitCriteria)

    match filteredArray.Length with
    | 1 -> filteredArray.[0] |> System.String.Concat |> fun x -> System.Convert.ToInt32(x, 2) //|> fun x -> System.Convert.ToInt32(x, 2)
    | 0 -> failwith "No number found"
    | _ -> CO2ScrubberRating (position + 1) filteredArray


let lifeSupportRating (array: char [] []) =
    (OxygenGeneratorCriteria 0 array) * (CO2ScrubberRating 0 array)

let getMaxBitByPosition (array2D: char [,]) =
    [ let rowSize = array2D.GetLength 0

      for row in 0 .. rowSize - 1 do
          yield
              array2D.[row, *]
              |> Seq.countBy id
              |> Seq.maxBy (fun (bit, occurences) -> occurences)
              |> fun (bit, occurences) -> bit ]

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

let SolveDay3Part1 =
    inputs
    |> toTransposedArray2D
    |> getMaxBitByPosition
    |> extractDiagnostics
    |> fun d -> d.Consumption

let SolveDay3Part2 = 
    inputs 
    |> lifeSupportRating
