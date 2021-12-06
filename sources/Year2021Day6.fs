module Year2021Day6

open System.IO

let inputs =
    File.ReadAllText("inputs/year2021day6.txt").Split(',')
    |> List.ofArray 
    |> List.map (int)

let allGeneration fishes = 
    [ for i = 0 to 8 do
        match fishes |> List.tryFind (fun (timer,_) -> timer = i) with
        | Some (_, amount) -> yield (i, (int64)amount)
        | None -> yield (i, (int64)0)
    ]

let initialFishes = 
    inputs |> List.countBy id

let addDay fishes =
    let nextDayFishes =
        fishes 
        |> List.map (fun (timer,counter) -> ((if timer = 0 then 8 else timer - 1), counter))
    
    let (_,newFishesCount) = 
        nextDayFishes 
        |> List.find (fun (timer,_) -> timer = 8)
    
    nextDayFishes 
    |> List.map (fun (timer, counter) ->
        if timer = 6 then 
            (timer, counter + newFishesCount) //Bring new parent back to 6
        else
            (timer, counter)
        )

let rec addDays days fishes =
    if days = 0 then 
        fishes
    else
        addDays (days - 1) (addDay fishes)

let countFishes fishes = 
    fishes 
    |> List.map ( fun (_,counter) -> (int64)counter)
    |> List.reduce (+)


let SolveDay6Part1 = 
    initialFishes
    |> allGeneration
    |> addDays 80
    |> countFishes

    

let SolveDay6Part2 = 
    initialFishes
    |> allGeneration
    |> addDays 256
    |> countFishes
    