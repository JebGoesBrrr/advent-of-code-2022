
let rec SplitBy state key source =
    match source with
    | [] -> if state = [] then [] else [state]
    | x::xs -> if x <> key  then SplitBy (state @ [x]) key xs
                            else (SplitBy state key []) @ (SplitBy [] key xs)

[<EntryPoint>]
let main args =

    let calorieList =
        args.[0]
        |> System.IO.File.ReadAllLines
        |> List.ofSeq
        |> SplitBy [] ""
        |> List.map (List.map int)
        |> List.map (List.sum)

    calorieList
    |> List.max
    |> printfn "Part 1: %A"

    calorieList
    |> List.sortDescending
    |> List.take 3
    |> List.sum
    |> printfn "Part 2: %A"

    0
