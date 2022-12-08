
let Score (a, b) =
    if abs(b - a) > 1 then (b+2) % 3 - (a+2) % 3 else b - a

let Outcome (a, b) =
    (a + ([|2;0;1|].[b])) % 3

[<EntryPoint>]
let main args =
    
    let input =
        args.[0]
        |> System.IO.File.ReadAllLines
        |> Array.map (fun s -> s.[0],s.[2])
        |> Array.map (fun (a,b) -> (int a) - (int 'A'), (int b) - (int 'X'))

    input
    |> Array.map (fun x -> snd x, Score x)
    |> Array.map (fun (x,y) -> x + 1 + (y+1) * 3)
    |> Array.sum
    |> printfn "Part 1: %A"

    input
    |> Array.map (fun x -> snd x, Outcome x)
    |> Array.map (fun (x,y) -> y + 1 + x * 3)
    |> Array.sum
    |> printfn "Part 2: %A"

    0