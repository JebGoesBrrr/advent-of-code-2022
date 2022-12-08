
let Contains ((a0,b0),(a1,b1)) =
    (a0 >= a1 && b0 <= b1) || (a0 <= a1 && b0 >= b1)

let Overlap ((a0,b0),(a1,b1)) =
    (b0 >= a1 && a0 < b1) || (b1 >= a0 && a1 < b0)

[<EntryPoint>]
let main args =
    
    let input =
        args.[0]
        |> System.IO.File.ReadAllLines
        |> Array.map (fun s -> let x = s.Split "," in [|x.[0].Trim(); x.[1].Trim() |] )
        |> Array.map (Array.map (fun s -> let x = s.Split "-" in int x.[0], int x.[1]))
        |> Array.map (fun x -> x.[0], x[1])  

    input
    |> Array.filter Contains
    |> Array.length
    |> printfn "Part 1: %A"
    
    input
    |> Array.filter Overlap
    |> Array.length
    |> printfn "Part 2: %A"

    0