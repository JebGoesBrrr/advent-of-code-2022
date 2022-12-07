
let Priority c =
    if System.Char.IsUpper c then (int c) - (int 'A') + 27
                             else (int c) - (int 'a') +  1

[<EntryPoint>]
let main args =
    
    let lines =
        args.[0]
        |> System.IO.File.ReadAllLines
    
    lines
    |> Seq.map (Seq.splitInto 2)
    |> Seq.map (Seq.map Set.ofSeq)
    |> Seq.map Set.intersectMany
    |> Seq.map Set.minElement
    |> Seq.map Priority 
    |> Seq.sum
    |> printfn "Part 1: %A"

    lines
    |> Seq.chunkBySize 3
    |> Seq.map (Seq.map Set.ofSeq)
    |> Seq.map Set.intersectMany
    |> Seq.map Set.minElement
    |> Seq.map Priority 
    |> Seq.sum
    |> printfn "Part 2: %A"
    0