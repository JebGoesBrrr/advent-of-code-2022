

[<EntryPoint>]
let main args =

    let input = System.IO.File.ReadAllText args.[0]

    let markerPos n = seq {
        for i in 0 .. input.Length-n do
            if (input.[i..i+n-1] |> Set.ofSeq |> Set.count) = n then
                yield i + n
    }

    printfn "Part 1: %A" (Seq.head (markerPos  4))
    printfn "Part 2: %A" (Seq.head (markerPos 14))

    0