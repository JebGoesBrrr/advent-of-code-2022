
[<EntryPoint>]
let main args =

    let lines = System.IO.File.ReadAllLines args.[0]

    let mutable s = 0,0
    let mutable e = 0,0

    let Char2Height x y =
        let c = lines.[x].[y]
        if System.Char.IsUpper c then
            match c with
            | 'S' -> s <- x,y;  0
            | 'E' -> e <- x,y; 25
            | _   -> failwith ""
        else
            (int c) - (int 'a')

    let field = Array2D.init   lines.Length lines.[0].Length Char2Height
    let steps = Array2D.create lines.Length lines.[0].Length System.Int32.MaxValue

    let rec UpdateSteps x y s =
        if s < steps.[x,y] then
            steps.[x,y] <- s
            for dx,dy in [ x-1,y; x+1,y; x,y-1; x,y+1 ] do
                if dx >= 0 && dx < (Array2D.length1 field) && dy >= 0 && dy < (Array2D.length2 field) then
                    if field.[dx,dy] >= field.[x,y] - 1 then
                        UpdateSteps dx dy (s+1)
 
    UpdateSteps (fst e) (snd e) 0

    printfn "Part 1: %A" steps.[fst s, snd s]

    Seq.allPairs (seq { 0 .. lines.Length-1 }) (seq { 0 .. lines.[0].Length-1 })
    |> Seq.filter (fun (x,y) -> field.[x,y] = 0)
    |> Seq.map (fun (x,y) -> steps.[x,y])
    |> Seq.min
    |> printfn "Part 2: %A"

    0
