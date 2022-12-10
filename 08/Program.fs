
[<EntryPoint>]
let main args =

    let input = System.IO.File.ReadAllLines args.[0]
    let trees = Array2D.init input.Length input.[0].Length (fun x y -> (int input.[x].[y]) - (int '0') )

    Seq.allPairs (seq { 0 .. (Array2D.length1 trees)-1}) (seq { 0 .. (Array2D.length2 trees)-1})
    |> Seq.map (fun (x,y) ->
        [ trees.[x,..y-1]; trees.[x,y+1..]; trees.[..x-1,y]; trees.[x+1..,y] ]
        |> Seq.map (Seq.forall ((>)trees.[x,y]))
        |> Seq.contains true)
    |> Seq.filter id
    |> Seq.length
    |> printfn "Part 1: %A"

    Seq.allPairs (seq { 0 .. (Array2D.length1 trees)-1}) (seq { 0 .. (Array2D.length2 trees)-1})
    |> Seq.map (fun (x,y) ->
        [ Array.rev trees.[x,..y-1]; trees.[x,y+1..]; Array.rev trees.[..x-1,y]; trees.[x+1..,y] ]
        |> Seq.map (fun s -> let i = Array.tryFindIndex ((<=)trees.[x,y]) s in if Option.isSome i then (Option.get i)+1 else Array.length s)
        |> Seq.fold (*) 1)
    |> Seq.max
    |> printfn "Part 2: %A"

    0
