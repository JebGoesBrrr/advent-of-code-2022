open System.Text.RegularExpressions

let ParseStack i (lines : string[]) =
    let x = i * 4 + 1
    lines
    |> Seq.rev
    |> Seq.map (fun l -> if l.[x] = ' ' then None else Some l.[x])
    |> Seq.filter Option.isSome
    |> Seq.map Option.get
    |> Seq.rev
    |> Seq.toList

let ParseStacks (lines : string[]) =
    seq {
        for i in 0 .. lines.[0].Length / 4 do
            yield ParseStack i lines.[0..lines.Length-2]
    }
    |> Seq.toArray

let ParseCommand line =
    (Regex(@"move ([0-9]+) from ([0-9]+) to ([0-9]+)").Match line).Groups
    |> Seq.tail
    |> Seq.map (fun g -> int g.Value)
    |> Seq.toArray
    |> fun x -> x.[0], x.[1], x.[2]

let rec ExecuteCommand (stacks : char list[]) (n,f,t) =
    if n <> 0 then do
        stacks.[t-1] <- List.head stacks.[f-1] :: stacks.[t-1]
        stacks.[f-1] <- List.tail stacks.[f-1]
        ExecuteCommand stacks (n-1,f,t)

let ExecuteCommand2 (stacks : char list[]) (n,f,t) =
    do
        let x,y = List.splitAt n stacks.[f-1]
        stacks.[t-1] <- x @ stacks.[t-1]
        stacks.[f-1] <- y


[<EntryPoint>]
let main args =

    let input =
        args.[0]
        |> System.IO.File.ReadAllLines
        |> fun x -> Array.splitAt (Array.findIndex ((=)"") x) x

    let stacks = ParseStacks (fst input)
    let cmds   = Array.map ParseCommand (snd input).[1..]
    
    Array.iter (fun cmd -> ExecuteCommand stacks cmd) cmds
    let heads = System.String (Array.map List.head stacks)
    printfn "Part 1: %s" heads
  

    let stacks = ParseStacks (fst input)

    Array.iter (fun cmd -> ExecuteCommand2 stacks cmd) cmds
    let heads = System.String (Array.map List.head stacks)
    printfn "Part 2: %s" heads
    0