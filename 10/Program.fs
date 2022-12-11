
type Cmd =
    | Nop
    | Add of int

let ExeCmd (ticks,value) cmd =
    match cmd with
    | Nop   -> [ticks+1,value]
    | Add x -> [ticks+2,value+x;ticks+1,value]

let rec ExeCmds trace cmds =
    match cmds with
    | []    -> trace
    | x::xs -> let t = ExeCmd (List.head trace) x in ExeCmds (t@trace) xs

[<EntryPoint>]
let main args =

    let cmds =
        args.[0]
        |> System.IO.File.ReadAllLines
        |> Seq.map (fun s -> if s = "noop" then Nop else Add (int (s.Split(" ").[1])))
        |> List.ofSeq

    let trace = Map.ofSeq (ExeCmds [(0,1)] cmds)
    
    [ 20; 60; 100; 140; 180; 220 ]
    |> Seq.map (fun s -> trace.[s-1] * s) 
    |> Seq.sum
    |> printfn "Part 1: %A" 

    printfn "Part 2:"
    for r in 0 .. 5 do
        for c in 0 .. 39 do
            if abs(c-trace.[r*40+c]) <= 1 then printf "#"
                                          else printf "."
        printfn ""

    0
