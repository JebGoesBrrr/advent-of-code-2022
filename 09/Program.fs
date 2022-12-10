
let ParseLine (line : string) =
    let n = int line[2..]
    match line[0] with
    | 'R' -> Seq.replicate n ( 0, 1)
    | 'L' -> Seq.replicate n ( 0,-1)
    | 'U' -> Seq.replicate n (-1, 0)
    | 'D' -> Seq.replicate n ( 1, 0)
    | _ -> failwith ""

let CatchUp (hx,hy) (tx,ty) =
    let dx,dy = hx-tx,hy-ty in if abs(dx) <= 1 && abs(dy) <= 1 then tx,ty else (tx + dx / if dx = 0 then 1 else abs(dx)), (ty + dy / if dy = 0 then 1 else abs(dy))

let MakeStep (trace,(hx,hy),(tails:(int*int)[])) (x,y) =
    let nhx,nhy = hx+x,hy+y
    tails.[0] <- CatchUp (nhx,nhy) tails.[0]
    for i in 1 .. tails.Length-1 do
        tails.[i] <- CatchUp tails.[i-1] tails.[i]
    (Map.add tails.[tails.Length-1] (if Map.containsKey tails.[tails.Length-1] trace then trace.[tails.[tails.Length-1]]+1 else 1) trace),(nhx,nhy),tails

[<EntryPoint>]
let main args =

    let input = args.[0] |> System.IO.File.ReadAllLines |> Seq.map ParseLine |> Seq.concat |> Array.ofSeq

    let initial = (Map.ofList [((0,0),1)], (0,0), [|(0,0)|])
    let trace,_,_ = Seq.fold MakeStep initial input
    printfn "Part 1: %A" trace.Count

    let initial = (Map.ofList [((0,0),1)], (0,0), Array.replicate 9 (0,0))
    let trace,_,_ = Seq.fold MakeStep initial input
    printfn "Part 2: %A" trace.Count

    0
