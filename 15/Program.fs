open System.Text.RegularExpressions

type Point  = { x : int; y : int }
type Sensor = { pos : Point; beacon : Point; dist : int }

let Dist a b = abs(a.x-b.x) + abs(a.y-b.y)

let ParseLine line =
    let a =
        Regex(@"Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)").Match(line).Groups.Values
        |> Seq.tail |> Seq.map (fun g -> int g.Value) |> Seq.toArray

    let p, b = { x=a.[0]; y=a.[1] }, { x=a.[2]; y=a.[3] }
    { pos = p; beacon = b; dist = Dist p b }

let NoGoSet y s = seq {
    let d = (max 0 (s.dist-(abs s.pos.y-y)))
    for x in s.pos.x-d .. s.pos.x+d do
        let p = { x=x; y=y }
        if (Dist p s.pos) <= s.dist then yield p
    }

let rec SquareSolution origin size sensors =

    let a = origin
    let b = { x=origin.x+size.x-1; y=origin.y }
    let c = { x=origin.x; y=origin.y+size.y-1 }
    let d = { x=origin.x+size.x-1; y=origin.y+size.y-1 }

    let TestPoint p s = Dist s.pos p <= s.dist
    let TestSquare s = TestPoint a s && TestPoint b s && TestPoint c s && TestPoint d s

    if sensors |> Seq.map TestSquare |> Seq.contains true then
        []
    elif size.x <= 1 && size.y <= 1 then
        if sensors |> Seq.map (TestPoint origin) |> Seq.contains true then
            []
        else
            [ origin ]
    else
        let a, asize = origin, { x=size.x/2; y=size.y/2 }
        let b, bsize = {x=origin.x; y=origin.y+size.y/2}, { x=size.x/2; y=size.y-size.y/2 }
        let c, csize = {x=origin.x+size.x/2; y=origin.y}, { x=size.x-size.x/2; y=size.y/2 }
        let d, dsize = {x=origin.x+size.x/2; y=origin.y+size.y/2}, { x=size.x-size.x/2; y=size.y-size.y/2 }    
        (SquareSolution a asize sensors) @
        (SquareSolution b bsize sensors) @
        (SquareSolution c csize sensors) @
        (SquareSolution d dsize sensors)


[<EntryPoint>]
let main args =

    let yLine   = args.[0] |> int
    let size    = args.[1] |> int
    let sensors = args.[2] |> System.IO.File.ReadAllLines |> Array.map ParseLine

    let noGoSet = sensors |> Seq.collect (NoGoSet yLine) |> Seq.filter (fun p -> p.y = yLine) |> Set.ofSeq
    let beaconsAt = sensors |> Seq.map (fun s -> s.beacon) |> Seq.filter (fun p -> p.y = yLine) |> Set.ofSeq

    printfn "Part 1: %A" (noGoSet.Count - beaconsAt.Count)

    let p = (SquareSolution {x=0;y=0} {x=size+1;y=size+1} sensors) |> Set.ofSeq |> Set.minElement
    printfn "Part 2: %A"  ((int64 p.x) * 4000000L + (int64 p.y))

    0
