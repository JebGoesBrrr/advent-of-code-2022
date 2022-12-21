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

let NoGoSet s = seq {
    for x in s.pos.x-s.dist .. s.pos.x+s.dist do
        for y in s.pos.y-s.dist .. s.pos.y+s.dist do
            let p = { x=x; y=y }
            if (Dist p s.pos) <= s.dist then yield p
    }

[<EntryPoint>]
let main args =

    let yLine   = args.[0] |> int
    let sensors = args.[1] |> System.IO.File.ReadAllLines |> Array.map ParseLine

    let noGoSet = sensors |> Seq.collect NoGoSet |> Seq.filter (fun p -> p.y = yLine) |> Set.ofSeq
    let beaconsAt = sensors |> Seq.map (fun s -> s.beacon) |> Seq.filter (fun p -> p.y = yLine) |> Set.ofSeq

    printfn "Part 1: %A" (noGoSet.Count - beaconsAt.Count)

    0
