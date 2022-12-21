

[<EntryPoint>]
let main args =

    let input =
        args.[0]
        |> System.IO.File.ReadAllLines
        |> Array.map (fun s -> s.Split(" -> "))
        |> Array.map (Array.map (fun s-> s.Split(",")))
        |> Array.map (Array.map (fun a -> int a.[0], int a.[1]))
        |> Array.map (Array.pairwise)

    let Line ((x0,y0), (x1,y1)) = seq {
        for x in (min x0 x1) .. (max x0 x1) do
            for y in (min y0 y1) .. (max y0 y1) do
                yield x,y
    }

    let blocks = input |> Seq.collect (Seq.collect Line) |> Set.ofSeq

    let xmin = blocks |> Set.map fst |> Set.minElement
    let xmax = blocks |> Set.map fst |> Set.maxElement
    let ymin = blocks |> Set.map snd |> Set.minElement
    let ymax = blocks |> Set.map snd |> Set.maxElement

(*
    for y in ymin .. ymax do
        for x in xmin .. xmax do
            printf "%c" (if Set.contains (x,y) blocks then '#' else '.')
        printfn ""
*)

    let Part1 =
        let mutable sand = Set.empty

        let SimulateSand() =
            let mutable current = 500,0

            let TestPoints (x,y) = [| x,y+1; x-1,y+1; x+1,y+1 |]
            let IsBlocked (x,y)  = (Set.contains (x,y) blocks) || (Set.contains (x,y) sand)
            let NotBound (_,y)   = y > ymax

            let mutable settled  = false
            let mutable freeFall = false

            while not settled && not freeFall do
                let newCurrent = (TestPoints current) |> Seq.tryFind (IsBlocked >> not) 

                if newCurrent.IsSome then
                    current <- newCurrent.Value
                    if NotBound current then
                        freeFall <- true
                else
                    settled <- true

            if not freeFall then sand <- Set.add current sand; true else false

        let ticks = seq { while true do yield SimulateSand() }

        ticks
        |> Seq.takeWhile id
        |> Seq.length
        |> printfn "Part 1: %A"

    let Part2 =
            let mutable sand = Set.empty

            let SimulateSand() =
                let mutable current = 500,0

                let TestPoints (x,y) = [| x,y+1; x-1,y+1; x+1,y+1 |]
                let IsBlocked (x,y)  = (Set.contains (x,y) blocks) || (Set.contains (x,y) sand) || y = ymax + 2

                let mutable settled  = false

                while not settled do
                    let newCurrent = (TestPoints current) |> Seq.tryFind (IsBlocked >> not)

                    if newCurrent.IsSome then
                        current <- newCurrent.Value
                    else
                        settled <- true

                sand <- Set.add current sand
                if current = (500,0) then false else true

            let ticks = seq { while true do yield SimulateSand() }

            ticks
            |> Seq.takeWhile id
            |> Seq.length
            |> (+)1
            |> printfn "Part 2: %A"

    0
