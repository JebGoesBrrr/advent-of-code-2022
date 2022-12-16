
type Monkey = {
    items      : int64 list
    divisor    : int64
    operator   : int64 -> int64
    tgtTrue    : int
    tgtFalse   : int
    numInspect : int64
} with

    static member Parse num (lines : string[]) =
        assert (num = (lines.[0].Replace("Monkey ", "").Replace(":", "") |> int))

        let ParseOp : int64 -> int64 =
            let opLine = lines.[2].Replace("  Operation: new = old ", "")
            if opLine.Contains("old") then fun x -> (if opLine.Contains("+") then (+) else (*)) x x
            else let opArg = (int64 opLine.[2..]) in if opLine.Contains("+") then ((+)opArg) else ((*)opArg)

        {
            items      = lines.[1].Replace("  Starting items: ","").Split(", ") |> Seq.map int64 |> Seq.toList
            divisor    = lines.[3].Replace("  Test: divisible by ", "") |> int64
            operator   = ParseOp
            tgtTrue    = lines.[4].Replace("    If true: throw to monkey ", "") |> int
            tgtFalse   = lines.[5].Replace("    If false: throw to monkey ", "") |> int
            numInspect = 0
        }

    static member Round p i (monkeys : Monkey[]) =
        for item in monkeys.[i].items do
            let newItem = let x = (monkeys.[i].operator item) in if p = 0L then x / 3L else x % p
            let tgt = if newItem % monkeys.[i].divisor = 0 then monkeys.[i].tgtTrue else monkeys.[i].tgtFalse
            monkeys.[tgt] <- { monkeys.[tgt] with items = monkeys.[tgt].items @ [ newItem ] }
        monkeys.[i] <- { monkeys.[i] with numInspect = monkeys.[i].numInspect + int64 monkeys.[i].items.Length; items = [] }

    static member Business p r (monkeys : Monkey[]) =
        for _ in 1 .. r do
            for i in 0 .. monkeys.Length-1 do
                Monkey.Round p i monkeys
    
        monkeys
        |> Seq.map (fun m -> m.numInspect)
        |> Seq.sortDescending
        |> Seq.take 2
        |> Seq.fold (*) 1L

[<EntryPoint>]
let main args =

    let monkeys =
        args.[0]
        |> System.IO.File.ReadAllLines
        |> Array.filter ((<>)"")
        |> Array.chunkBySize 6
        |> Array.mapi Monkey.Parse

    let monkeys2 = Array.copy monkeys
    let p = monkeys2 |> Seq.map (fun m -> m.divisor) |> Seq.fold (*) 1L

    printfn "Part 1: %A" (Monkey.Business 0L    20 monkeys)
    printfn "Part 2: %A" (Monkey.Business p  10000 monkeys2)

    0
