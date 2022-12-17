
type Packet =
    | Int  of int
    | List of Packet list

let (|IsInt|_|) (token : string) =
    try Some (int token)
    with _ -> None

let rec ParseList (tokens : string list) =
    match tokens with
    | []            -> [], tokens
    | "]"::xs       -> [], xs
    | IsInt i :: xs -> let l,ys = ParseList xs in (Int i) :: l, ys
    | "["::xs       -> let p,ys = ParseList xs in let l,zs = ParseList ys in (List p) :: l, zs
    | x             -> failwith (sprintf "%A" x)

let ParsePacket (tokens : string list) =
    tokens |> ParseList |> fst |> List.head

let rec SplitBrackets (tokens : string list) =
    match tokens with
    | []      -> []
    | "["::xs -> "[" :: (SplitBrackets xs)
    | "]"::xs -> "]" :: (SplitBrackets xs)
    |  x ::xs -> if   x.StartsWith("[") then "[" :: (SplitBrackets (x.[1..] :: xs))
                 elif x.EndsWith("]")   then        (SplitBrackets (x.[..x.Length-2] :: "]" :: xs))
                                        else  x  :: (SplitBrackets xs)

let rec ComparePacket p1 p2 =
    match p1,p2 with
    | Int i1, Int i2 -> if i1 < i2 then -1 elif i2 < i1 then 1 else 0
    | List _, Int  _ -> ComparePacket p1 (List [p2])
    | Int  _, List _ -> ComparePacket (List [p1]) p2
    | List [], _     -> -1
    | _, List []     ->  1
    | List (x::xs), List (y::ys) -> let c = ComparePacket x y in if c = 0 then ComparePacket (List xs) (List ys) else c

let rec Sort l =
    let rec Insert i l =
        match l with
        | [] -> [i]
        | x::xs -> if (ComparePacket i x) <= 0 then i::x::xs else x::(Insert i xs)
    match l with
    | []    -> []
    | x::xs -> Insert x (Sort xs)

[<EntryPoint>]
let main args =

    let packets =
        args.[0]
        |> System.IO.File.ReadAllLines
        |> Seq.chunkBySize 3
        |> Seq.map (fun a -> List.ofArray (a.[0].Split(",")) , List.ofArray (a.[1].Split(",")) )
        |> Seq.map (fun (x,y) -> SplitBrackets x, SplitBrackets y)
        |> Seq.map (fun (x,y) -> ParsePacket x, ParsePacket y)
        |> Seq.toArray

    packets
    |> Seq.map (fun (x,y) -> ComparePacket x y)
    |> Seq.indexed
    |> Seq.filter (snd >> ((=)(-1)))
    |> Seq.map (fst >> ((+)1))
    |> Seq.sum
    |> printfn "Part 1: %A"

    let div1 = List [List [Int 2]]
    let div2 = List [List [Int 6]]

    let allPackets =
        packets
        |> Array.collect (fun x -> [|fst x; snd x|]) 
        |> Array.append [| div1; div2 |]
        |> List.ofArray
        |> Sort // lol F# wat?!
        |> Seq.toArray

    let i1 = 1 + Array.findIndex ((=)div1) allPackets
    let i2 = 1 + Array.findIndex ((=)div2) allPackets

    printfn "Part 2: %A" (i1 * i2)

    0