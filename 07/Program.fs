open System.Text.RegularExpressions


let (|IsCdDir|_|) line =
    let m = Regex(@"cd ([a-z]+)").Match line
    if m.Success then Some (m.Groups.Item 1).Value
                 else None

let (|IsCdUp|_|) line =
    if line = "$ cd .." then Some true
                        else None

let (|IsFile|_|) line =
    let m = Regex(@"([0-9]+) ([a-z,.]+)").Match line
    if m.Success then Some( (int (m.Groups.Item 1).Value), (m.Groups.Item 2).Value)
                 else None


type File = {
    name : string
    size : int
}

type Dir = {
    name  : string
    files : File list
    dirs  : Dir list
    size  : int
}
with
    static member MakeNew name =
        { name = name; files = []; dirs = []; size = 0 }

    static member AddFile name size dir =
        { dir with files = { name = name; size = size } :: dir.files }

    static member AddDir newDir dir =
        { dir with dirs = newDir :: dir.dirs }

    static member BuildTree lines dir =
        match lines with
        | []               -> dir, []
        | IsCdUp _::xs     -> dir, xs
        | IsFile (s,f)::xs -> Dir.BuildTree xs (Dir.AddFile f s dir)
        | IsCdDir d::xs    -> let newdir,ys = Dir.BuildTree xs (Dir.MakeNew d) in Dir.BuildTree ys (Dir.AddDir newdir dir)
        | _::xs            -> Dir.BuildTree xs dir

    static member ComputeSize dir =
        let fsize = dir.files |> Seq.map (fun f -> f.size) |> Seq.sum
        let newdirs = List.map Dir.ComputeSize dir.dirs
        let dsize = newdirs |> Seq.map (fun d -> d.size) |> Seq.sum
        { dir with dirs = newdirs; size = fsize + dsize }

    static member AllDirs pred dir =
        let head = if pred dir then [dir] else []
        head @ (List.concat (Seq.map (fun d -> Dir.AllDirs pred d) dir.dirs))

    static member Print indent dir =
        for _ in 0 .. indent do printf " "
        printfn "- %s" dir.name
        for f in dir.files do
            for _ in 0 .. indent+2 do printf " "
            printfn "%s (%i)" f.name f.size
        for d in dir.dirs do
            Dir.Print (indent+2) d

[<EntryPoint>]
let main args =

    let lines = System.IO.File.ReadAllLines args.[0] |> List.ofArray

    let dir = let d,_ = Dir.BuildTree (List.tail lines) (Dir.MakeNew "/") in Dir.ComputeSize d
    // Dir.Print 0 dir

    let dirs100k = Dir.AllDirs (fun d -> d.size <= 100000) dir
    printfn "Part 1: %A" (Seq.sum (Seq.map (fun d -> d.size) dirs100k))

    let requiredSpace = 30000000 - (70000000 - dir.size)
    let dirsEnough = Dir.AllDirs (fun d -> d.size >= requiredSpace) dir
    printfn "Part 2: %A" (Seq.min (Seq.map (fun d -> d.size) dirsEnough))

    0
