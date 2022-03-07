open System.IO

type Tree<'T> =
    |Branch of 'T * Tree<'T> seq
    |Leaf of 'T

type Waypoint = { Location:string; Route:(string * decimal) list; Duration:decimal }

type Connection = { From:string; To:string; Distance:int; Speed:int }

let lines = 
    Path.Combine(__SOURCE_DIRECTORY__, "resources", "data.csv")
    |> File.ReadAllText
    |> fun data -> data.Split(System.Environment.NewLine)
    |> Array.skip 1

let myData = [
    for line in lines do
        match line.Split(",") with
        | [|A; B; Km; speed|] -> 
            { From = A; To = B; Distance = int Km; Speed = int speed }
            { From = B; To = A; Distance = int Km; Speed = int speed }
        | _ -> ()
]

let generateChildren (wayPoint:Waypoint) =
    myData
    |> List.filter (fun x -> x.From = wayPoint.Location)
    |> List.filter (fun x -> wayPoint.Route |> List.tryFind (fun z -> fst z = x.To) = None)
    |> List.map (fun x -> 
        let duration = decimal x.Distance / decimal x.Speed
        { Location = x.To; Route = (x.From, wayPoint.Duration)::wayPoint.Route; Duration = duration + wayPoint.Duration })

let hasChildren wayPoint = 
    generateChildren wayPoint
    |> List.isEmpty
    |> not

let rec createTree hasChildren generateChildren finish (current:Waypoint) =
    let generateTree = createTree hasChildren generateChildren
    match hasChildren current && current.Location <> finish with
    | true -> Branch(current, seq{for next in generateChildren current do yield (generateTree finish next)})
    | false -> Leaf(current)

let findRoute start finish =
    createTree hasChildren generateChildren finish { Location = start; Route = []; Duration = 0M }

let rec treeToList tree =
    match tree with 
    | Leaf x -> [x]
    | Branch (x, xs) -> x :: (List.collect treeToList (xs |> Seq.toList))

[<EntryPoint>]
let main argv =
    findRoute argv.[0] argv.[1]
    |> treeToList
    |> List.filter (fun x -> x.Location = argv.[1])
    |> List.minBy (fun x -> x.Duration)
    |> fun x -> (x.Location, x.Duration)::x.Route |> List.rev |> List.tail
    |> List.fold (fun acc (loc,time) -> fst acc + "\n" + $"%.2f{time}h  ARRIVE  {loc}", 0M) ($"00.00h  DEPART  {argv.[0]}", 0M)    
    |> fun x -> printfn "%s" (fst x)
    0
    