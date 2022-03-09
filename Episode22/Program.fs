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
        | [|start; finish; distance; speed|] -> 
            { From = start; To = finish; Distance = int distance; Speed = int speed }
            { From = finish; To = start; Distance = int distance; Speed = int speed }
        | _ -> ()
]

let generateChildren (wayPoint:Waypoint) =
    myData
    |> List.filter (fun cn -> cn.From = wayPoint.Location && wayPoint.Route |> List.tryFind (fun loc -> fst loc = cn.To) = None)
    |> List.map (fun cn -> 
        let duration = decimal cn.Distance / decimal cn.Speed
        { Location = cn.To; Route = (cn.From, wayPoint.Duration) :: wayPoint.Route; Duration = duration + wayPoint.Duration })

let hasChildren wayPoint = 
    generateChildren wayPoint
    |> List.isEmpty
    |> not

let rec createTree hasChildren generateChildren finish (current:Waypoint) =
    let generateTree = createTree hasChildren generateChildren
    if hasChildren current && current.Location <> finish then
        Branch (current, seq { for next in generateChildren current do yield (generateTree finish next) })
    else Leaf current

let findRoute start finish =
    createTree hasChildren generateChildren finish { Location = start; Route = []; Duration = 0M }

let rec treeToList tree =
    match tree with 
    | Leaf x -> [x]
    | Branch (x, xs) -> x :: (List.collect treeToList (xs |> Seq.toList))

let getFastest finish lst =
    lst
    |> List.filter (fun x -> x.Location = finish)
    |> List.minBy (fun x -> x.Duration)

let prepareOutput start waypoint =
    waypoint
    |> fun wp -> (wp.Location, wp.Duration) :: wp.Route |> List.rev |> List.tail
    |> List.fold (fun acc (loc,time) -> fst acc + "\n" + $"%.2f{time}h  ARRIVE  {loc}", 0M) ($"00.00h  DEPART  {start}", 0M)    

[<EntryPoint>]
let main argv =
    findRoute argv.[0] argv.[1]
    |> treeToList
    |> getFastest argv.[1]
    |> prepareOutput argv.[0]   
    |> fun (output, _) -> printfn "%s" output
    0
    