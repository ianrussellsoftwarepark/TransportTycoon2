open System.IO

type Tree<'T> =
    |Branch of 'T * Tree<'T> seq
    |Leaf of 'T

type Waypoint = { Location:string; Route:string list; Distance:int }

type Connection = { From:string; To:string; Distance:int }

let lines = 
    Path.Combine(__SOURCE_DIRECTORY__, "resources", "data.csv")
    |> File.ReadAllText
    |> fun data -> data.Split(System.Environment.NewLine)
    |> Array.skip 1

let routes = [
    for line in lines do
        match line.Split(",") with
        | [|A; B; Km|] -> 
            { From = A.Trim(); To = B.Trim(); Distance = int Km }
            { From = B.Trim(); To = A.Trim(); Distance = int Km }
        | _ -> ()
]

let generateChildren (wayPoint:Waypoint) =
    routes
    |> List.filter (fun x -> x.From = wayPoint.Location)
    |> List.filter (fun x -> wayPoint.Route |> List.tryFind (fun z -> z = x.To) = None)
    |> List.map (fun x -> { Location = x.To; Route = x.From::wayPoint.Route; Distance = x.Distance + wayPoint.Distance })

let hasChildren wayPoint = 
    generateChildren wayPoint
    |> fun x -> x <> List.empty

let rec createTree hasChildren generateChildren finish (current:Waypoint) =
    let generateTree = createTree hasChildren generateChildren
    match hasChildren current && current.Location <> finish with
    | true -> Branch(current, seq{for next in generateChildren current do yield (generateTree finish next)})
    | false -> Leaf(current)

let findRoute start finish =
    createTree hasChildren generateChildren finish { Location = start; Route = []; Distance = 0}

let rec treeToList tree =
    match tree with 
    | Leaf x -> [x]
    | Branch (x, xs) -> x :: (List.collect treeToList (xs |> Seq.toList))

let selectShortest finish lst =
    lst
    |> List.filter (fun x -> x.Location = finish)
    |> List.map (fun x -> x.Location::x.Route |> List.rev, x.Distance)
    |> List.minBy snd

[<EntryPoint>]
let main argv =
    findRoute argv.[0] argv.[1]
    |> treeToList
    |> selectShortest argv.[1]
    |> fun (x, _) -> printfn "%A" (x |> List.reduce (fun acc z -> acc + "," + z))
    0
    