open System.Collections.Immutable
open System.Collections.Generic
open System.Linq

type Graph = ImmutableArray<ImmutableArray<char>>

type Node = {
  X: int
  Y: int
  Value: char
  Elevation: int
}

type Edge = {
  FromNode: Node
  ToNode: Node
  Cost: int
}

module Graph =
  let toString (g: Graph) =
    g
    |> Seq.fold (
        fun (sb: System.Text.StringBuilder) row ->
            row
            |> Seq.fold (
                fun (sb: System.Text.StringBuilder) c ->
                  sb.Append(c)) (sb.AppendLine())) (System.Text.StringBuilder())
    |> (fun sb -> sb.ToString().Trim())

  let height (g: Graph) = g.Length
  let width (g: Graph) = g[0].Length

  let getNode (x, y) (g: Graph) =
    let value = g[y][x]

    let elevation =
      value
      |> function
        | v when v = 'S' -> 'a'
        | v when v = 'E' -> 'z'
        | v -> v
      |> (fun v -> (int v) - (int 'a'))

    { X = x; Y = y; Value = value; Elevation = elevation }

  let private findFirstNode (value) (g: Graph) =
    let row = g |> Seq.findIndex (fun row -> row.Contains(value))
    let col = g[row] |> Seq.findIndex (fun col -> col = value)
    getNode (col, row) g

  let findStartNode = findFirstNode 'S'
  let findEndNode = findFirstNode 'E'

  let private nodesAreAdjacent a b =
    ((a.X = b.X) && (System.Math.Abs(a.Y - b.Y) = 1)) ||
    ((a.Y = b.Y) && (System.Math.Abs(a.X - b.X) = 1))

  let private getEdgeBetween g fromNode toNode =
    if nodesAreAdjacent fromNode toNode then
      let cost = toNode.Elevation - fromNode.Elevation
      if cost <= 1 then
        Some { FromNode = fromNode; ToNode = toNode; Cost = cost}
      else
        None
    else
      None

  let findNeighbors node (g: Graph) =
    let (x, y) = (node.X, node.Y)
    let (height, width) = ((height g), (width g))


    [
      if (x - 1 >= 0) then (x - 1, y)
      if (x + 1 < width) then (x + 1, y)
      if (y - 1 >= 0) then (x, y - 1)
      if (y + 1 < height) then (x, y + 1)
    ]
    |> List.map (fun coord -> getNode coord g)
    |> List.choose (getEdgeBetween g node)

  let allNodes (g: Graph) =
    let (width, height) = ((width g), (height g))

    [
      for y = 0 to (height - 1) do
        for x = 0 to (width - 1) do
          getNode (x, y) g
    ]

  let allVertices (g: Graph) =
    g
    |> allNodes
    |> List.collect (fun fromNode -> findNeighbors fromNode g)

  let findShortestPath (costFunction: Edge -> int) startNode destinationNode g =
    let h node = 0

    let inf = System.Int32.MaxValue

    let openSet = Set.empty |> Set.add startNode

    let cameFrom: Map<Node, Node> = Map.empty
    let gScore = Map.empty |> Map.add startNode 0
    let fScore = Map.empty |> Map.add startNode (h startNode)

    let defaultInfinity key map = map |> Map.tryFind key |> Option.defaultValue inf

    let reconstructPath (cameFrom: Map<Node, Node>) current =
      let rec inner totalPath current =
        if Map.containsKey current cameFrom then
          let nextCurrent = cameFrom |> Map.find current
          inner (nextCurrent::totalPath) nextCurrent
        else
          totalPath

      inner [ current ] current


    let rec inner openSet cameFrom gScore fScore =
      if Set.isEmpty openSet then
        []
      else
        let current =
          openSet
          |> Seq.sortBy (fun n -> defaultInfinity n fScore)
          |> Seq.head

        let openSet = openSet |> Set.remove current

        if current = destinationNode then
          reconstructPath cameFrom current
        else
          let neighborsToAdd =
            findNeighbors current g
            |> List.map (fun n ->
                let current_gScore = gScore |> defaultInfinity current

                (current_gScore + (costFunction n), n))
            |> List.filter(fun (tentativeScore, n) ->
                  let neighbor_g_score = gScore |> Map.tryFind n.ToNode |> Option.defaultValue inf
                  tentativeScore < neighbor_g_score)

          let (cameFrom, gScore, fScore, openSet) =
            neighborsToAdd
            |> List.fold (
                fun (cameFrom, gScore, fScore, openSet) (tentativeScore, n) ->
                  let cameFrom = cameFrom |> Map.add n.ToNode current
                  let gScore = gScore |> Map.add n.ToNode tentativeScore
                  let fScore = fScore |> Map.add n.ToNode (tentativeScore + (h n.ToNode))

                  let openSet = openSet |> Set.add n.ToNode

                  (cameFrom, gScore, fScore, openSet)) (cameFrom, gScore, fScore, openSet)

          inner openSet cameFrom gScore fScore

    inner openSet cameFrom gScore fScore



let parseInput (lines: string array) =
  lines
  |> Array.fold (fun (a: Graph) l ->
      let nextRow = ImmutableArray<char>.Empty.AddRange(l)
      a.Add(nextRow)) ImmutableArray<ImmutableArray<char>>.Empty

//let inputPath = "day12.test.txt"
let inputPath = "day12.real.txt"

let input = System.IO.File.ReadAllLines(inputPath) |> parseInput

let startNode = input |> Graph.findStartNode
let endNode = input |> Graph.findEndNode

// Part 1
input
|> Graph.findShortestPath (fun _ -> 1) startNode endNode
|> List.length
|> (fun x -> x - 1) //the path includes the start node, but you are already there so there's no step to get there
|> printfn "Part 1 Shortest Number of Steps: %d"

// Part 2
let allAs =
  input
  |> Graph.allNodes
  |> List.filter (fun n -> n.Elevation = 0)

allAs
  .AsParallel()
  .Select(fun n -> Graph.findShortestPath (fun _ -> 1) n endNode input)
  .ToImmutableList()
|> Seq.toList
|> List.filter (List.isEmpty >> not)
|> List.sortBy (fun p -> p.Length)
|> List.head
|> List.length
|> (fun x -> x - 1) //the path includes the start node, but you are already there so there's no step to get there
|> printfn "Part 2 Shortest Number of Steps: %d"
