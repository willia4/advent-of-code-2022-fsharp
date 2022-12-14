
type SpaceType =
  | Air
  | Rock
  | Sand
  | Abyss

type Scan = {
  KnownPoints: Map<(int * int), SpaceType>
  MinX: int
  MaxX: int
  MinY: int
  MaxY: int
}

let startPoint = (500, 0)

module Scan =
  let spaceTypeAtPoint_part1 scan pt = Map.tryFind pt scan.KnownPoints |> Option.defaultValue Air
  let spaceTypeAtPoint_part2 scan pt =
    match Map.tryFind pt scan.KnownPoints with
    | Some t -> t
    | None ->
        let (_, y) = pt
        if y > (scan.MaxY + 1) then Rock else Air


  let toString spaceTypeAtPoint scan =
    let (startX, startY) = startPoint
    let (x1, x2, y1, y2) = (
      System.Math.Min(scan.MinX, startX),
      System.Math.Max(scan.MaxX, startX),
      System.Math.Min(scan.MinY, startY),
      System.Math.Max(scan.MaxY, startY))

    let sb = System.Text.StringBuilder()

    for y in [y1..y2] do
      for x in [x1..x2] do
        if (x, y) = startPoint then
          sb.Append("+") |> ignore
        else
          sb.Append(
            match spaceTypeAtPoint scan (x, y) with
            | Air -> "."
            | Rock -> "#"
            | Sand -> "o"
            | Abyss -> "~"
          ) |> ignore
      sb.AppendLine() |> ignore

    sb.ToString()

  let toString_part1 = toString spaceTypeAtPoint_part1

let parseInput (lines: string seq) =
  let parsePoint p =
    let m = System.Text.RegularExpressions.Regex.Match(p, "(\\d+),(\\d+)")
    (int (m.Groups[1].Value)), (int (m.Groups[2].Value))

  let parseLine (line: string) =
      line.Split(" -> ")
      |> Seq.windowed 2
      |> Seq.collect (fun pts ->
          let (x1, y1) = parsePoint pts[0]
          let (x2, y2)= parsePoint pts[1]

          if x1 = x2 then
            [
              let minY = System.Math.Min(y1, y2)
              let maxY = System.Math.Max(y1, y2)
              for y in [minY..maxY] do
                (x1, y)
            ]
          elif y1 = y2 then
            let minX = System.Math.Min(x1, x2)
            let maxX = System.Math.Max(x1, x2)
            [
              for x in [minX..maxX] do
                (x, y1)
            ]
          else
            []
          )
      |> Seq.distinct
      |> Seq.toList

  let rockPoints =
    lines
    |> Seq.collect parseLine
    |> Set.ofSeq

  { KnownPoints = rockPoints |> Seq.map (fun pt -> (pt, Rock)) |> Map.ofSeq
    MinX = rockPoints |> Seq.map fst |> Seq.min
    MaxX = rockPoints |> Seq.map fst |> Seq.max
    MinY = rockPoints |> Seq.map snd |> Seq.min
    MaxY = rockPoints |> Seq.map snd |> Seq.max }



let dropSandOnce spaceTypeAtPoint hasAbyss scan =
  let downPoint (x, y) = (x, y + 1)
  let leftPoint (x, y) = (x - 1, y + 1)
  let rightPoint (x, y) = (x + 1, y + 1)

  let nextPoint currentPoint =
    [ downPoint currentPoint
      leftPoint currentPoint
      rightPoint currentPoint ]
    |> List.map (fun pt -> pt, spaceTypeAtPoint scan pt)
    |> List.tryFind (fun (_, t) -> t = Air)
    |> Option.map fst

  let floor = if hasAbyss then scan.MaxY else System.Int32.MaxValue

  let rec inner currentPoint =
    // printfn "currentPoint: %A" currentPoint

    let nextPoint = nextPoint currentPoint
    // printfn "nextPoint: %A" nextPoint

    match nextPoint with
    | Some (nextX, nextY) ->
        if nextY > floor then
          Abyss, (nextX, nextY)
        else
          inner (nextX, nextY)

    | None -> Sand, currentPoint

  let (nextPointType, nextPoint) = inner startPoint

  match nextPointType with
  | Abyss -> None, scan
  | _ ->
    if nextPoint <> startPoint then
      let knownPoints = scan.KnownPoints |> Map.add nextPoint Sand
      Some nextPoint, {scan with KnownPoints = knownPoints }
    else
      None, scan

let dropSandUntilAbyss spaceTypeAtPoint scan =
  let rec inner count scan =
    // printfn "Count: %d" count
    // printfn "%s" (Scan.toString scan)

    let (nextPt, nextScan) = dropSandOnce spaceTypeAtPoint true scan

    // printfn "Next Point: %A" nextPt
    // printfn "Next Scan:\n%s" (Scan.toString nextScan)

    match nextPt with
    | None -> count, nextScan
    | Some nextPt -> inner (count + 1) nextScan

  inner 0 scan

let dropSandUntilStopped spaceTypeAtPoint scan =
  let rec inner count scan =
    // printfn "Count: %d" count
    // printfn "%s" (Scan.toString scan)

    let (nextPt, nextScan) = dropSandOnce spaceTypeAtPoint false scan

    // printfn "Next Point: %A" nextPt
    // printfn "Next Scan:\n%s" (Scan.toString nextScan)

    match nextPt with
    | None -> count, nextScan // this is probably an error condition but it seems safe to just treat it as an exit given our inputs
    | Some nextPt ->

        inner (count + 1) nextScan

  inner 1 scan

//let inputPath = "day14.test.txt"
let inputPath = "day14.real.txt"

let inputScan = System.IO.File.ReadAllLines(inputPath) |> parseInput

// Part 1
let (count_1, finalScan_1) = inputScan |> dropSandUntilAbyss Scan.spaceTypeAtPoint_part1
printfn "Part 1 Count before abyss: %d" count_1

// Part 2
let (count_2, finalScan_2) = inputScan |> dropSandUntilStopped Scan.spaceTypeAtPoint_part2
printfn "Part 2 Count before stopped: %d" count_2