open System.Linq

type IPoint =
    abstract member X: int64 with get
    abstract member Y: int64 with get

type Point = {
  X: int64
  Y: int64
} with
  override self.ToString() = $"({self.X}, {self.Y})"
  interface IPoint with
    member self.X with get() = self.X
    member self.Y with get() = self.Y

type Sensor = {
  X: int64
  Y: int64
  BeaconX: int64
  BeaconY: int64
} with
  override self.ToString() = $"Sensor at x={self.X}, y={self.Y}: closest beacon is at x={self.BeaconX}, y={self.BeaconY}"
  member self.OriginPoint = {X = self.X; Y = self.Y}
  member self.BeaconPoint = {X = self.BeaconX; Y = self.BeaconY}

  interface IPoint with
    member self.X with get() = self.OriginPoint.X
    member self.Y with get() = self.OriginPoint.Y

let parseSensorLine line =
  let m = System.Text.RegularExpressions.Regex.Match(line,
    "Sensor at x=(\\d+), y=(\\d+): closest beacon is at x=(-?\\d+), y=(-?\\d+)")

  { X = System.Int64.Parse(m.Groups[1].Value)
    Y = System.Int64.Parse(m.Groups[2].Value)
    BeaconX = System.Int64.Parse(m.Groups[3].Value)
    BeaconY = System.Int64.Parse(m.Groups[4].Value) }

let parseInput lines =
  lines
  |> Array.map parseSensorLine
  |> Array.toList

let inline manhattenDistance (p1: Point) (p2: Point) = (abs (p1.X - p2.X)) + (abs (p1.Y - p2.Y))

let pointsInRadiusOnRow (pt: Point) radius row =
  let top = { X = pt.X; Y = pt.Y - radius}
  let bottom = { X = pt.X; Y = pt.Y + radius}

  if row < top.Y || row > bottom.Y || radius = 0L then []
  else
    let dist = abs (pt.Y - row)
    let radius = radius - dist
    [
      for x = (pt.X - radius) to (pt.X + radius) do
        { X = x; Y = row }
    ]

let pointsInRadius (pt: Point) radius =
  if radius = 0L then []
  else
    let top = { X = pt.X; Y = pt.Y - radius}
    let bottom = { X = pt.X; Y = pt.Y + radius}

    let points = pointsInRadiusOnRow pt radius
    [top.Y..bottom.Y]
    |> List.fold (fun acc row ->
        List.append acc (points row)) []
    |> List.distinct

    // let rec inner acc currentCenter currentWidth =
    //   // printfn "Top: %s" (string top)
    //   // printfn "Bottom: %s" (string bottom)
    //   // printfn "Current: %s" (string currentCenter)
    //   // printfn "Width: %d" currentWidth

    //   if currentCenter = bottom then
    //     List.append acc [ currentCenter ]
    //   else
    //     let left = [for x = (currentCenter.X - currentWidth) to currentCenter.X do { currentCenter with X = x }]
    //     let right = [for x = currentCenter.X to (currentCenter.X + currentWidth) do { currentCenter with X = x }]

    //     let acc =
    //       acc
    //       |> List.append left
    //       |> List.append right
    //       |> List.append [currentCenter]
    //     inner acc {currentCenter with Y = currentCenter.Y + 1L} (currentWidth + 1L)

    // inner [] top 0
    // |> List.distinct

let boundingBoxForPoints (points: IPoint seq) =
  points
  |> Seq.fold
      (fun (minX, maxX, minY, maxY) (pt) ->((min minX pt.X), (max maxX pt.X), (min minY pt.Y), (max maxY pt.Y)))
      (System.Int64.MaxValue, System.Int64.MinValue, System.Int64.MaxValue, System.Int64.MinValue)

let boundingBoxForSensors (sensors: Sensor seq) =
  sensors
  |> Seq.collect (fun s -> [ s.OriginPoint; s.BeaconPoint ])
  |> Seq.map (fun p -> p :> IPoint)
  |> boundingBoxForPoints


let pointsConstrainedToBoundingBox (minX, maxX, minY, maxY) (points: Point seq) =
  points
  |> Seq.filter (fun p -> p.X >= minX && p.X <= maxX && p.Y >= minY && p.Y <= maxY)

// let (inputPath, part1row, part2min, part2max) = "day15.test.txt", 10, 0, 20
let (inputPath, part1row, part2min, part2max) = "day15.real.txt", 2000000, 0, 4000000

let inputSensors = System.IO.File.ReadAllLines(inputPath) |> parseInput

let beacons =
  inputSensors
  |> List.map (fun s -> s.BeaconPoint)
  |> Set.ofList

// let definitelyNotBeacons =
//   inputSensors
//   |> List.collect (fun s -> pointsInRadius s.OriginPoint (manhattenDistance s.OriginPoint s.BeaconPoint))
//   |> List.distinct
//   |> List.filter (fun s -> not (Set.contains s beacons))

// let definitelyNotBeaconsInBoundingBox = pointsConstrainedToBoundingBox (boundingBoxForSensors inputSensors) definitelyNotBeacons

let beaconsOnRow row (inputSensors: Sensor list) =
  inputSensors
  |> List.map (fun s -> s.BeaconPoint)
  |> List.filter (fun p -> p.Y = row)
  |> List.distinct

let sensorsOnRow row (inputSensors: Sensor list) =
  inputSensors
  |> List.map (fun s -> s.OriginPoint)
  |> List.filter (fun p -> p.Y = row)
  |> List.distinct

let checkedValuesOnRow row (inputSensors: Sensor list) =
  inputSensors
  |> List.collect (fun s -> pointsInRadiusOnRow s.OriginPoint (manhattenDistance s.OriginPoint s.BeaconPoint) row)
  |> List.distinct

let definitenyNotBeaconsOnRow row (inputSensors: Sensor list) =
  List.append
    (inputSensors |> sensorsOnRow row)
    (inputSensors |> checkedValuesOnRow row)
  |> List.except (inputSensors |> beaconsOnRow row)
  |> List.distinct

let possibleBeaconsOnRow minX maxX row (inputSensors: Sensor list) =
  let excludedX =
    inputSensors
    |> definitenyNotBeaconsOnRow row
    |> List.map (fun pt -> pt.X)
    |> List.distinct

  seq {minX..maxX}
  |> Seq.except excludedX
  |> Seq.map (fun x -> {X=x;Y=row})
  |> Seq.toList

// // Part 1

let definitelyNotBeaconsOnInspectedRow = inputSensors |> definitenyNotBeaconsOnRow part1row

definitelyNotBeaconsOnInspectedRow
|> List.length
|> printfn "Part 1: Non-beacons on row %d: %d" part1row


// Part 2
let knownBeacons = inputSensors |> List.map (fun s -> s.BeaconPoint)

// seq {part2min..part2max}
// |> Seq.collect (fun y -> (possibleBeaconsOnRow part2min part2max y inputSensors))
// |> Seq.except knownBeacons
// |> Seq.take 1
// |> Seq.head
// |> (fun pt -> (pt.X * 4000000L) + pt.Y)
// |> printfn "Part 2: Beacon Tuning Frequency: %d"

let knownBeaconsp = knownBeacons.AsParallel()
(seq {part2min..part2max})
  .AsParallel()
  .SelectMany(fun y -> (possibleBeaconsOnRow part2min part2max y inputSensors) |> (Seq.except knownBeacons))
  // .Except(knownBeaconsp)
  .Take(1)
  .Select(fun pt -> (pt.X * 4000000L) + pt.Y)
  .First()
|> printfn "Part 2: Beacon Tuning Frequency: %d"



// let s1 = inputSensors |> List.find (fun s -> s.OriginPoint = { X=8; Y=7})
// inputSensors |> beaconsOnRow 16
// inputSensors |> sensorsOnRow 16
// inputSensors |> checkedValuesOnRow 16 |> List.map string
// // pointsInRadiusOnRow s1.OriginPoint (manhattenDistance s1.OriginPoint s1.BeaconPoint) -3 |> List.map string
// // pointsInRadiusOnRow s1.OriginPoint (manhattenDistance s1.OriginPoint s1.BeaconPoint) -2 |> List.map string
// // pointsInRadiusOnRow s1.OriginPoint (manhattenDistance s1.OriginPoint s1.BeaconPoint) -1 |> List.map string
// // pointsInRadiusOnRow s1.OriginPoint (manhattenDistance s1.OriginPoint s1.BeaconPoint)  0 |> List.map string
// // pointsInRadiusOnRow s1.OriginPoint (manhattenDistance s1.OriginPoint s1.BeaconPoint)  1 |> List.map string
// // pointsInRadiusOnRow s1.OriginPoint (manhattenDistance s1.OriginPoint s1.BeaconPoint)  2 |> List.map string
// // pointsInRadiusOnRow s1.OriginPoint (manhattenDistance s1.OriginPoint s1.BeaconPoint)  3 |> List.map string
// // pointsInRadiusOnRow s1.OriginPoint (manhattenDistance s1.OriginPoint s1.BeaconPoint)  4 |> List.map string
// // pointsInRadiusOnRow s1.OriginPoint (manhattenDistance s1.OriginPoint s1.BeaconPoint)  5 |> List.map string
// // pointsInRadiusOnRow s1.OriginPoint (manhattenDistance s1.OriginPoint s1.BeaconPoint)  6 |> List.map string
// // pointsInRadiusOnRow s1.OriginPoint (manhattenDistance s1.OriginPoint s1.BeaconPoint)  7 |> List.map string
// // pointsInRadiusOnRow s1.OriginPoint (manhattenDistance s1.OriginPoint s1.BeaconPoint)  17 |> List.map string