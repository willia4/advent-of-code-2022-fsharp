open System;
open System.Collections.Immutable;

type Matrix = ImmutableArray<ImmutableArray<int>>
module Matrix =
  let matrixWidth (matrix: Matrix) = matrix[0].Length
  let matrixHeight (matrix: Matrix) = matrix.Length
  let allPoints (matrix: Matrix) =
    let height = matrixHeight matrix
    let width = matrixWidth matrix

    seq {
    for y = 0 to height - 1 do
      yield! seq {
        for x = 0 to width - 1 do
            yield (x, y, matrix[y][x])
      }
  }

  let get (x, y) (matrix: Matrix) = matrix[y][x]

  let isVerticalEdge (x, y) matrix = (x = 0) || (x = (matrixWidth matrix) - 1)
  let isHorizontalEdge (x, y) matrix = (y = 0) || (y = (matrixHeight matrix) - 1)
  let isEdge pt matrix = (isVerticalEdge pt matrix) || (isHorizontalEdge pt matrix)

  let allPointsInRow y matrix = seq {
    for x = 0 to ((matrixWidth matrix) - 1) do
      yield (x, y, matrix[y][x])
  }
  let allPointsInColumn x matrix = seq {
    for y = 0 to ((matrixHeight matrix) - 1) do
      yield (x, y, matrix[y][x])
  }

  let allPointsInRowExclusive (x, y) matrix =
    allPointsInRow x matrix
    |> Seq.filter (fun (x', y', _) -> (x', y') <> (x, y))

  let allPointsInColumnExclusive (x, y) matrix =
    allPointsInColumn x matrix
    |> Seq.filter (fun (x', y', _) -> (x', y') <> (x, y))

  let allPointsInColumnAbove (x, y) matrix =
    allPointsInColumn x matrix
    |> Seq.filter ((fun (_, y', _) -> y' < y))

  let allPointsInColumnBelow (x, y) matrix =
    allPointsInColumn x matrix
    |> Seq.filter ((fun (_, y', _) -> y' > y))

  let allPointsInRowToLeft (x, y) matrix =
    allPointsInRow y matrix
    |> Seq.filter ((fun (x', _, _) -> x' < x))

  let allPointsInRowToRight (x, y) matrix =
    allPointsInRow y matrix
    |> Seq.filter ((fun (x', _, _) -> x' > x))

  let pointIsVisibleHorizontally (x, y, v) matrix =
    let isVisible (_, _, v') = v' < v

    let visibleToTheLeft =
      matrix
      |> allPointsInRowToLeft (x, y)
      |> Seq.forall isVisible

    let visibleToTheRight =
      matrix
      |> allPointsInRowToRight (x, y)
      |> Seq.forall isVisible

    visibleToTheLeft || visibleToTheRight

  let pointIsVisibleVertically (x, y, v) matrix =
    let isVisible (_, _, v') = v' < v

    let visibleAbove =
      matrix
      |> allPointsInColumnAbove (x, y)
      |> Seq.forall isVisible

    let visibleBelow =
      matrix
      |> allPointsInColumnBelow (x, y)
      |> Seq.forall isVisible

    visibleAbove || visibleBelow

  let pointIsVisible (x, y, v) matrix =
    isEdge (x, y) matrix ||
    pointIsVisibleHorizontally (x, y, v) matrix ||
    pointIsVisibleVertically (x, y, v) matrix

  let coordinateIsVisible (x, y) matrix =
    let v = get (x, y) matrix
    pointIsVisible (x, y, v) matrix

//let inputPath = "day8.test.txt"
let inputPath = "day8.real.txt"

let inputLines = System.IO.File.ReadAllLines(inputPath) :> string seq
let matrix : Matrix =
  inputLines
  |> Seq.map (
      fun line ->
          line
          |> Seq.map (fun c -> c |> string |> Int32.Parse)
          |> ImmutableArray.Empty.AddRange)
  |> ImmutableArray.Empty.AddRange

// Part 1
matrix
|> Matrix.allPoints
|> Seq.filter (fun (x, y, v) -> Matrix.pointIsVisible (x, y, v) matrix)
|> Seq.length
|> printfn "Part 1 answer: %d"

let scenicCount blockingHeight (pts: int seq) =
  seq {
    let mutable didBlocking = false
    for pt in pts do
      if (not didBlocking) && (pt < blockingHeight) then
        yield pt
      else
        if not didBlocking then
          yield pt
          didBlocking <- true
  }
  |> Seq.length


// Part 2
let visibilityUp (x, y) matrix =
  let v = matrix |> Matrix.get (x, y)

  matrix
  |> Matrix.allPointsInColumnAbove (x, y)
  |> Seq.sortByDescending (fun (_, y', _) -> y')
  |> Seq.map (fun (_, _, v') -> v')
  |> scenicCount v

let visibilityDown (x, y) matrix =
  let v = matrix |> Matrix.get (x, y)

  matrix
  |> Matrix.allPointsInColumnBelow (x, y)
  |> Seq.sortBy (fun (_, y', _) -> y')
  |> Seq.map (fun (_, _, v') -> v')
  |> scenicCount v

let visibilityLeft (x, y) matrix =
  let v = matrix |> Matrix.get (x, y)

  matrix
  |> Matrix.allPointsInRowToLeft (x, y)
  |> Seq.sortByDescending (fun (x', _, _) -> x')
  |> Seq.map (fun (_, _, v') -> v')
  |> scenicCount v

let visibilityRight (x, y) matrix =
  let v = matrix |> Matrix.get (x, y)

  matrix
  |> Matrix.allPointsInRowToRight (x, y)
  |> Seq.sortBy (fun (x', _, _) -> x')
  |> Seq.map (fun (_, _, v') -> v')
  |> scenicCount v

let scenicScore pt matrix =
  (visibilityUp pt matrix) * (visibilityRight pt matrix) * (visibilityDown pt matrix) * (visibilityLeft pt matrix)

matrix
|> Matrix.allPoints
|> Seq.map (fun (x, y, _) -> scenicScore (x, y) matrix)
|> Seq.max
|> printfn "Part 2 answer: %d"