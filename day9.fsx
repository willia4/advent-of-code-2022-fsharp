// let inputFile = "day9.test.txt"
// let inputFile = "day9.test2.txt"
let inputFile = "day9.real.txt"

type Move =
  | Right of int
  | Left of int
  | Up of int
  | Down of int

let parseLine line =
  let regex = new System.Text.RegularExpressions.Regex("^(?<direction>[RULD])\\s+?(?<count>\\d+)$")
  match regex.Match(line) with
  | m when m.Success ->
      let dir = m.Groups["direction"].Value
      let count = m.Groups["count"].Value |> int

      if dir = "R" then (Right count)
      elif dir = "L" then (Left count)
      elif dir = "U" then (Up count)
      elif dir = "D" then (Down count)
      else failwithf "Could not parse \"%s\" as direction" dir
  | _ -> failwithf "Could not parse \"%s\" as Move" line

let input = System.IO.File.ReadAllLines(inputFile) |> Seq.map parseLine |> Seq.toList

type Coordinate = int * int
type Rope = Coordinate list

module Coordinate =
  let move deltaX deltaY (c: Coordinate) : Coordinate =
    let (x, y) = c
    (x + deltaX, y + deltaY)

  let distance ((a: Coordinate), (b: Coordinate)) =
    match a, b with
    | (hx, hy), (tx, ty) when hx = tx && hy = ty -> 0
    | (hx, hy), (tx, ty) when hx = tx -> System.Math.Abs(hy - ty)
    | (hx, hy), (tx, ty) when hy = ty -> System.Math.Abs(hx - tx)
    | (hx, hy), (tx, ty) ->
        let x' = (hx - tx)
        let y' = (hy - ty)
        let x' = x' * x'
        let y' = y' * y'
        let s = x' + y'
        let s = System.Math.Sqrt((float s))
        int (System.Math.Floor(s))

  let isValid (a: Coordinate) (b: Coordinate) =
    distance (a, b) <= 1

module Rope =
  let knots (r: Rope) =
    let rec i pairs (r: Rope) =
      match r with
      | [] -> pairs
      | [ x ] -> pairs
      | (first) :: (second :: rest) -> i ((first, second)::pairs) (second::rest)
    i [] r

  let tail (r: Rope) = r |> List.rev |> List.head

  let distances (r: Rope) =
    r
    |> knots
    |> List.map Coordinate.distance


  let moveHead deltaX deltaY (r: Rope) : Rope=
    let rec inner deltaX deltaY (left: Rope) (right: Rope) =
      match right with
      | [] -> left
      | [ last ] ->List.append left [ Coordinate.move deltaX deltaY last ]
      | first::(second::rest) ->
          let first = first |> Coordinate.move deltaX deltaY
          let left = List.append left [first]

          if Coordinate.isValid first second then List.append left (second::rest)
          else
            match (first, second) with
            | (hx, hy), (tx, ty) when hx = tx && hy = ty -> List.append left (second::rest) // no movement
            | (hx, hy), (tx, ty) when hx = tx && hy > ty -> inner 0 1 left (second::rest)
            | (hx, hy), (tx, ty) when hx = tx && hy < ty -> inner 0 -1 left (second::rest)
            | (hx, hy), (tx, ty) when hy = ty && hx > tx -> inner 1 0 left (second::rest)
            | (hx, hy), (tx, ty) when hy = ty && hx < tx -> inner -1 0 left (second::rest)
            | _, (tx, ty) ->
              let ((dx, dy), _) =
                [
                  ((1, 1), (first, (tx + 1, ty + 1)))
                  ((1, -1 ), (first, (tx + 1, ty - 1)))
                  ((-1, 1), (first, (tx - 1, ty + 1)))
                  ((-1, -1), (first, (tx - 1, ty - 1)))
                ]
                |> List.minBy (snd >> Coordinate.distance)
              inner dx dy left (second::rest)

    inner deltaX deltaY [] r

  let moveRopeBy (move: Move) (r: Rope) =
    let ((deltaX, deltaY), count) =
      match move with
      | Right count -> (1, 0), count
      | Left count -> (-1, 0), count
      | Up count -> (0, -1), count
      | Down count -> (0, 1), count

    let rec m acc remaining =
      match remaining with
      | 0 -> acc
      | n ->
          let (rope, tailLocations) = acc
          let tail1 = tail rope

          let rope' = rope |> moveHead deltaX deltaY
          let tail2 = tail rope'

          let tailLocations = tail1::tail2::tailLocations

          m (rope', tailLocations) (remaining - 1)
    let (rope', tailLocations) = m (r, []) count
    (rope', List.distinct tailLocations)


let part1 input =
  let start = [(100, 100); (100, 100)]
  input
  |> List.fold (fun (r, tailPositions) nextMove ->
        let (r', tailPositions') = Rope.moveRopeBy nextMove r

        (r', tailPositions' |> List.append tailPositions |> List.distinct)
  ) (start, [])
  |> snd
  |> List.length
  |> printfn "%A"

let part2 input =
  let start : Rope = [1..10] |> List.map (fun x -> (100, 100))
  input
  |> List.fold (fun (r, tailPositions) nextMove ->
        let (r', tailPositions') = Rope.moveRopeBy nextMove r

        (r', tailPositions' |> List.append tailPositions |> List.distinct)
  ) (start, [])
  |> snd
  |> List.length
  |> printfn "%A"

part1 input
part2 input

