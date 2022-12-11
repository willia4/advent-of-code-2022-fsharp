type SecondMoves =
  | X
  | Y
  | Z

type RPS =
  | Rock
  | Paper
  | Scissors

let scoreMove move =
  match move with
  | Rock -> 1
  | Paper -> 2
  | Scissors -> 3

let winningMove move =
  match move with
  | Rock -> Paper
  | Paper -> Scissors
  | Scissors -> Rock

let losingMove move =
  match move with
  | Rock -> Scissors
  | Paper -> Rock
  | Scissors -> Paper

let tieMove (move: RPS) = move

let scoreOutcome firstMove secondMove =
    if (winningMove firstMove) = secondMove then 6
    elif (losingMove firstMove) = secondMove then 0
    else 3

let scoreRound firstMove secondMove = (scoreOutcome firstMove secondMove) + (scoreMove secondMove)

// let inputPath = "day2.test.txt"
let inputPath = "day2.real.txt"

let input =
  System.IO.File.ReadAllLines(inputPath)
  |> Seq.map (fun line ->
      let line = line.Split(" ")
      let first =
        match line[0] with
        | s when s = "A" -> Rock
        | s when s = "B" -> Paper
        | s when s = "C" -> Scissors
        | _ -> raise (System.InvalidOperationException())

      let second =
        match line[1] with
        | s when s = "X" -> X
        | s when s = "Y" -> Y
        | s when s = "Z" -> Z
        | _ -> raise (System.InvalidOperationException())

      first, second
  )

// Part 1
let part1PlayRound (left, right) =
  let firstMove = left
  let secondMove = match right with
                   | X -> Rock
                   | Y -> Paper
                   | Z -> Scissors
  scoreRound firstMove secondMove

input
|> Seq.map part1PlayRound
|> Seq.sum
|> printfn "Part 1 Total Score: %d"

// Part 2
let part2PlayRound (left, right) =
  let firstMove = left
  let secondMove = match right with
                   | X -> losingMove firstMove
                   | Y -> tieMove firstMove
                   | Z -> winningMove firstMove
  scoreRound firstMove secondMove

input
|> Seq.map part2PlayRound
|> Seq.sum
|> printfn "Part 2 Total Score: %d"