open System

type Item = {
  Worry: int64
}

type Monkey = {
  Index: int
  Items: Item list
  Operation: Item -> int64
  TestDivisor: int64
  TrueMonkeyIndex: int
  FalseMonkeyIndex: int
  InspectionCount: int64
}

type Monkeys = Monkey list

module Monkey =

  let fromString (monkeyString: string) =
    let parseRegex pattern fmap input =
      let m = Text.RegularExpressions.Regex.Match(input, pattern)
      if m.Success && m.Groups.Count > 1 then
        fmap m.Groups[1].Value
      else
        raise (InvalidOperationException($"Could not match regex '{pattern}' against '{input}'"))

    let parseOperation (operationString: string) =
      let parts = operationString.Split(" ")

      let parseValue expression variableItem = match expression with
                                                | "old" -> variableItem.Worry
                                                | constant -> Int64.Parse constant

      let lhs = parseValue parts[0]
      let rhs = parseValue parts[2]
      let op = match parts[1] with
               | "+" -> (+)
               | "*" -> (*)
               | _ -> raise (InvalidOperationException($"Could not determine operator for {parts[1]}"))

      fun value ->
        op (lhs value) (rhs value)

    let monkeyLines = monkeyString.Split("\n")

    {
      InspectionCount = 0L
      Index = monkeyLines[0] |> parseRegex "Monkey (\\d+):" Int32.Parse
      Items = monkeyLines[1]
              |> parseRegex "\\s+Starting items: (.*)" string
              |> fun itemsList -> itemsList.Split(", ")
              |> Array.map (fun itemWorry -> { Worry = (Int64.Parse itemWorry)})
              |> Array.toList

      Operation = monkeyLines[2] |> parseRegex "\\s+Operation: new = (.*)" parseOperation
      TestDivisor = monkeyLines[3] |> parseRegex "\\s+Test: divisible by (\\d*)" Int64.Parse
      TrueMonkeyIndex = monkeyLines[4] |> parseRegex "\\s+.*?throw to monkey (\\d*)" Int32.Parse
      FalseMonkeyIndex = monkeyLines[5] |> parseRegex "\\s+.*?throw to monkey (\\d*)" Int32.Parse
    }

  let toString m =
    let printItems items =
      let worries = items |> Seq.map (fun i -> i.Worry) |> Seq.map string
      String.Join(", ", worries)

    $"Monkey {m.Index}: {printItems m.Items}"

let takeTurn manageWorry monkeys monkey =
  let inspectedItemCount = monkey.Items |> List.length |> int64

  let commonMultiple =
    monkeys
    |> Map.values
    |> Seq.map (fun m -> m.TestDivisor)
    |> Seq.reduce(*)

  let mutable monkeys = monkeys
  for item in monkey.Items do
    let inspectionWorry = monkey.Operation item
    let managedWorry= manageWorry inspectionWorry

    let testValue = managedWorry % monkey.TestDivisor = 0L

    let destMonkey = if testValue then monkey.TrueMonkeyIndex else monkey.FalseMonkeyIndex
    let saferItem = { item with Worry = managedWorry }

    let destMonkey = (Map.find destMonkey monkeys)
    let destMonkeyItems = List.append destMonkey.Items [saferItem]

    let destMonkey = { destMonkey with Items = destMonkeyItems }
    monkeys <- Map.add destMonkey.Index destMonkey monkeys

  monkeys
  |> Map.add monkey.Index { monkey with Items = []; InspectionCount = monkey.InspectionCount + inspectedItemCount}

let playRound manageWorry monkeys =
  let takeTurn = takeTurn manageWorry

  monkeys
  |> Map.keys
  |> Seq.sort
  |> Seq.fold (fun monkeys idx ->
      let m = monkeys |> Map.find idx
      takeTurn monkeys m) monkeys

//let inputPath = "day11.test.txt"
let inputPath = "day11.real.txt"
let inputMonkeys = IO.File.ReadAllText(inputPath).Split("\n\n") |> Array.map Monkey.fromString

let monkeys =
  inputMonkeys
  |> Array.fold (fun map monkey -> Map.add monkey.Index monkey map) Map.empty


// Part 1
let part1PlayRound  = playRound (fun x -> x / 3L)

let part1playRecord =
  [1..21]
  |> Seq.fold (
        fun playRecord round ->
          let lastRound = playRecord |> Map.find (round - 1)
          let currentRound = part1PlayRound lastRound
          Map.add round currentRound playRecord) (Map.ofList [(0, monkeys)])

let part1round20 = Map.find 20 part1playRecord
let part1InspectionCounts =
  part1round20
  |> Map.values
  |> Seq.map (fun m -> m.InspectionCount)

part1InspectionCounts
|> Seq.sortDescending
|> Seq.take 2
|> Seq.reduce (*)
|> string
|> printfn "Part 1 Monkey Business: %s"

// Part 2
let manageWorry monkeys =
  // note: I did cheat a little by looking at some other solutions on how to do this for part 2
  // common multiples and divisors is not the kind of math I excel at, if there is any math I excel at
  let commonMultiple =
    monkeys
    |> Map.values
    |> Seq.map (fun m -> m.TestDivisor)
    |> Seq.reduce (*)

  fun worryValue -> worryValue % commonMultiple

let part2PlayRound = playRound (manageWorry monkeys)

let part2playRecord =
  [1..10001]
  |> Seq.fold (
        fun playRecord round ->
          let lastRound = playRecord |> Map.find (round - 1)
          let currentRound = part2PlayRound lastRound
          Map.add round currentRound playRecord) (Map.ofList [(0, monkeys)])

let part2round10000 = Map.find 10000 part2playRecord
let part2InspectionCounts =
  part2round10000
  |> Map.values
  |> Seq.map (fun m -> m.InspectionCount)

part2InspectionCounts
|> Seq.sortDescending
|> Seq.take 2
|> Seq.reduce (*)
|> string
|> printfn "Part 2 Monkey Business: %s"