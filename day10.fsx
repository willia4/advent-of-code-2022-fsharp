open System
open System.Collections.Immutable

let addx_regex = new System.Text.RegularExpressions.Regex("addx (-?\\d*)")
let noop_regex = new System.Text.RegularExpressions.Regex("noop")

type ScreenMatrix = ImmutableArray<ImmutableArray<bool>>

module ScreenMatrix =
  let blankScreen: ScreenMatrix =
    let blankRow = ImmutableArray.Empty.AddRange([0..39] |> Seq.map (fun _ -> false))
    ImmutableArray.Empty.AddRange([0..5] |> Seq.map (fun _ -> blankRow))

  let toString (m: ScreenMatrix) =
    let sb = new Text.StringBuilder(41 * 6)
    for row in m do
      for pixel in row do
        sb.Append(if pixel then "▉" else " ") |> ignore
      sb.AppendLine() |> ignore

    sb.ToString()

  let setPixel (x, y) (m: ScreenMatrix) =
    let row = m[y]
    let newRow = row.SetItem(x, true)
    m.SetItem(y, newRow)

let spritePixelsInLine x =
  let pixelLeft = x - 1
  let pixelMiddle = x
  let pixelRight = x + 1

  seq {
    if pixelLeft >= 0 then yield pixelLeft
    yield pixelMiddle
    if pixelRight < 40 then yield pixelRight
  } |> Seq.toList

type ProgramState = { Cycle: int; X: int; Screen: ScreenMatrix }

let parseInstruction inst =
  let addx_match = addx_regex.Match(inst)
  let noop_match = noop_regex.Match(inst)

  //let debugPrint msg = printfn "%s" msg
  let debugPrint msg = ()

  let updateScreen_function state =
    debugPrint $"\n\n>> Cycle %d{state.Cycle}"
    debugPrint $"  X: %d{state.X}"

    let row =
      match state.Cycle with
      | c when c < (40 * 1) -> 0
      | c when c < (40 * 2) -> 1
      | c when c < (40 * 3) -> 2
      | c when c < (40 * 4) -> 3
      | c when c < (40 * 5) -> 4
      | c when c < (40 * 6) -> 5
      | _ -> 5
    let col = (state.Cycle - 1) % 40

    debugPrint $"  row: %d{row}"
    debugPrint $"  col: %d{col}"

    let litPixels = spritePixelsInLine state.X
    let newScreen =
      if litPixels |> List.contains col then
        state.Screen |> ScreenMatrix.setPixel (col, row)
      else
        state.Screen
    { state with Screen = newScreen}

  let noop_function state =
    let state = updateScreen_function state
    { state with Cycle = state.Cycle + 1}

  let addx_function value state =
    let state = updateScreen_function state

    { state with Cycle = state.Cycle + 1
                         X = state.X + value }

  if (noop_match.Success) then
    [ noop_function ]
  elif (addx_match.Success) then
    let addValue = System.Int32.Parse(addx_match.Groups[1].Value)
    [ noop_function
      addx_function addValue ]
  else []

let parseInput (lines: string seq) =
  lines
  |> Seq.collect parseInstruction
  |> Seq.toList

let runProgram (functions: (ProgramState -> ProgramState) seq) =
  let emptyState = {Cycle = 1; X = 1; Screen = ScreenMatrix.blankScreen }

  functions
  |> Seq.mapFold (fun state f ->
      let nextState = f state
      (f state), (f state)
    )  emptyState
  |> fst
  |> Seq.map (fun state -> state, state.Cycle * state.X )
  |> Seq.toList

let inputPath = "day10.real.txt"
//let inputPath = "day10.test.txt"
let input =
  System.IO.File.ReadAllLines(inputPath)

let sw = System.Diagnostics.Stopwatch()
sw.Start()
let execution = input |> parseInput |> runProgram
sw.Stop()

sw.ElapsedMilliseconds |> printfn "Ran program in %dms"
sw.Reset()
sw.Start()

// Part 1
let findCycle cycle = cycle, (execution |> List.find (fun (state, _) -> state.Cycle = cycle) |> snd)
let answers = Map [ findCycle 20; findCycle 60; findCycle 100; findCycle 140; findCycle 180; findCycle 220 ]

sw.Stop()
sw.ElapsedMilliseconds |> printfn "Found cycles in %dms"
answers
|> Map.values
|> Seq.sum
|> printfn "Part 1 answer: %d"

// {Part 2}
execution
|> List.last
|> (fun (state, _) -> state.Screen)
|> ScreenMatrix.toString
|> printfn "Part 2\n%s"