let parseStackLine (stackLine: string) =
  // each stack item is made up of four characters, "[", "X", "]", and " "
  // except for the last item which is missing the final space.
  // if a stack item is not present, it will be made up of four spaces (except for the last which is only 3 spaces).
  // But we can just add a space to the end of each line and then they all match!
  let stackLine = stackLine + " "
  let items =
    stackLine
    |> Seq.chunkBySize 4
    |> Seq.map (System.String.Concat)

  items
  |> Seq.map (fun s ->
                let m = System.Text.RegularExpressions.Regex.Match(s, "\[(.)\] ")
                if m.Success then
                  Some m.Groups[1].Value
                else
                  None)
  |> Seq.toList



let parseStacks (stackInput: string seq) =
  let stackInput = stackInput |> Seq.take ((Seq.length stackInput) - 1)
  let stackLines = stackInput |> Seq.map parseStackLine |> Seq.toList

  stackLines
  |> List.transpose
  |> List.map (List.rev)
  |> List.map (List.choose id)
  |> List.mapi (fun i s -> (i + 1), s)
  |> Map.ofList

let part1Mover amount moveFrom moveTo stacks =
  let moveSingle stacks moveFrom moveTo =
    let fromStack = stacks |> Map.find moveFrom
    let toStack = stacks |> Map.find moveTo

    let value = fromStack |> List.last
    let fromStack = fromStack |> List.take (List.length fromStack - 1)
    let toStack = List.append toStack [value]

    stacks
    |> Map.add moveFrom fromStack
    |> Map.add moveTo toStack

  [1..amount]
  |> List.fold (fun stacks _ -> moveSingle stacks moveFrom moveTo) stacks

let part2Mover amount moveFrom moveTo stacks =
  let fromStack = stacks |> Map.find moveFrom
  let toStack = stacks |> Map.find moveTo

  let skipAmount = (List.length fromStack) - amount
  let values = fromStack |> List.skip skipAmount
  let fromStack = fromStack |> List.take skipAmount
  let toStack = List.append toStack values

  stacks
  |> Map.add moveFrom fromStack
  |> Map.add moveTo toStack

let parseInputLine mover line =
  let m = System.Text.RegularExpressions.Regex.Match(line, "move (\\d+) from (\\d+) to (\\d+)")
  let amount = m.Groups[1].Value |> System.Int32.Parse
  let moveFrom = m.Groups[2].Value |> System.Int32.Parse
  let moveTo = m.Groups[3].Value |> System.Int32.Parse

  fun stacks -> stacks |> mover amount moveFrom moveTo

// let inputPath = "day5.test.txt"
let inputPath = "day5.real.txt"

let input = System.IO.File.ReadAllText(inputPath)
let inputStacks = (input.Split("\n\n")[0]).Split("\n")
let inputMoves = (input.Split("\n\n")[1]).Split("\n")

let stacks = parseStacks inputStacks

// part 1
inputMoves
|> Array.map (parseInputLine part1Mover)
|> Array.fold (fun s input -> input s) stacks
|> Map.values
|> Seq.map (List.last)
|> System.String.Concat
|> printfn "%s"

// part 2
inputMoves
|> Array.map (parseInputLine part2Mover)
|> Array.fold (fun s input -> input s) stacks
|> Map.values
|> Seq.map (List.last)
|> System.String.Concat
|> printfn "%s"