let parseInput (input: string) =
  let parseElf (input: string) =
    input.Split("\n")
    |> Array.map (System.Int32.Parse)

  input.Split ("\n\n")
  |> Array.map parseElf

//let inputPath = "day1.test.txt"
let inputPath = "day1.real.txt"
let elves = parseInput (System.IO.File.ReadAllText inputPath)

// Part 1
elves
|> Seq.map (fun elf -> Seq.sum elf)
|> Seq.sortDescending
|> Seq.take 1
|> Seq.sum
|> printfn "Part 1: %d"

// Part 2
elves
|> Seq.map (fun elf -> Seq.sum elf)
|> Seq.sortDescending
|> Seq.take 3
|> Seq.sum
|> printfn "Part 2: %d"