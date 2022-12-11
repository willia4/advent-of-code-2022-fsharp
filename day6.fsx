
let slidingWindow size (s: 'a seq) = seq {
  let mutable l = s |> Seq.toList

  while (List.length l) > size do
    yield (List.take size l)
    l <- List.skip 1 l
}

let allDifferent (s: 'a seq) =
  s
  |> Seq.filter (fun v ->
      let matchingValues = Seq.filter (fun v' -> v' = v) s
      (Seq.length matchingValues) > 1)
  |> Seq.isEmpty

// let inputPath = "day6.test.txt"
let inputPath = "day6.real.txt"

let input = System.IO.File.ReadAllText(inputPath)

// Part 1
input
|> slidingWindow 4
|> Seq.filter allDifferent
|> Seq.head
|> System.String.Concat
|> input.IndexOf
|> fun i -> i + 4
|> printfn "Part 1 Marker: %d"

// Part 2
input
|> slidingWindow 14
|> Seq.filter allDifferent
|> Seq.head
|> System.String.Concat
|> input.IndexOf
|> fun i -> i + 14
|> printfn "Part 2 Marker: %d"

