let findDuplicateItem (left, right) =
  let isInRight c = right |> List.contains c

  left
  |> List.filter isInRight
  |> List.head

let priorty c =
  let lowercase = "abcdefghijklmnopqrstuvwxyz" |> Seq.tryFindIndex (fun c' -> c' = c)
  let uppercase = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" |> Seq.tryFindIndex (fun c' -> c' = c)

  match lowercase, uppercase with
  | Some i, _ -> i + 1
  | _, Some i -> i + 27
  | _ -> 0

// let inputPath = "day3.test.txt"
let inputPath = "day3.real.txt"

let input =
  System.IO.File.ReadAllLines(inputPath)
  |> Array.map (fun s -> (s.Substring(0, s.Length / 2) |> Seq.toList), (s.Substring(s.Length / 2) |> Seq.toList))
  |> Array.toList

// Part 1
input
|> List.map findDuplicateItem
|> List.map (fun c -> c, (priorty c))
|> List.map snd
|> List.sum
|> printfn "Part 1: %d"


// Part 2
let findBadge (elves: char list list) =
  let elementInAllLists (lists: 'a list list) (element: 'a) =
    lists
    |> List.forall (List.contains element)

  match elves with
  | head::rest ->
      head
      |> List.filter (elementInAllLists rest)
      |> List.tryHead
  | _ -> None

input
|> List.map (fun (left, right) -> List.append left right)
|> List.chunkBySize 3
|> List.map findBadge
|> List.choose id
|> List.map priorty
|> List.sum
|> printfn "Part 2: %d"