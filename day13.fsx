type Token =
  | OpenBracket
  | CloseBracket
  | Comma
  | IntegerValue of int
  with
  override self.ToString() =
    match self with
    | OpenBracket -> "["
    | CloseBracket -> "]"
    | Comma -> ","
    | IntegerValue int -> string int

type Tokenizer = {
  data: string
  index: int
}

module Tokenizer =
  let fromString s = { data= s; index = -1 }
  let remainingData t = (t.data.Length - 1) - (t.index)

  let nextChar t =
    let nextIndex = t.index + 1
    if nextIndex < t.data.Length then

      Some (t.data.Substring(nextIndex, 1)), { t with index = nextIndex}
    else
      None, t

  let peekChar = nextChar >> fst

  let hasNextChar = peekChar >> Option.isSome

  let rec eatSpaces t =
    match nextChar t with
    | Some c, nextT when c = " " -> eatSpaces nextT
    | _ -> t

  let remainingString t =
    let idx = t.index + 1
    if t.data.Length > idx then t.data.Substring(idx) else ""

  let private tryOpenBracket t =
    let (nextChar, nextTokenizer) = nextChar t
    match nextChar with
    | Some c when c = "[" -> Some (OpenBracket, nextTokenizer)
    | _ -> None

  let private tryCloseBracket t =
    let (nextChar, nextTokenizer) = nextChar t
    match nextChar with
    | Some c when c = "]" -> Some (CloseBracket, nextTokenizer)
    | _ -> None

  let private tryComma t =
    let (nextChar, nextTokenizer) = nextChar t
    match nextChar with
    | Some c when c = "," -> Some (Comma, nextTokenizer)
    | _ -> None

  let private tryIntegerValue t =
    let isDigit (s: string) = "0123456789".Contains(s)

    let rec inner (acc: string) t =
      match nextChar t with
      | Some d, nextT when isDigit d -> inner (acc + d) nextT
      | _ -> acc, t

    let buffer, nextT = inner "" t
    if (buffer |> ((System.String.IsNullOrWhiteSpace) >> not)) then
      Some (IntegerValue (System.Int32.Parse(buffer)), nextT)
    else
      None

  let nextToken t =
    let t = eatSpaces t

    let possibleToken =
      [ tryOpenBracket t
        tryCloseBracket t
        tryComma t
        tryIntegerValue t ]
      |> List.tryPick id

    match possibleToken with
    | Some (token, nextT) -> Some token, nextT
    | _ -> None, t

  let peekToken t =
    match nextToken t with
    | Some token, _ -> Some token
    | _ -> None

  let hasNextToken = peekToken >> Option.isSome

type Packet =
  | PacketValue of int
  | PacketList of Packet list
  with
  override self.ToString() =
    match self with
    | PacketValue v -> $"{v}"
    | PacketList l -> "[" + System.String.Join(",", (l |> List.map string)) + "]"

module Packet =
  let fromString s =
    let parsePacket t =
      let parseIntegerValue t =
        match Tokenizer.nextToken t with
        | Some (Token.IntegerValue v), nextT -> (Some (PacketValue v)), nextT
        | Some otherToken, _ -> None, t
        | None, _ -> None, t

      let rec parseListValue originalT =
        let openBracket, t = Tokenizer.nextToken originalT
        if openBracket |> Option.isNone then
          (None, originalT)
        elif (openBracket |> Option.get) <> OpenBracket then
          (None, originalT)
        else
          let mutable t = t
          let currentList = ResizeArray<Packet>()

          let isCloseBracket = function
                              | Some token when token = CloseBracket -> true
                              | _ -> false
          let isNotCloseBracket = (isCloseBracket >> not)

          while t |> Tokenizer.peekToken |> isNotCloseBracket do
            match Tokenizer.peekToken t with
            | Some (IntegerValue _) ->
                let nextValue, nextT = parseIntegerValue t
                t <- nextT
                currentList.Add(Option.get nextValue)
            | Some (Comma) ->
                let _, nextT = Tokenizer.nextToken t
                t <- nextT
            | Some (OpenBracket) ->
                let nextValue, nextT = parseListValue t
                t <- nextT
                currentList.Add(Option.get nextValue)
            | _ -> ()

          let closeBracket, nextT = Tokenizer.nextToken t
          if isCloseBracket closeBracket then
            let list = List.ofSeq currentList
            ((Some (PacketList list)), nextT)
          else
            (None, originalT)

      parseListValue t
    parsePacket (Tokenizer.fromString s) |> fst

let parseInput (input: string) =
  input.Split("\n\n")
  |> Array.map (fun block ->
                  let pairs = block.Split("\n")
                  (pairs[0], pairs[1]))
  |> Array.map (fun (left, right) -> (Packet.fromString left), (Packet.fromString right))
  |> Array.choose (function
                   | Some left, Some right -> Some (left, right)
                   | _ -> None)
  |> Array.toList

// I didn't really understand the puzzle instructions about comparisons so I ended up stealing this
// algorithm from Amons at https://fasterthanli.me/series/advent-of-code-2022/part-13
// I did, at least, translate it from Rust to F# so it's not _entirely_ plagarized
let rec compare (left: Packet) (right: Packet) =
  match left, right with
  | PacketValue left, PacketValue right -> (left :> System.IComparable<int>).CompareTo(right)
  | PacketValue _, PacketList _ -> compare (PacketList [left]) right
  | PacketList _, PacketValue _ -> compare left (PacketList [right])
  | PacketList left, PacketList right ->
        Seq.zip left right
        |> Seq.map (fun (left, right) -> compare left right)
        |> Seq.tryFind (fun c -> c <> 0)
        |> Option.defaultValue ((left.Length :> System.IComparable<int>).CompareTo(right.Length))

let inCorrectOrder (left, right) = (compare left right) < 0


//let inputPath = "day13.test.txt"
let inputPath = "day13.real.txt"

let input = System.IO.File.ReadAllText(inputPath) |> parseInput

// Part 1
input
|> List.mapi (fun i pair -> i, inCorrectOrder pair)
|> List.filter (fun (_, res) -> res)
|> List.map (fst >> ((+) 1))
|> List.sum
|> printf "Part 1 Sum of indexes in correct order: %d"

// Part 2
let unpairedList =
  input
  |> List.collect (fun (left, right) -> [left; right])

let dividerPacket2 = PacketList [ PacketList [ PacketValue 2]]
let dividerPacket6 = PacketList [ PacketList [ PacketValue 6]]

let sortedList =
  unpairedList
  |> List.append [ dividerPacket2; dividerPacket6 ]
  |> List.sortWith compare

let index2 =
  sortedList
  |> List.findIndex (fun i -> i = dividerPacket2)
  |> (+) 1

let index6 =
  sortedList
  |> List.findIndex (fun i -> i = dividerPacket6)
  |> (+) 1

printfn "Part 2 Sum of Key: %d" (index2 * index6)