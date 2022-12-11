type Range = {
  First: int
  Last: int
}

let fullyContains (a, b) =
  (a.First >= b.First && a.Last <= b.Last) || (b.First >= a.First && b.Last <= a.Last)

let overlaps (a, b) =
  (a.First >= b.First && a.First <= b.Last) || (a.Last >= b.First && a.Last <= b.Last) ||
  (b.First >= a.First && b.First <= a.Last) || (b.Last >= a.First && b.Last <= a.Last)

let parseLine (line: string)=
  let regex = new System.Text.RegularExpressions.Regex("(\\d*?)-(\\d*?),(\\d*?)-(\\d*)")
  let m = regex.Match(line)

  let a = System.Int32.Parse(m.Groups[1].Value)
  let b = System.Int32.Parse(m.Groups[2].Value)
  let c = System.Int32.Parse(m.Groups[3].Value)
  let d = System.Int32.Parse(m.Groups[4].Value)

  {First = a; Last = b}, {First = c; Last = d}

// let inputPath = "day4.test.txt"
let inputPath = "day4.real.txt"

let input =
  System.IO.File.ReadAllLines(inputPath)
  |> Seq.map parseLine

// Part 1
input
|> Seq.filter fullyContains
|> Seq.length
|> printfn "Part 1 Fully Contains: %d"

// Part 2
input
|> Seq.filter overlaps
|> Seq.length
|> printfn "Part 2 Overlaps: %d"