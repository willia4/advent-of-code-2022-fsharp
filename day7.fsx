open System

type PathComponents =
  | PathComponents of (string list)

module PathComponents =
  let private stringIsRoot s = s = "/"
  let private stringIsNotRoot = stringIsRoot >> not

  let fromString (path: string) =
    path.Split("/")
    |> Array.filter (fun s -> (System.String.IsNullOrWhiteSpace(s)) |> not)
    |> Array.insertAt 0 "/"
    |> Array.toList
    |> PathComponents

  let toString (components: PathComponents) =
    match components with
    | PathComponents pathComponents ->
        let r =
          pathComponents
          |> List.filter stringIsNotRoot
          |> String.concat "/"
        r.Insert(0, "/")

  let toList components =
    match components with
    | PathComponents pathComponents -> pathComponents

  let pushComponent newComponent (components: PathComponents) =
    match components with
    | PathComponents components ->
        components
        |> List.rev
        |> List.append [ newComponent ]
        |> List.rev
        |> PathComponents

  let popComponent (components: PathComponents) =
    match components with
    | PathComponents components ->
        let revComponents = List.rev components
        let poppedItem =
          List.tryHead revComponents
          |> Option.bind (fun item -> if stringIsNotRoot item then Some item else None)

        let r =
          components
          |> List.rev
          |> List.removeAt 0
          |> List.rev
        if (List.isEmpty r) then
          poppedItem, fromString "/"
        else
          poppedItem, PathComponents r

  let itemName components =
    match components with
    | PathComponents components ->
        match (List.rev components) with
        | head :: [] -> ""
        | head :: rest -> head
        | [] -> ""

  let length components =
    match components with
    | PathComponents components -> (List.length components) - 1

  let isEmpty components = (length components) = 0
  let isRoot = isEmpty

type TreeNode =
  | File of string * PathComponents * int
  | Directory of string * PathComponents * (TreeNode list)

module TreeNode =
  let isFile node =
    match node with
    | File _ -> true
    | _ -> false

  let isDirectory node =
    match node with
    | Directory _ -> true
    | _ -> false

  let isRoot node =
    match node with
    | Directory (_, path, _) -> (PathComponents.isRoot path)
    | _ -> false

  let children node =
    match node with
    | Directory (_, _, children) -> children
    | _ -> []

  let hasChildren node = (not (List.isEmpty (children node)))

  let nodePath node =
    match node with
    | Directory (_, path, _) -> path
    | File (_, path, _) -> path

  let nodeParentPath node =
    let path = nodePath node
    path |> PathComponents.popComponent |> snd

  let nodeName node =
    match node with
    | Directory (name, _, _) -> name
    | File (name, _, _) -> name

  let rec walkTree root = seq {
    yield root

    let children = children root
    for c in children do
      yield! (walkTree c)
  }

  let findParent child root=
    if (isRoot child) then None
    else
      let parentPath = (nodeParentPath child)
      walkTree root
      |> Seq.tryFind (fun potentialParent -> (nodePath potentialParent) = parentPath)

  let rec addChild parent newChild root =
    let simpleAddChild parent newChild =
      let newChildPath = nodePath newChild

      match parent with
      | Directory (name, path, existingChildren) ->
          let childrenWithoutNewChild = existingChildren |> List.filter (fun e -> (nodePath e) <> newChildPath)
          let childrenWithNewChild = (List.append childrenWithoutNewChild [newChild]) |> List.sortBy nodeName

          Directory (name, path, childrenWithNewChild)
      | _ -> raise (Exception (sprintf "Could not add child to non-directory %A" parent))

    if (isRoot parent) then
      simpleAddChild parent newChild
    else
      match findParent parent root with
      | Some parentParent ->
        let newParent = simpleAddChild parent newChild
        addChild parentParent newParent root
      | None -> raise (Exception (sprintf "Could not find parent of non-root %A" parent))

  let navigateByPathComponents pathComponents root =
    let rec inner pathComponentsList current =
      if ((List.length pathComponentsList) <= 1) then
        current
      else
        let nextComponentName = List.item 1 pathComponentsList
        match current with
        | Directory (name, path, children) ->
          match children |> List.tryFind (fun c -> (nodeName c) = nextComponentName) with
          | Some nextChild -> inner (List.removeAt 0 pathComponentsList) nextChild
          | None -> raise (Exception (sprintf "Could not find child %s in directory %s" nextComponentName (PathComponents.toString path)))
        | File (_, path, _) -> raise (Exception (sprintf "Could not traverse into file at %s" (PathComponents.toString path)))

    inner (pathComponents |> PathComponents.toList) root

  let navigate pathString root = navigateByPathComponents (PathComponents.fromString pathString) root

  let toDebugString root =
    walkTree root
    |> Seq.map (fun n ->
        match n with
        | File (_, path, _) -> path |> PathComponents.toString
        | Directory (_, path, _) -> $"{path |> PathComponents.toString}/")
    |> Seq.sort
    |> String.concat "\n"

  let rec itemSize item =
    match item with
    | File (_, _, size) -> size
    | Directory (_, _, children) ->
        children
        |> List.map itemSize
        |> List.sum



let makeDirectory path = Directory ((PathComponents.itemName path), path, [])
let makeFile path size = File ((PathComponents.itemName path), path, size)

let makeDirectoryForParent parent name =
  let parentPath = TreeNode.nodePath parent
  let itemPath = PathComponents.pushComponent name parentPath
  makeDirectory itemPath

let  makeFileForParent parent name size =
  let parentPath = TreeNode.nodePath parent
  let itemPath = PathComponents.pushComponent name parentPath
  makeFile itemPath size

let emptyRoot = makeDirectory (PathComponents.fromString "/")

type State = {
  CurrentDirectory: PathComponents
  TreeRoot: TreeNode
  RemainingInput: string list
}

let defaultState = { CurrentDirectory = PathComponents.fromString "/"; TreeRoot = emptyRoot; RemainingInput = []}

type Command =
  | ListFiles
  | ChangeDirectoryRoot
  | ChangeDirectoryUp
  | ChangeDirectory of string

let parseCommand (commandString: string) =
  let commandString = commandString.Trim()

  if commandString = "$ ls" then
    ListFiles
  elif commandString = "$ cd /" then
    ChangeDirectoryRoot
  elif commandString = "$ cd .." then
    ChangeDirectoryUp
  else
    let directoryString = commandString.Replace("$ cd ", "")
    ChangeDirectory directoryString

let processChangeDirectoryUpCommand state =
  let newPath = state.CurrentDirectory |> PathComponents.popComponent |> snd
  { state with CurrentDirectory = newPath }

let processChangeDirectoryRootCommand state =
  { state with CurrentDirectory = (PathComponents.fromString "/") }

let processChangeDirectoryCommand state newDirectory =
  let newPath = state.CurrentDirectory |> PathComponents.pushComponent newDirectory
  {state with CurrentDirectory = newPath}

let processListFilesCommand state (input: string list) =
  let fileMatcher = new System.Text.RegularExpressions.Regex("(\\d+?) (.*)")
  let dirMatcher = new System.Text.RegularExpressions.Regex("dir (.*)")

  let state =
    input
    |> List.fold (
        fun state directoryItem ->
          let currentDirectoryNode = state.TreeRoot |> TreeNode.navigateByPathComponents state.CurrentDirectory
          match (fileMatcher.Match(directoryItem)), (dirMatcher.Match(directoryItem)) with
          | f, _ when f.Success ->
            let fileName = f.Groups[2].Value;
            let fileSize = f.Groups[1].Value |> System.Int32.Parse;
            let newFile = makeFileForParent currentDirectoryNode fileName fileSize
            let newRoot = TreeNode.addChild currentDirectoryNode newFile state.TreeRoot

            {state with TreeRoot = newRoot }
          | _, d when d.Success ->
            let dirName = d.Groups[1].Value;
            let newDir = makeDirectoryForParent currentDirectoryNode dirName
            let newRoot = TreeNode.addChild currentDirectoryNode newDir state.TreeRoot

            {state with TreeRoot = newRoot }
          | _ -> raise (Exception (sprintf "Could not process ls input item %s" directoryItem))
        ) state
  state

let processCommand (state: State) =
  let nextCommand =
    let nextCommand = state.RemainingInput |> List.head
    if (not (nextCommand.StartsWith("$ ")))
      then raise (Exception (sprintf "Expected command but got %s" nextCommand))
      else parseCommand nextCommand

  let nextInput =
    state.RemainingInput
    |> List.skip 1
    |> List.takeWhile (fun s -> not (s.StartsWith("$ ")))

  let remainingInput =
    state.RemainingInput
    |> List.skip (1 + (List.length nextInput))

  let state = { state with RemainingInput = remainingInput }

  match nextCommand with
  | ChangeDirectoryUp -> processChangeDirectoryUpCommand state
  | ChangeDirectoryRoot -> processChangeDirectoryRootCommand state
  | ChangeDirectory newDirectory -> processChangeDirectoryCommand state newDirectory
  | ListFiles -> processListFilesCommand state nextInput

let processCommands (state: State) =
  let mutable state = state
  while not (state.RemainingInput |> List.isEmpty) do
    state <- processCommand state
  state

//let inputPath = "day7.test.txt"
let inputPath = "day7.real.txt"
let inputLines = System.IO.File.ReadAllLines(inputPath) |> List.ofArray

let state = { defaultState with RemainingInput = inputLines}

// Part 1
processCommands state
|> (fun state -> state.TreeRoot)
|> TreeNode.walkTree
|> Seq.filter TreeNode.isDirectory
|> Seq.map TreeNode.itemSize
|> Seq.filter (fun s -> s <= 100000)
|> Seq.sum
|> printfn "Part 1 Answer: %d"

// Part 2
let totalSpace = 70000000
let requiredSpace = 30000000


let processedTree =
  processCommands state
  |> (fun state -> state.TreeRoot)

let totalUsed =
  processedTree
  |> TreeNode.navigate "/"
  |> TreeNode.itemSize

let totalUnused = totalSpace - totalUsed
let needToDelete = requiredSpace - totalUnused

processedTree
|> TreeNode.walkTree
|> Seq.filter TreeNode.isDirectory
|> Seq.map TreeNode.itemSize
|> Seq.sort
|> Seq.find (fun s -> s >= needToDelete)
|> printfn "Part 2 Answer: %d"