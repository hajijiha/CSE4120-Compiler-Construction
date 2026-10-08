module DFA

open IR
open CFG


type RDSet = Set<int>

module RDAnalysis =

  
  let getDefReg instr =
    match instr with
    | Set (r, _) | UnOp (r, _, _) | BinOp (r, _, _, _) 
    | LoadWord (r, _) | LoadByte (r, _) | LocalAlloc (r, _) -> Some r
    | _ -> None

  let run (cfg: CFG) : Map<int,RDSet> =
    let allNodes = CFG.getAllNodes cfg

    
    let globalDefs =
      allNodes
      |> List.fold (fun acc nodeID ->
          let instr = CFG.getInstr nodeID cfg
          match getDefReg instr with
          | Some r ->
              let oldSet = Map.tryFind r acc |> Option.defaultValue Set.empty
              Map.add r (Set.add nodeID oldSet) acc
          | None -> acc
      ) Map.empty

   
    let nodeGenKill =
      allNodes
      |> List.map (fun nodeID ->
          let instr = CFG.getInstr nodeID cfg
          let gen, kill =
            match getDefReg instr with
            | Some r ->
                let allDefs = Map.tryFind r globalDefs |> Option.defaultValue Set.empty
                let currentGen = Set.singleton nodeID
                let currentKill = Set.remove nodeID allDefs
                (currentGen, currentKill)
            | None -> (Set.empty, Set.empty)
          (nodeID, (gen, kill))
      )
      |> Map.ofList

   
    let initIn = allNodes |> List.map (fun id -> id, Set.empty) |> Map.ofList
    let initOut = allNodes |> List.map (fun id -> id, Set.empty) |> Map.ofList

   
    let rec solve inMaps outMaps =
      let foldF (changed, currIn, currOut) nodeID =
        let preds = CFG.getPreds nodeID cfg
        let newIn =
          match preds with
          | [] -> Set.empty
          | _ ->
              preds
              |> List.map (fun p -> Map.find p currOut)
              |> List.reduce Set.union
        
        let (gen, kill) = Map.find nodeID nodeGenKill
        let newOut = Set.union gen (Set.difference newIn kill)
        
        let oldOut = Map.find nodeID currOut
        if oldOut <> newOut then
          (true, Map.add nodeID newIn currIn, Map.add nodeID newOut currOut)
        else
          (changed, Map.add nodeID newIn currIn, currOut)

      let (changed, nextIn, nextOut) =
        allNodes
        |> List.fold foldF (false, inMaps, outMaps)
      
      if changed then solve nextIn nextOut else nextIn

    solve initIn initOut