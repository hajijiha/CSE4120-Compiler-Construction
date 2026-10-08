module Optimize

open IR
open CFG
open DFA


module ConstantFolding =
  let foldConstant instr =
    match instr with
    | UnOp (r, NegOp, Imm x) -> (true, Set (r, Imm (-x)))
    | UnOp (r, NotOp, Imm x) -> (true, Set (r, Imm (if x = 0 then 1 else 0)))
    
    | BinOp (r, AddOp, Imm x, Imm y) -> (true, Set (r, Imm (x + y)))
    | BinOp (r, SubOp, Imm x, Imm y) -> (true, Set (r, Imm (x - y)))
    | BinOp (r, MulOp, Imm x, Imm y) -> (true, Set (r, Imm (x * y)))
    | BinOp (r, DivOp, Imm x, Imm y) when y <> 0 -> (true, Set (r, Imm (x / y)))
    | BinOp (r, EqOp, Imm x, Imm y) -> (true, Set (r, Imm (if x = y then 1 else 0)))
    | BinOp (r, NeqOp, Imm x, Imm y) -> (true, Set (r, Imm (if x <> y then 1 else 0)))
    | BinOp (r, LtOp, Imm x, Imm y) -> (true, Set (r, Imm (if x < y then 1 else 0)))
    | BinOp (r, LeqOp, Imm x, Imm y) -> (true, Set (r, Imm (if x <= y then 1 else 0)))
    | BinOp (r, GtOp, Imm x, Imm y) -> (true, Set (r, Imm (if x > y then 1 else 0)))
    | BinOp (r, GeqOp, Imm x, Imm y) -> (true, Set (r, Imm (if x >= y then 1 else 0)))
    
    | BinOp (r, AddOp, op, Imm 0) -> (true, Set (r, op)) 
    | BinOp (r, AddOp, Imm 0, op) -> (true, Set (r, op))
    | BinOp (r, SubOp, op, Imm 0) -> (true, Set (r, op))
    | BinOp (r, MulOp, op, Imm 1) -> (true, Set (r, op))
    | BinOp (r, MulOp, Imm 1, op) -> (true, Set (r, op))
    | BinOp (r, MulOp, _, Imm 0) -> (true, Set (r, Imm 0))
    | BinOp (r, MulOp, Imm 0, _) -> (true, Set (r, Imm 0))
    | BinOp (r, DivOp, op, Imm 1) -> (true, Set (r, op))
    
    | BinOp (r, SubOp, Reg r1, Reg r2) when r1 = r2 -> (true, Set (r, Imm 0))
    | BinOp (r, EqOp, Reg r1, Reg r2) when r1 = r2 -> (true, Set (r, Imm 1))
    | BinOp (r, NeqOp, Reg r1, Reg r2) when r1 = r2 -> (true, Set (r, Imm 0))
    | BinOp (r, LeqOp, Reg r1, Reg r2) when r1 = r2 -> (true, Set (r, Imm 1))
    | BinOp (r, GeqOp, Reg r1, Reg r2) when r1 = r2 -> (true, Set (r, Imm 1))
    | BinOp (r, LtOp, Reg r1, Reg r2) when r1 = r2 -> (true, Set (r, Imm 0))
    | BinOp (r, GtOp, Reg r1, Reg r2) when r1 = r2 -> (true, Set (r, Imm 0))
    
    | Set (r1, Reg r2) when r1 = r2 -> (true, Label "NOP")
    
    | _ -> (false, instr)

  let run instrs =
    let results = List.map foldConstant instrs
    let flags, newInstrs = List.unzip results
    let filtered = newInstrs |> List.filter (fun i -> i <> Label "NOP")
    (List.contains true flags || List.length filtered < List.length instrs, filtered)


module Mem2Reg =
  let run instrs =
    let scalarAllocs = 
      instrs 
      |> List.choose (fun i -> 
          match i with 
          | LocalAlloc (r, sz) when sz = 1 || sz = 4 -> Some (r, sz)
          | _ -> None)
      |> Map.ofList
    
    if Map.isEmpty scalarAllocs then (false, instrs)
    else
      let escaped = System.Collections.Generic.HashSet<Register>()
      
      instrs |> List.iter (fun instr ->
        let checkOp op =
          match op with
          | Reg r when Map.containsKey r scalarAllocs -> escaped.Add(r) |> ignore
          | _ -> ()
        match instr with
        | Set (_, op) -> checkOp op
        | UnOp (_, _, op) -> checkOp op
        | BinOp (_, _, op1, op2) -> checkOp op1; checkOp op2
        | StoreWord (op, _) | StoreByte (op, _) -> checkOp op
        | GotoIf (op, _) | GotoIfNot (op, _) -> checkOp op
        | Ret op -> checkOp op
        | _ -> ())
      
      let promotable = 
        scalarAllocs 
        |> Map.filter (fun r _ -> not (escaped.Contains(r)))
      
      if Map.isEmpty promotable then (false, instrs)
      else
        let mutable regCounter = 0
        let getNewReg () =
          regCounter <- regCounter + 1
          sprintf "_m2r_%d" regCounter
        
        let varRegs = promotable |> Map.map (fun _ _ -> getNewReg ())
        
        let transformInstr instr =
          match instr with
          | LocalAlloc (r, _) when Map.containsKey r promotable -> None
          | StoreWord (src, dest) when Map.containsKey dest promotable ->
              Some (Set (Map.find dest varRegs, src))
          | StoreByte (src, dest) when Map.containsKey dest promotable ->
              Some (Set (Map.find dest varRegs, src))
          | LoadWord (r, src) when Map.containsKey src promotable ->
              Some (Set (r, Reg (Map.find src varRegs)))
          | LoadByte (r, src) when Map.containsKey src promotable ->
              Some (Set (r, Reg (Map.find src varRegs)))
          | _ -> Some instr
        
        let newInstrs = instrs |> List.choose transformInstr
        (true, newInstrs)


module LocalValueNumbering =
  type BinExprKey = BinaryOp * Operand * Operand
  type UnExprKey = UnaryOp * Operand

  let run instrs =
    let rec loop remaining (regMap: Map<Register, Operand>) 
                          (binExprMap: Map<BinExprKey, Register>)
                          (unExprMap: Map<UnExprKey, Register>)
                          acc changed =
      match remaining with
      | [] -> (changed, List.rev acc)
      | instr :: tail ->
          let canonicalize op =
            match op with
            | Reg r -> Map.tryFind r regMap |> Option.defaultValue op
            | _ -> op
          
          let invalidateReg r (rMap: Map<Register, Operand>) (bMap: Map<BinExprKey, Register>) (uMap: Map<UnExprKey, Register>) =
            let rMap' = rMap |> Map.filter (fun _ v -> v <> Reg r)
            let bMap' = bMap |> Map.filter (fun (_, o1, o2) reg -> reg <> r && o1 <> Reg r && o2 <> Reg r)
            let uMap' = uMap |> Map.filter (fun (_, o1) reg -> reg <> r && o1 <> Reg r)
            (rMap', bMap', uMap')
          
          match instr with
          | Label _ ->
              loop tail Map.empty Map.empty Map.empty (instr :: acc) changed
          
          | Goto _ ->
              loop tail regMap binExprMap unExprMap (instr :: acc) changed
          
          | GotoIf (op, l) ->
              let newOp = canonicalize op
              let newInstr = GotoIf (newOp, l)
              loop tail Map.empty Map.empty Map.empty (newInstr :: acc) (changed || newOp <> op)
          
          | GotoIfNot (op, l) ->
              let newOp = canonicalize op
              let newInstr = GotoIfNot (newOp, l)
              loop tail Map.empty Map.empty Map.empty (newInstr :: acc) (changed || newOp <> op)
          
          | Ret op ->
              let newOp = canonicalize op
              let newInstr = Ret newOp
              loop tail regMap binExprMap unExprMap (newInstr :: acc) (changed || newOp <> op)
          
          | Set (r, op) ->
              let newOp = canonicalize op
              let newInstr = Set (r, newOp)
              let (rMap', bMap', uMap') = invalidateReg r regMap binExprMap unExprMap
              let rMap'' = Map.add r newOp rMap'
              loop tail rMap'' bMap' uMap' (newInstr :: acc) (changed || newOp <> op)
          
          | BinOp (r, op, op1, op2) ->
              let newOp1 = canonicalize op1
              let newOp2 = canonicalize op2
              
              let normKey : BinExprKey = 
                match op with
                | AddOp | MulOp | EqOp | NeqOp ->
                    if newOp1 > newOp2 then (op, newOp2, newOp1) else (op, newOp1, newOp2)
                | _ -> (op, newOp1, newOp2)
              
              match Map.tryFind normKey binExprMap with
              | Some prevReg ->
                  let newInstr = Set (r, Reg prevReg)
                  let (rMap', bMap', uMap') = invalidateReg r regMap binExprMap unExprMap
                  let rMap'' = Map.add r (Reg prevReg) rMap'
                  loop tail rMap'' bMap' uMap' (newInstr :: acc) true
              | None ->
                  let newInstr = BinOp (r, op, newOp1, newOp2)
                  let (rMap', bMap', uMap') = invalidateReg r regMap binExprMap unExprMap
                  let bMap'' = Map.add normKey r bMap'
                  let instrChanged = (newOp1 <> op1 || newOp2 <> op2)
                  loop tail rMap' bMap'' uMap' (newInstr :: acc) (changed || instrChanged)
          
          | UnOp (r, op, op1) ->
              let newOp1 = canonicalize op1
              let key : UnExprKey = (op, newOp1)
              
              match Map.tryFind key unExprMap with
              | Some prevReg ->
                  let newInstr = Set (r, Reg prevReg)
                  let (rMap', bMap', uMap') = invalidateReg r regMap binExprMap unExprMap
                  let rMap'' = Map.add r (Reg prevReg) rMap'
                  loop tail rMap'' bMap' uMap' (newInstr :: acc) true
              | None ->
                  let newInstr = UnOp (r, op, newOp1)
                  let (rMap', bMap', uMap') = invalidateReg r regMap binExprMap unExprMap
                  let uMap'' = Map.add key r uMap'
                  loop tail rMap' bMap' uMap'' (newInstr :: acc) (changed || newOp1 <> op1)
          
          | StoreWord (op, dest) ->
              let newOp = canonicalize op
              let newInstr = StoreWord (newOp, dest)
              loop tail regMap binExprMap unExprMap (newInstr :: acc) (changed || newOp <> op)
          
          | StoreByte (op, dest) ->
              let newOp = canonicalize op
              let newInstr = StoreByte (newOp, dest)
              loop tail regMap binExprMap unExprMap (newInstr :: acc) (changed || newOp <> op)
          
          | LoadWord (r, _) | LoadByte (r, _) | LocalAlloc (r, _) ->
              let (rMap', bMap', uMap') = invalidateReg r regMap binExprMap unExprMap
              loop tail rMap' bMap' uMap' (instr :: acc) changed
          
          | _ -> loop tail regMap binExprMap unExprMap (instr :: acc) changed
    
    loop instrs Map.empty Map.empty Map.empty [] false


module ConstantPropagation =
  let run instrs =
    if List.isEmpty instrs then (false, instrs)
    else
      let cfg = CFG.make instrs
      let rdMap = RDAnalysis.run cfg
      let instrArr = Array.ofList instrs

      let getConstVal idx =
        if idx >= 0 && idx < instrArr.Length then
          match instrArr.[idx] with
          | Set (_, Imm k) -> Some k
          | _ -> None
        else None

      let getDefReg idx =
        if idx >= 0 && idx < instrArr.Length then
          match instrArr.[idx] with
          | Set (r, _) | UnOp (r, _, _) | BinOp (r, _, _, _) 
          | LoadWord (r, _) | LoadByte (r, _) | LocalAlloc (r, _) -> Some r
          | _ -> None
        else None

      let findConstant reg lineNo =
        match Map.tryFind lineNo rdMap with
        | Some reachDefs ->
            let defsForReg = reachDefs |> Set.filter (fun idx -> getDefReg idx = Some reg)
            if Set.isEmpty defsForReg then None
            else
              let vals = defsForReg |> Set.toList |> List.choose getConstVal
              if List.length vals = Set.count defsForReg then
                match vals with
                | v :: rest when List.forall ((=) v) rest -> Some v
                | _ -> None
              else None
        | None -> None

      let replaceOp lineNo op =
        match op with
        | Reg r ->
            match findConstant r lineNo with
            | Some v -> (true, Imm v)
            | None -> (false, op)
        | _ -> (false, op)

      let optimizeInstr (lineNo, instr) =
        match instr with
        | Set (r, op) ->
            let c, newOp = replaceOp lineNo op
            (c, Set (r, newOp))
        | UnOp (r, op, op1) ->
            let c, newOp1 = replaceOp lineNo op1
            (c, UnOp (r, op, newOp1))
        | BinOp (r, op, op1, op2) ->
            let c1, newOp1 = replaceOp lineNo op1
            let c2, newOp2 = replaceOp lineNo op2
            (c1 || c2, BinOp (r, op, newOp1, newOp2))
        | StoreWord (src, dest) -> 
            let c, newSrc = replaceOp lineNo src
            (c, StoreWord (newSrc, dest))
        | StoreByte (src, dest) -> 
            let c, newSrc = replaceOp lineNo src
            (c, StoreByte (newSrc, dest))
        | GotoIf (op, l) ->
            let c, newOp = replaceOp lineNo op
            (c, GotoIf (newOp, l))
        | GotoIfNot (op, l) ->
            let c, newOp = replaceOp lineNo op
            (c, GotoIfNot (newOp, l))
        | Ret op ->
            let c, newOp = replaceOp lineNo op
            (c, Ret newOp)
        | _ -> (false, instr)

      let results = List.mapi (fun i instr -> optimizeInstr (i, instr)) instrs
      let flags, newInstrs = List.unzip results
      (List.contains true flags, newInstrs)


module GlobalCopyPropagation =
  let run instrs =
    if List.isEmpty instrs then (false, instrs)
    else
      let cfg = CFG.make instrs
      let rdMap = RDAnalysis.run cfg
      let instrArr = Array.ofList instrs

      let getCopySource idx =
        if idx >= 0 && idx < instrArr.Length then
          match instrArr.[idx] with
          | Set (_, Reg src) -> Some src
          | _ -> None
        else None

      let getDefReg idx =
        if idx >= 0 && idx < instrArr.Length then
          match instrArr.[idx] with
          | Set (r, _) | UnOp (r, _, _) | BinOp (r, _, _, _) 
          | LoadWord (r, _) | LoadByte (r, _) | LocalAlloc (r, _) -> Some r
          | _ -> None
        else None

      let findCopySource reg lineNo =
        match Map.tryFind lineNo rdMap with
        | Some reachDefs ->
            let defsForReg = reachDefs |> Set.filter (fun idx -> getDefReg idx = Some reg)
            if Set.count defsForReg <> 1 then None
            else
              let defIdx = Set.minElement defsForReg
              match getCopySource defIdx with
              | Some src ->
              
                  let srcDefs = reachDefs |> Set.filter (fun idx -> getDefReg idx = Some src)
                  let allBeforeCopyDef = srcDefs |> Set.forall (fun idx -> idx <= defIdx)
                  if allBeforeCopyDef then Some src
                  else None
              | None -> None
        | None -> None

      let replaceOp lineNo op =
        match op with
        | Reg r ->
            match findCopySource r lineNo with
            | Some src -> (true, Reg src)
            | None -> (false, op)
        | _ -> (false, op)

      let replaceReg lineNo r =
        match findCopySource r lineNo with
        | Some src -> (true, src)
        | None -> (false, r)

      let optimizeInstr (lineNo, instr) =
        match instr with
        | Set (r, op) ->
            let c, newOp = replaceOp lineNo op
            (c, Set (r, newOp))
        | UnOp (r, op, op1) ->
            let c, newOp1 = replaceOp lineNo op1
            (c, UnOp (r, op, newOp1))
        | BinOp (r, op, op1, op2) ->
            let c1, newOp1 = replaceOp lineNo op1
            let c2, newOp2 = replaceOp lineNo op2
            (c1 || c2, BinOp (r, op, newOp1, newOp2))
        | StoreWord (src, dest) -> 
            let c1, newSrc = replaceOp lineNo src
            let c2, newDest = replaceReg lineNo dest
            (c1 || c2, StoreWord (newSrc, newDest))
        | StoreByte (src, dest) -> 
            let c1, newSrc = replaceOp lineNo src
            let c2, newDest = replaceReg lineNo dest
            (c1 || c2, StoreByte (newSrc, newDest))
        | LoadWord (r, src) ->
            let c, newSrc = replaceReg lineNo src
            (c, LoadWord (r, newSrc))
        | LoadByte (r, src) ->
            let c, newSrc = replaceReg lineNo src
            (c, LoadByte (r, newSrc))
        | GotoIf (op, l) ->
            let c, newOp = replaceOp lineNo op
            (c, GotoIf (newOp, l))
        | GotoIfNot (op, l) ->
            let c, newOp = replaceOp lineNo op
            (c, GotoIfNot (newOp, l))
        | Ret op ->
            let c, newOp = replaceOp lineNo op
            (c, Ret newOp)
        | _ -> (false, instr)

      let results = List.mapi (fun i instr -> optimizeInstr (i, instr)) instrs
      let flags, newInstrs = List.unzip results
      (List.contains true flags, newInstrs)


module DeadCodeElimination =
  let run instrs =
    let usedRegs = System.Collections.Generic.HashSet<Register>()
    
    let addUse op =
      match op with
      | Reg r -> usedRegs.Add(r) |> ignore
      | _ -> ()

    let addRegUse r = usedRegs.Add(r) |> ignore

    instrs |> List.iter (fun instr ->
      match instr with
      | Set (_, op) -> addUse op
      | UnOp (_, _, op) -> addUse op
      | BinOp (_, _, op1, op2) -> addUse op1; addUse op2
      | LoadWord (_, src) | LoadByte (_, src) -> addRegUse src
      | StoreWord (op, dest) | StoreByte (op, dest) -> addUse op; addRegUse dest
      | GotoIf (op, _) | GotoIfNot (op, _) -> addUse op
      | Ret op -> addUse op
      | _ -> ())

    let isNeeded instr =
      match instr with
      | Set (r, _) | UnOp (r, _, _) | BinOp (r, _, _, _) 
      | LoadWord (r, _) | LoadByte (r, _) | LocalAlloc (r, _) -> 
          usedRegs.Contains(r)
      | _ -> true
    
    let newInstrs = List.filter isNeeded instrs
    (List.length newInstrs < List.length instrs, newInstrs)


module BranchSimplification =
  let run instrs =
    let simplify instr =
      match instr with
      | GotoIf (Imm v, l) -> 
          if v <> 0 then (true, Goto l) else (true, Label "NOP")
      | GotoIfNot (Imm v, l) -> 
          if v = 0 then (true, Goto l) else (true, Label "NOP")
      | _ -> (false, instr)
    
    let results = List.map simplify instrs
    let flags, newInstrs = List.unzip results
    let filtered = newInstrs |> List.filter (fun i -> i <> Label "NOP")
    (List.contains true flags, filtered)


module ControlFlowSimplification =
  let run instrs =
    let usedLabels = 
      instrs 
      |> List.choose (fun i ->
          match i with
          | Goto l | GotoIf (_, l) | GotoIfNot (_, l) -> Some l
          | _ -> None)
      |> Set.ofList
    
    let step1 = instrs |> List.filter (fun instr ->
      match instr with
      | Label l -> l = "" || Set.contains l usedLabels
      | _ -> true)
    
    let instrArr = Array.ofList step1
    let len = instrArr.Length
    
    let labelPos = System.Collections.Generic.Dictionary<string, int>()
    for i = 0 to len - 1 do
      match instrArr.[i] with
      | Label l when l <> "" -> labelPos.[l] <- i
      | _ -> ()
    
    let step2 = 
      step1 
      |> List.indexed 
      |> List.filter (fun (i, instr) ->
          match instr with
          | Goto l ->
              if labelPos.ContainsKey(l) then labelPos.[l] <> i + 1
              else true
          | _ -> true)
      |> List.map snd
    
    (List.length step2 < List.length instrs, step2)


let rec optimizeLoop instrs iteration =
  if iteration > 100 then instrs
  else
    let c1, instrs = Mem2Reg.run instrs
    let c2, instrs = LocalValueNumbering.run instrs
    let c3, instrs = ConstantFolding.run instrs
    let c4, instrs = ConstantPropagation.run instrs
    let c5, instrs = GlobalCopyPropagation.run instrs
    let c6, instrs = LocalValueNumbering.run instrs
    let c7, instrs = ConstantFolding.run instrs
    let c8, instrs = BranchSimplification.run instrs
    let c9, instrs = DeadCodeElimination.run instrs
    let c10, instrs = ControlFlowSimplification.run instrs
    
    if c1 || c2 || c3 || c4 || c5 || c6 || c7 || c8 || c9 || c10 then 
      optimizeLoop instrs (iteration + 1) 
    else 
      instrs

let run (ir: IRCode) : IRCode =
  let (fname, args, instrs) = ir
  let optimized = optimizeLoop instrs 0
  (fname, args, optimized)