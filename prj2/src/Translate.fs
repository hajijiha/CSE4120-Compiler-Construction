module Translate

open AST
open IR
open Helper

// Symbol table is a mapping from identifier to a pair of register and type.
// Register recorded here will be containg the address of that variable.
type SymbolTable = Map<Identifier,Register * CType>

// Let's assume the following size for each data type.
let sizeof (ctyp: CType) =
  match ctyp with
  | CInt -> 4
  | CBool -> 1
  | CIntPtr | CBoolPtr -> 4
  | CIntArr n -> 4 * n
  | CBoolArr n -> n

let rec getTyp symtab e =
  match e with
  | Null -> CIntPtr
  | Num _ -> CInt
  | Boolean _ -> CBool
  | Var x -> 
      match snd (Map.find x symtab) with
      | CIntArr _ -> CIntPtr
      | CBoolArr _ -> CBoolPtr
      | t -> t
  | AddrOf x ->
      match snd (Map.find x symtab) with
      | CInt -> CIntPtr
      | CBool -> CBoolPtr
      | CIntArr _ -> CIntPtr
      | CBoolArr _ -> CBoolPtr
      | _ -> CIntPtr
  | Deref e ->
      match getTyp symtab e with
      | CIntPtr -> CInt
      | CBoolPtr -> CBool
      | _ -> CInt
  | Arr (x, _) ->
      match snd (Map.find x symtab) with
      | CIntArr _ | CIntPtr -> CInt
      | CBoolArr _ | CBoolPtr -> CBool
      | _ -> CInt
  | Add (e1, e2) ->
      let t1 = getTyp symtab e1
      let t2 = getTyp symtab e2
      if t1 = CIntPtr || t1 = CBoolPtr then t1
      elif t2 = CIntPtr || t2 = CBoolPtr then t2
      else CInt
  | Sub (e1, _) -> getTyp symtab e1
  | _ -> CInt

let rec transExp (symtab: SymbolTable) (e: Exp) : Register * Instr list =
  match e with
  | Null ->
      let r = createRegName ()
      (r, [Set (r, Imm 0)])
  | Num i ->
      let r = createRegName ()
      (r, [Set (r, Imm i)])
  // TODO: Fill in the remaining cases to complete the code.
  | Boolean b ->
      let r = createRegName ()
      let v = if b then 1 else 0
      (r, [Set (r, Imm v)])
  | Var x ->
      let (addr, typ) = Map.find x symtab
      match typ with
      | CIntArr _ | CBoolArr _ -> (addr, [])
      | _ ->
          let r = createRegName ()
          let instr =
            if sizeof typ = 4 then LoadWord (r, addr)
            else LoadByte (r, addr)
          (r, [instr])
  | AddrOf x ->
      let (addr, _) = Map.find x symtab
      (addr, [])
  | Deref e ->
      let (rAddr, instrs) = transExp symtab e
      let r = createRegName ()
      let typ = getTyp symtab e
      let loadInstr =
        match typ with
        | CIntPtr -> LoadWord (r, rAddr)
        | CBoolPtr -> LoadByte (r, rAddr)
        | _ -> LoadWord (r, rAddr)
      (r, instrs @ [loadInstr])
  | Arr (x, eIdx) ->
      let (addr, typ) = Map.find x symtab
      let (rIdx, idxInstrs) = transExp symtab eIdx
      let (elemSize, isPtr) =
        match typ with
        | CIntArr _ -> 4, false
        | CBoolArr _ -> 1, false
        | CIntPtr -> 4, true
        | CBoolPtr -> 1, true
        | _ -> 4, false
      
      let rBase = createRegName ()
      let rOffset = createRegName ()
      let rElemAddr = createRegName ()
      let rres = createRegName ()
      
      let baseInstr = 
        if isPtr then LoadWord (rBase, addr)
        else Set (rBase, Reg addr)
      
      let instrs =
        idxInstrs @
        [baseInstr;
         BinOp (rOffset, MulOp, Reg rIdx, Imm elemSize);
         BinOp (rElemAddr, AddOp, Reg rBase, Reg rOffset)] @
        [if elemSize = 4 then LoadWord (rres, rElemAddr) else LoadByte (rres, rElemAddr)]
      (rres, instrs)
  | Neg e ->
      let (r, instrs) = transExp symtab e
      let rres = createRegName ()
      (rres, instrs @ [UnOp (rres, NegOp, Reg r)])
  | Not e ->
      let (r, instrs) = transExp symtab e
      let rres = createRegName ()
      (rres, instrs @ [UnOp (rres, NotOp, Reg r)])
  | Add (e1, e2) ->
      let (r1, i1) = transExp symtab e1
      let (r2, i2) = transExp symtab e2
      let t1 = getTyp symtab e1
      let t2 = getTyp symtab e2
      let r = createRegName ()
      let rScaled = createRegName ()
      
      match t1, t2 with
      | (CIntPtr, CInt) | (CIntPtr, CBool) -> 
          (r, i1 @ i2 @ [BinOp (rScaled, MulOp, Reg r2, Imm 4); BinOp (r, AddOp, Reg r1, Reg rScaled)])
      | (CBoolPtr, CInt) | (CBoolPtr, CBool) -> 
          (r, i1 @ i2 @ [BinOp (rScaled, MulOp, Reg r2, Imm 1); BinOp (r, AddOp, Reg r1, Reg rScaled)])
      | (CInt, CIntPtr) | (CBool, CIntPtr) -> 
          (r, i1 @ i2 @ [BinOp (rScaled, MulOp, Reg r1, Imm 4); BinOp (r, AddOp, Reg rScaled, Reg r2)])
      | (CInt, CBoolPtr) | (CBool, CBoolPtr) -> 
          (r, i1 @ i2 @ [BinOp (rScaled, MulOp, Reg r1, Imm 1); BinOp (r, AddOp, Reg rScaled, Reg r2)])
      | _ -> 
          (r, i1 @ i2 @ [BinOp (r, AddOp, Reg r1, Reg r2)])
  | Sub (e1, e2) ->
      let (r1, i1) = transExp symtab e1
      let (r2, i2) = transExp symtab e2
      let t1 = getTyp symtab e1
      let r = createRegName ()
      let rScaled = createRegName ()
      
      match t1 with
      | CIntPtr -> 
          (r, i1 @ i2 @ [BinOp (rScaled, MulOp, Reg r2, Imm 4); BinOp (r, SubOp, Reg r1, Reg rScaled)])
      | CBoolPtr -> 
          (r, i1 @ i2 @ [BinOp (rScaled, MulOp, Reg r2, Imm 1); BinOp (r, SubOp, Reg r1, Reg rScaled)])
      | _ -> 
          (r, i1 @ i2 @ [BinOp (r, SubOp, Reg r1, Reg r2)])
  | Mul (e1, e2) ->
      let (r1, i1) = transExp symtab e1
      let (r2, i2) = transExp symtab e2
      let r = createRegName ()
      (r, i1 @ i2 @ [BinOp (r, MulOp, Reg r1, Reg r2)])
  | Div (e1, e2) ->
      let (r1, i1) = transExp symtab e1
      let (r2, i2) = transExp symtab e2
      let r = createRegName ()
      (r, i1 @ i2 @ [BinOp (r, DivOp, Reg r1, Reg r2)])
  | Equal (e1, e2) ->
      let (r1, i1) = transExp symtab e1
      let (r2, i2) = transExp symtab e2
      let r = createRegName ()
      (r, i1 @ i2 @ [BinOp (r, EqOp, Reg r1, Reg r2)])
  | NotEq (e1, e2) ->
      let (r1, i1) = transExp symtab e1
      let (r2, i2) = transExp symtab e2
      let r = createRegName ()
      (r, i1 @ i2 @ [BinOp (r, NeqOp, Reg r1, Reg r2)])
  | LessEq (e1, e2) ->
      let (r1, i1) = transExp symtab e1
      let (r2, i2) = transExp symtab e2
      let r = createRegName ()
      (r, i1 @ i2 @ [BinOp (r, LeqOp, Reg r1, Reg r2)])
  | LessThan (e1, e2) ->
      let (r1, i1) = transExp symtab e1
      let (r2, i2) = transExp symtab e2
      let r = createRegName ()
      (r, i1 @ i2 @ [BinOp (r, LtOp, Reg r1, Reg r2)])
  | GreaterEq (e1, e2) ->
      let (r1, i1) = transExp symtab e1
      let (r2, i2) = transExp symtab e2
      let r = createRegName ()
      (r, i1 @ i2 @ [BinOp (r, GeqOp, Reg r1, Reg r2)])
  | GreaterThan (e1, e2) ->
      let (r1, i1) = transExp symtab e1
      let (r2, i2) = transExp symtab e2
      let r = createRegName ()
      (r, i1 @ i2 @ [BinOp (r, GtOp, Reg r1, Reg r2)])
  | And (e1, e2) ->
      let (r1, i1) = transExp symtab e1
      let (r2, i2) = transExp symtab e2
      let r = createRegName ()
      let labelFalse = createLabel ()
      let labelEnd = createLabel ()
      let instrs = 
        i1 @ 
        [GotoIfNot (Reg r1, labelFalse)] @ 
        i2 @ 
        [GotoIfNot (Reg r2, labelFalse); 
         Set (r, Imm 1); 
         Goto labelEnd; 
         Label labelFalse; 
         Set (r, Imm 0); 
         Label labelEnd]
      (r, instrs)
  | Or (e1, e2) ->
      let (r1, i1) = transExp symtab e1
      let (r2, i2) = transExp symtab e2
      let r = createRegName ()
      let labelTrue = createLabel ()
      let labelEnd = createLabel ()
      let instrs = 
        i1 @ 
        [GotoIf (Reg r1, labelTrue)] @ 
        i2 @ 
        [GotoIf (Reg r2, labelTrue); 
         Set (r, Imm 0); 
         Goto labelEnd; 
         Label labelTrue; 
         Set (r, Imm 1); 
         Label labelEnd]
      (r, instrs)

let rec transStmt (symtab: SymbolTable) stmt : SymbolTable * Instr list =
  match stmt with
  | Declare (_, typ, vname) ->
      let r = createRegName ()
      let size = sizeof typ
      let symtab = Map.add vname (r, typ) symtab
      (symtab, [LocalAlloc (r, size)])
  // TODO: Fill in the remaining cases to complete the code.
  | Define (_, typ, vname, e) ->
      let r = createRegName ()
      let size = sizeof typ
      let symtab = Map.add vname (r, typ) symtab
      let (re, instrs) = transExp symtab e
      let storeInstr =
        if size = 4 then StoreWord (Reg re, r)
        else StoreByte (Reg re, r)
      (symtab, [LocalAlloc (r, size)] @ instrs @ [storeInstr])
  | Assign (_, vname, e) ->
      let (addr, typ) = Map.find vname symtab
      let (re, instrs) = transExp symtab e
      let storeInstr =
        if sizeof typ = 4 then StoreWord (Reg re, addr)
        else StoreByte (Reg re, addr)
      (symtab, instrs @ [storeInstr])
  | PtrUpdate (_, e1, e2) ->
      let (r1, i1) = transExp symtab e1
      let (r2, i2) = transExp symtab e2
      
      let ptrTyp = getTyp symtab e1
      let storeInstr =
        match ptrTyp with
        | CBoolPtr -> StoreByte (Reg r2, r1)
        | _ -> StoreWord (Reg r2, r1) 
      
      (symtab, i1 @ i2 @ [storeInstr])
  | ArrUpdate (_, vname, eIdx, eVal) ->
      let (addr, typ) = Map.find vname symtab
      let (rIdx, idxInstrs) = transExp symtab eIdx
      let (rVal, valInstrs) = transExp symtab eVal
      let (elemSize, isPtr) =
        match typ with
        | CIntArr _ -> 4, false
        | CBoolArr _ -> 1, false
        | CIntPtr -> 4, true
        | CBoolPtr -> 1, true
        | _ -> 4, false
      
      let rBase = createRegName ()
      let rOffset = createRegName ()
      let rElemAddr = createRegName ()
      
      let baseInstr = 
        if isPtr then LoadWord (rBase, addr) 
        else Set (rBase, Reg addr)

      let instrs =
        idxInstrs @ valInstrs @
        [baseInstr;
         BinOp (rOffset, MulOp, Reg rIdx, Imm elemSize);
         BinOp (rElemAddr, AddOp, Reg rBase, Reg rOffset)] @
        [if elemSize = 4 then StoreWord (Reg rVal, rElemAddr) else StoreByte (Reg rVal, rElemAddr)]
      (symtab, instrs)
  | Return (_, e) ->
      let (r, instrs) = transExp symtab e
      (symtab, instrs @ [Ret (Reg r)])
  | If (_, e, s1, s2) ->
      let (r, iExp) = transExp symtab e
      let iThen = transStmts symtab s1
      let iElse = transStmts symtab s2
      let labelElse = createLabel ()
      let labelEnd = createLabel ()
      let instrs =
        iExp @
        [GotoIfNot (Reg r, labelElse)] @
        iThen @
        [Goto labelEnd; Label labelElse] @
        iElse @
        [Label labelEnd]
      (symtab, instrs)
  | While (_, e, s) ->
      let labelStart = createLabel ()
      let labelEnd = createLabel ()
      let (r, iExp) = transExp symtab e
      let iBody = transStmts symtab s
      let instrs =
        [Label labelStart] @
        iExp @
        [GotoIfNot (Reg r, labelEnd)] @
        iBody @
        [Goto labelStart; Label labelEnd]
      (symtab, instrs)

and transStmts symtab stmts: Instr list =
  match stmts with
  | [] -> []
  | headStmt :: tailStmts ->
      let symtab, instrs = transStmt symtab headStmt
      instrs @ transStmts symtab tailStmts

// This code allocates memory for each argument and records information to the
// symbol table. Note that argument can be handled similarly to local variable.
let rec transArgs accSymTab accInstrs args =
  match args with
  | [] -> accSymTab, accInstrs
  | headArg :: tailArgs ->
      // In our IR model, register 'argName' is already defined at the entry.
      let (argTyp, argName) = headArg
      // Allocate memory space and store the argument value there.
      let r = createRegName ()
      let size = sizeof argTyp
      let allocInstr = LocalAlloc (r, size)
      let storeInstr =
        if size = 4 then StoreWord (Reg argName, r)
        else StoreByte (Reg argName, r)
      // From now on, we can use 'r' as a pointer to access 'argName'.
      let accSymTab = Map.add argName (r, argTyp) accSymTab
      let accInstrs = accInstrs @ [allocInstr; storeInstr]
      transArgs accSymTab accInstrs tailArgs

// Translate input program into IR code.
let run (prog: Program) : IRCode =
  let (_, fname, args, stmts) = prog
  let argRegs = List.map snd args
  let symtab, argInstrs = transArgs Map.empty [] args
  let bodyInstrs = transStmts symtab stmts
  (fname, argRegs, argInstrs @ bodyInstrs)