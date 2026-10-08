module TypeCheck

open AST

// Symbol table is a mapping from 'Identifier' (string) to 'CType'. Note that
// 'Identifier' and 'Ctype' are defined in AST.fs file.
type SymbolTable = Map<Identifier,CType>

// For semantic analysis, you will need the following type definition. Note the
// difference between 'Ctype' and 'Typ': 'Ctype' represents the type annotated
// in C code, whereas 'Typ' represents the type identified during type checking.
type Typ = Int | Bool | NullPtr | IntPtr | BoolPtr | Error

// Convert 'CType' into 'Typ'.
let ctypeToTyp (ctype: CType) : Typ =
  match ctype with
  | CInt -> Int
  | CBool -> Bool
  | CIntPtr -> IntPtr
  | CBoolPtr -> BoolPtr

let areCompatible (t1: Typ) (t2: Typ) : bool =
  match t1, t2 with
  | _ when t1 = t2 -> true
  | IntPtr, NullPtr | NullPtr, IntPtr -> true
  | BoolPtr, NullPtr | NullPtr, BoolPtr -> true
  | _ -> false

let isValidCondition (t: Typ) : bool =
  match t with
  | Error -> false
  | _ -> true

// Check expression 'e' and return its type. If the type of expression cannot be
// decided due to some semantic error, return 'Error' as its type.
let rec checkExp (symTab: SymbolTable) (e: Exp) : Typ =
  match e with
  | Null -> NullPtr
  | Num _ -> Int
  | Boolean _ -> Bool
  // TODO: Fill in the remaining cases to complete the code.
  | Var x -> 
      match Map.tryFind x symTab with
      | Some ctyp -> ctypeToTyp ctyp
      | None -> Error
  | Deref x -> 
      match Map.tryFind x symTab with
      | Some CIntPtr -> Int
      | Some CBoolPtr -> Bool
      | _ -> Error
  | AddrOf x ->
      match Map.tryFind x symTab with
      | Some CInt -> IntPtr
      | Some CBool -> BoolPtr
      | _ -> Error
  | Neg sub -> 
      if checkExp symTab sub = Int then Int else Error
  | Add (e1, e2) | Sub (e1, e2) | Mul (e1, e2) | Div (e1, e2) ->
      match checkExp symTab e1, checkExp symTab e2 with
      | Int, Int -> Int
      | _ -> Error
  | Equal (e1, e2) | NotEq (e1, e2) ->
      let t1 = checkExp symTab e1
      let t2 = checkExp symTab e2
      if t1 <> Error && t2 <> Error && areCompatible t1 t2 then Bool else Error
  | LessEq (e1, e2) | LessThan (e1, e2) | GreaterEq (e1, e2) | GreaterThan (e1, e2) ->
      match checkExp symTab e1, checkExp symTab e2 with
      | Int, Int -> Bool
      | _ -> Error
  | And (e1, e2) | Or (e1, e2) ->
      match checkExp symTab e1, checkExp symTab e2 with
      | Bool, Bool -> Bool
      | _ -> Error
  | Not sub ->
      if checkExp symTab sub = Bool then Bool else Error

// Check statement 'stmt' and return a pair of (1) list of line numbers that
// contain semantic errors, and (2) symbol table updated by 'stmt'.
let rec checkStmt (symTab: SymbolTable) (retCTyp: CType) (stmt: Stmt) =
  match stmt with
  | Declare (line, ctyp, x) ->
      // If you think this statement is error-free, then return [] as error line
      // list. If you think it contains an error, you may return [line] instead.
      ([], Map.add x ctyp symTab)
  // TODO: Fill in the remaining cases to complete the code.
  | Define (line, ctyp, x, e) ->
      let tExp = checkExp symTab e
      let tVar = ctypeToTyp ctyp
      let errors = 
          if areCompatible tVar tExp then [] else [line]
      (errors, Map.add x ctyp symTab)

  | Assign (line, x, e) ->
      match Map.tryFind x symTab with
      | Some ctyp ->
          let tVar = ctypeToTyp ctyp
          let tExp = checkExp symTab e
          if areCompatible tVar tExp then ([], symTab) else ([line], symTab)
      | None -> ([line], symTab)

  | PtrUpdate (line, x, e) ->
      match Map.tryFind x symTab with
      | Some CIntPtr ->
          if checkExp symTab e = Int then ([], symTab) else ([line], symTab)
      | Some CBoolPtr ->
          if checkExp symTab e = Bool then ([], symTab) else ([line], symTab)
      | _ -> ([line], symTab)

  | Return (line, e) ->
      let tExp = checkExp symTab e
      let tRet = ctypeToTyp retCTyp
      if areCompatible tRet tExp then ([], symTab) else ([line], symTab)

  | If (line, e, s1, s2) ->
      let tCond = checkExp symTab e
      let condErr = if isValidCondition tCond then [] else [line]
      
      let s1Errs = checkStmts symTab retCTyp s1
      let s2Errs = checkStmts symTab retCTyp s2
      
      (condErr @ s1Errs @ s2Errs, symTab)

  | While (line, e, s) ->
      let tCond = checkExp symTab e
      let condErr = if isValidCondition tCond then [] else [line]
      let bodyErrs = checkStmts symTab retCTyp s
      
      (condErr @ bodyErrs, symTab)

// Check the statement list and return the line numbers of semantic errors. Note
// that 'checkStmt' and 'checkStmts' are mutually-recursive (they can call each
// other). This function design will make your code more concise.
and checkStmts symTab (retCTyp: CType) (stmts: Stmt list): LineNo list =
  match stmts with
  | [] -> []
  // TODO: Fill in this case to complete the code
  | stmt :: rest ->
      let (errs1, newSymTab) = checkStmt symTab retCTyp stmt
      let errs2 = checkStmts newSymTab retCTyp rest
      errs1 @ errs2

// Record the type of arguments to the symbol table.
let rec collectArgTypes argDecls symTab =
  match argDecls with
  | [] -> symTab
  // TODO: Fill in this case to complete the code
  | (ctyp, name) :: rest ->
      collectArgTypes rest (Map.add name ctyp symTab)

// Check the program and return the line numbers of semantic errors.
let run (prog: Program) : LineNo list =
  let (retCTyp, _, args, stmts) = prog
  let symTab = collectArgTypes args Map.empty
  let errorLines = checkStmts symTab retCTyp stmts
  // Remove duplicate entries and sort in ascending order.
  List.sort (List.distinct errorLines)