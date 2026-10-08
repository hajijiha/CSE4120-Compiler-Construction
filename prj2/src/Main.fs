open FSharp.Text.Lexing
open AST
open IR

let checkArgValid argv =
  let argNum = Array.length argv
  argNum <> 0 &&
  match argv[0] with
  | "print-ast" -> argNum = 2
  | "print-ir" -> argNum = 2
  | "run-ir" -> argNum = 3
  | _ -> false

let usage () =
  let _ = printfn "<Usage>"
  let _ = printfn "[*] ./IRGenerate print-ast <source file>"
  let _ = printfn "[*] ./IRGenerate print-ir <source file>"
  let _ = printfn "[*] ./IRGenerate run-ir <source file> <input file>"
  exit 1

let lex (file: string): LexBuffer<_> =
  try
    let streamReader = new System.IO.StreamReader(file)
    let lexbuf = LexBuffer<_>.FromTextReader streamReader
    lexbuf.EndPos <- { lexbuf.EndPos with pos_lnum = 1 }
    lexbuf
  with :? System.IO.IOException ->
    printfn "[*] Failed to open file '%s'" file
    exit 1

let parse (lexbuf: LexBuffer<_>) : Program =
  try Parser.prog Lexer.token lexbuf
  with _ ->
    printfn "[*] Parsing error at line %d" (lexbuf.EndPos.Line)
    exit 1

// Read each line in 'filepath' and use it as arguments for function.
let runWithInputs irCode filepath =
  let lines = System.IO.File.ReadAllLines filepath
  Array.iter (fun (s: string) ->
    let args = Array.map (fun s -> int s) (s.Trim().Split [|' '|])
    Executor.run irCode (Array.toList args)
  ) lines

[<EntryPoint>]
let main argv =
  if not (checkArgValid argv) then usage ()
  let lexbuf = lex argv[1]
  let prog = parse lexbuf
  match argv[0] with
  | "print-ast" -> printfn "%s" (Program.toStr prog)
  | "print-ir" -> printfn "%s" (IRCode.toStr (Translate.run prog))
  | "run-ir" -> runWithInputs (Translate.run prog) argv[2]
  | _ -> failwith "Unreachable"
  0
