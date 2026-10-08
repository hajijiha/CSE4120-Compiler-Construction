namespace State

open IR

exception RuntimeError of string

// Address of memory space.
type MemAddr = int

// Unlike real memory, let's assume that code uses separate space.
type CodeAddr = int

// Mapping from register name to value (int).
type RegMap = Map<Register,int>

module RegMap =
  let empty: RegMap = Map.empty

  let lookup (reg: Register) (regMap: RegMap): int =
    if Map.containsKey reg regMap then Map.find reg regMap
    else raise (RuntimeError (sprintf "Accessing undefined register %s" reg))

  let bind (reg: Register) (v: int) (regMap: RegMap) =
    Map.add reg v regMap

// Mapping from memory address to byte, along with upper limit of address.
type Memory = int * Map<MemAddr,byte>

module Memory =
  // Let's start the memory space from non-zero address (to distinguish NULL).
  let startAddr = 0x1000

  let empty: Memory = (startAddr, Map.empty)

  let allocate size (mem: Memory): Memory * MemAddr =
    let memBrk, map = mem
    let newMem = (memBrk + size, map)
    (newMem, memBrk)

  let readByte (addr: MemAddr) (mem: Memory): byte =
    let memBrk, map = mem
    if addr < startAddr || addr >= memBrk then
      raise (RuntimeError (sprintf "Read from invalid address %d" addr))
    elif not (Map.containsKey addr map) then 255uy // Garbage value
    else Map.find addr map

  let loadByte (addr: MemAddr) (mem: Memory): int =
    int (sbyte (readByte addr mem))

  let loadWord (addr: MemAddr) (mem: Memory): int =
    let u1 = uint (readByte addr mem)
    let u2 = uint (readByte (addr + 1) mem)
    let u3 = uint (readByte (addr + 2) mem)
    let u4 = uint (readByte (addr + 3) mem)
    int (u1 + (u2 <<< 8) + (u3 <<< 16) + (u4 <<< 24))

  let updateByte (addr: MemAddr) (b: byte) (mem: Memory) : Memory =
    let memBrk, map = mem
    if addr < startAddr || addr >= memBrk then
      raise (RuntimeError (sprintf "Write to invalid address %d" addr))
    else (memBrk, Map.add addr b map)

  let storeByte (addr: MemAddr) (v: int) (mem: Memory) : Memory =
    updateByte addr (byte v) mem

  let storeWord (addr: MemAddr) (v: int) (mem: Memory) : Memory =
    updateByte addr (byte v) mem
    |> updateByte (addr + 1) (byte (v >>> 8))
    |> updateByte (addr + 2) (byte (v >>> 16))
    |> updateByte (addr + 3) (byte (v >>> 24))

// Mapping from code address to IR instruction.
type IRMap = Map<CodeAddr,Instr>

module IRMap =
  let empty: IRMap = Map.empty

  let tryFind (addr: CodeAddr) (irMap: IRMap): Instr option =
    Map.tryFind addr irMap

  let add (addr: CodeAddr) (instr: Instr) (irMap: IRMap) : IRMap =
    Map.add addr instr irMap

// Mapping from label to code address.
type LabelMap = Map<Label,CodeAddr>

module LabelMap =
  let empty: LabelMap = Map.empty

  let find (label: Label) (labelMap: LabelMap): CodeAddr =
    if Map.containsKey label labelMap then Map.find label labelMap
    else raise (RuntimeError (sprintf "Undefined label %s referred" label))

  let add (label: Label) (v: CodeAddr) (labelMap: LabelMap) =
    Map.add label v labelMap

// Execution state consists of register map, memory, and program counter.
type State = RegMap * Memory * CodeAddr

type StepResult =
  | Running of State // Successfully executed one instruction
  | Finished of int // Program (function) successfully returned an integer
