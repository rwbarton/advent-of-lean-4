inductive Instr : Type
| NOP : Instr
| ACC : Int → Instr
| JMP : Int → Instr

open Instr

instance : Inhabited Instr := ⟨NOP⟩

def execute (prog : Array Instr) : Int := do
let mut seen : Array Bool := Array.mkArray prog.size False
let mut ip : Nat := 0
let mut acc : Int := 0
-- program cannot execute more than prog.size+1 steps
for _ in [0:prog.size+1] do
  if seen.get! ip then return acc
  seen := seen.set! ip True
  match prog.get! ip with
  | NOP => ip := ip + 1
  | ACC z => do
    acc := acc + z
    ip := ip + 1
  | JMP z => ip := (ip + z : Int).toNat
pure $ panic! "unreachable"

def readSignedInt (str : String) : Int :=
match str.toList with
| '+' :: rest => rest.asString.toInt!
| '-' :: rest => - rest.asString.toInt!
| _ => panic! "bad signed int"

def parseProgram : Array String → Array Instr :=
Array.map $ λ str =>
match str.splitOn " " with
| ["nop", _] => NOP
| ["acc", n] => ACC (readSignedInt n)
| ["jmp", n] => JMP (readSignedInt n)
| _ => panic! "bad instruction"

def main : IO Unit := do
  let input ← IO.FS.lines "a.in"
  let prog := parseProgram input
  IO.print s!"{execute prog}\n"
