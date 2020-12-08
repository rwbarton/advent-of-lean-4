inductive Instr : Type
| NOP : Int → Instr
| ACC : Int → Instr
| JMP : Int → Instr

open Instr

instance : Inhabited Instr := ⟨NOP 0⟩

def execute (prog : Array Instr) : Option Int := do
let mut seen : Array Bool := Array.mkArray prog.size false
let mut ip : Nat := 0
let mut acc : Int := 0
-- program cannot execute more than prog.size+1 steps
for _ in [0:prog.size+1] do
  if ip == prog.size then return acc
  if seen.get! ip then Option.none
  seen := seen.set! ip True
  match prog.get! ip with
  | NOP _ => ip := ip + 1
  | ACC z => do
    acc := acc + z
    ip := ip + 1
  | JMP z => ip := (ip + z : Int).toNat
panic! "unreachable"

def tweakAt (prog : Array Instr) (i : Nat) : Option Int := do
let instr' ← match prog.get! i with
  | NOP n => pure (JMP n)
  | JMP n => pure (NOP n)
  | ACC n => Option.none
execute (prog.set! i instr')

def tweak (prog : Array Instr) : Int := do
for i in [0:prog.size] do
  match tweakAt prog i with
  | Option.some a => return a
  | _ => ()
panic! "no solution found"

def readSignedInt (str : String) : Int :=
match str.toList with
| '+' :: rest => rest.asString.toInt!
| '-' :: rest => - rest.asString.toInt!
| _ => panic! "bad signed int"

def parseProgram : Array String → Array Instr :=
Array.map $ λ str =>
match str.splitOn " " with
| ["nop", n] => NOP (readSignedInt n)
| ["acc", n] => ACC (readSignedInt n)
| ["jmp", n] => JMP (readSignedInt n)
| _ => panic! "bad instruction"

def main : IO Unit := do
  let input ← IO.FS.lines "a.in"
  let prog := parseProgram input
  IO.print s!"{tweak prog}\n"
