import Std.Data.HashMap

inductive Command
| setMask (mask : String) : Command
| setMem (addr : Nat) (val : Nat) : Command

open Command

instance : Inhabited Command := ⟨setMask ""⟩

structure St : Type where
  mask : String
  mem : Std.HashMap Nat Nat

def initSt : St :=
{ mask := "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX", mem := Std.HashMap.empty }

def clearBit (i : Nat) (val : Nat) : Nat :=
(val / (2^(i+1))) * 2^(i+1) + (val % (2^i))

def setBit (i : Nat) (val : Nat) : Nat :=
clearBit i val + 2^i

def maskedVal (mask : String) (val : Nat) : Nat := do
let mut res := val
for i in [0:36] do
  match mask.get i with
  | 'X' => ()
  | '1' => res := setBit (35-i) res
  | '0' => res := clearBit (35-i) res
  | _ => panic! "bad mask"
res

def exec (st : St) : Command → St
| setMask mask => { st with mask := mask }
| setMem addr val => { st with mem := st.mem.insert addr (maskedVal st.mask val) }

def parseCmd (str : String) : Command :=
match str.splitOn " " with
| ["mask", "=", rhs] => setMask rhs
| [lhs, "=", rhs] => setMem ((lhs.split (λ c => c == '[' || c == ']')).get! 1).toNat! rhs.toNat!
| _ => panic! "invalid command"

def main : IO Unit := do
let lines ← IO.FS.lines "a.in"
let st := (lines.map parseCmd).foldl exec initSt
let sum := st.mem.fold (λ s k v => s + v) 0
IO.print s!"{sum}\n"
