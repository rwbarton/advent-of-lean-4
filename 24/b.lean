import Std.Data.HashSet
import Std.Data.HashMap

open Std

instance : Hashable Int where
  hash n := hash n.toNat

@[reducible] def Hex := Int × Int
def Hex.x : Hex → Int := Prod.fst
def Hex.y : Hex → Int := Prod.snd

instance : Add Hex where
  add h₁ h₂ := ⟨h₁.x + h₂.x, h₁.y + h₂.y⟩

def parseLine : List Char → Hex
| [] => ⟨0, 0⟩
| 'e' :: rest => ⟨2, 0⟩ + parseLine rest
| 'n' :: 'e' :: rest => ⟨1, 1⟩ + parseLine rest
| 's' :: 'e' :: rest => ⟨1, -1⟩ + parseLine rest
| 'w' :: rest => ⟨-2, 0⟩ + parseLine rest
| 'n' :: 'w' :: rest => ⟨-1, 1⟩ + parseLine rest
| 's' :: 'w' :: rest => ⟨-1, -1⟩ + parseLine rest
| _ => panic! "invalid parse"

def adj : List Hex :=
[⟨2, 0⟩, ⟨1, 1⟩, ⟨1, -1⟩, ⟨-2, 0⟩, ⟨-1, 1⟩, ⟨-1, -1⟩]

def evolve (st : HashSet Hex) : HashSet Hex := do
let mut ns : HashMap Hex Nat := HashMap.empty
for h in st.toArray do
  for a in adj do
    ns := ns.insert (h+a) (ns.findD (h+a) 0 + 1)
let mut st' : HashSet Hex := HashSet.empty
for ⟨h, c⟩ in ns.toArray do
  let cond := if st.contains h then c = 1 ∨ c = 2 else c = 2
  if cond then st' := st'.insert h
st'

def main : IO Unit := do
let input ← IO.FS.lines "a.in"
let mut black := HashSet.empty
for h in input.map (parseLine ∘ String.toList) do
  if black.contains h
  then black := black.erase h
  else black := black.insert h
let final := Nat.repeat evolve 100 black
IO.print s!"{final.size}\n"

