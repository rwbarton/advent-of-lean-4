import Std.Data.HashSet

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

def main : IO Unit := do
let input ← IO.FS.lines "a.in"
let mut black := HashSet.empty
for h in input.map (parseLine ∘ String.toList) do
  if black.contains h
  then black := black.erase h
  else black := black.insert h
IO.print s!"{black.size}\n"
