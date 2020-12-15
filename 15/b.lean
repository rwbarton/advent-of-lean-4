import Std.Data.HashMap

def solve (inlst : List Nat) : Int := do
  let mut t : Std.HashMap Nat Nat := Std.HashMap.empty
  let mut last : Nat := 0
  let mut nextj : Option Nat := none
  for ⟨i, n⟩ in inlst.enum do
    nextj := t.find? n
    t := t.insert n i
    last := n
  for j in [inlst.length:30000000] do
    last :=
      match nextj with
      | none => 0
      | some i => j-i-1
    nextj := t.find? last
    t := t.insert last j
  last

def main : IO Unit := do
let lines ← IO.FS.lines "a.in"
let vals := ((lines.get! 0).splitOn ",").map String.toNat!
IO.print s!"{solve vals}"
