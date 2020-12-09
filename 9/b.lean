def bufSize : Nat := 25

def isPairSum (xs : List Nat) (y : Nat) : Bool := do
for x₁ in xs do
  for x₂ in xs do
    if x₁ ≠ x₂ && x₁ + x₂ == y then return true
return false

def firstBad (xs : List Nat) : Nat := do
let mut old := xs.take bufSize
for x in xs.drop bufSize do
  if ! isPairSum old x then return x
  old := old.drop 1 ++ [x]
panic! "no solution"

def psums_aux : List Nat → Nat → List Nat
| [], s => [s]
| (x :: xs), s => s :: psums_aux xs (x + s)

def asRange (xs : List Nat) (tgt : Nat) : Nat × Nat := do
let psums := (psums_aux xs 0).toArray
for i in [0:psums.size] do
  for j in [0:psums.size] do
    if psums.get! j - psums.get! i == tgt
    then let xs' : List Nat := (xs.take j).drop i
         return (xs'.foldl Nat.min tgt, xs'.foldl Nat.max 0)
panic! "no solution"

def main : IO Unit := do
  let input ← IO.FS.lines "a.in"
  let xs := input.toList.map String.toNat!
  let target := firstBad xs
  let ⟨mn, mx⟩ := asRange xs target
  IO.print s!"{mn + mx}\n"
