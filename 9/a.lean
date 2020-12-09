def bufSize : Nat := 25

def isPairSum (xs : List Int) (y : Int) : Bool := do
for x₁ in xs do
  for x₂ in xs do
    if x₁ ≠ x₂ && x₁ + x₂ == y then return true
return false

def firstBad (xs : List Int) : Int := do
let mut old := xs.take bufSize
for x in xs.drop bufSize do
  if ! isPairSum old x then return x
  old := old.drop 1 ++ [x]
panic! "no solution"

def main : IO Unit := do
  let input ← IO.FS.lines "a.in"
  IO.print s!"{firstBad (input.toList.map String.toInt!)}\n"
