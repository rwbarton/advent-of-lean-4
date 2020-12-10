def main : IO Unit := do
  let input ← IO.FS.lines "a.in"
  let mut vals := (input.map String.toInt!).qsort (· < ·)
  vals := vals.push (vals.get! (vals.size - 1) + 3)
  let mut diffs : Array Int := Array.mkArray 4 0
  let mut prev := 0
  for v in vals do
    let d : Nat := (v - prev).toNat
    diffs := diffs.set! d (diffs.get! d + 1)
    prev := v
  IO.print s!"{diffs.get! 1 * diffs.get! 3}\n"

