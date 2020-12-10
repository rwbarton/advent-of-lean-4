def solve (vals : Array Int) : Int := do
let mut counts : Array Int := Array.mkArray vals.size 0
for i in [0:vals.size] do
  if vals.get! i ≤ 3 then counts := counts.set! i (counts.get! i + 1)
  for j in [0:i] do
    if vals.get! i - vals.get! j ≤ 3 then counts := counts.set! i (counts.get! i + counts.get! j)
counts.get! (vals.size - 1)

def main : IO Unit := do
  let input ← IO.FS.lines "a.in"
  let mut vals := (input.map String.toInt!).qsort (· < ·)
  vals := vals.push (vals.get! (vals.size - 1) + 3)
  IO.print s!"{solve vals}\n"
