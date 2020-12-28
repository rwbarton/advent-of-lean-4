@[reducible, inline] def modulus := 20201227

def main : IO Unit := do
let input ← IO.FS.lines "a.in"
let pub₁ := input[0].toNat!
let pub₂ := input[1].toNat!
let mut dlog : Array Nat := Array.mkArray modulus 0
let mut pow := 1
for j in [0:modulus-1] do
  dlog := dlog.set! pow j
  pow := (7 * pow) % modulus
let priv₁ := dlog[pub₁]
let mut key := 1
for j in [0:priv₁] do
  key := (pub₂ * key) % modulus
IO.print s!"{key}\n"
