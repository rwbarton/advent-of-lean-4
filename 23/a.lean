partial def resolve (bad : List Nat) (x : Nat) : Nat :=
let x' := if x = 0 then 9 else x
if bad.any (· = x') then resolve bad (x'-1) else x'

def step : List Nat → List Nat
| a :: b₁ :: b₂ :: b₃ :: rest =>
  let tgt := resolve [b₁, b₂, b₃] (a-1)
  let ⟨pre, post⟩ := rest.span (· ≠ tgt)
  pre ++ tgt :: b₁ :: b₂ :: b₃ :: post.drop 1 ++ [a]
| _ => panic! "too short"

def main : IO Unit := do
let input ← IO.FS.lines "a.in"
let init := input[0].toList.map (String.toNat! ∘ String.singleton)
let after := Nat.repeat step 100 init
let ⟨pre, post⟩ := after.span (· ≠ 1)
IO.print s!"{String.join ((post.drop 1 ++ pre).map toString)}\n"
