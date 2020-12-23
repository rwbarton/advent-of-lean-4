structure St where
  cur : Nat
  next prev : Array Nat

def MAX : Nat := 1000000

partial def resolve (bad : List Nat) (x : Nat) : Nat :=
let x' := if x = 0 then MAX else x
if bad.any (· = x') then resolve bad (x'-1) else x'

def step (s : St) : St := do
let a := s.cur
let b₁ := s.next[a]
let b₂ := s.next[b₁]
let b₃ := s.next[b₂]
let b₄ := s.next[b₃]
let tgt := resolve [b₁, b₂, b₃] (a-1)
let tgt' := s.next[tgt]
let mut next' := s.next
let mut prev' := s.prev
next' := ((next'.set! a b₄).set! b₃ tgt').set! tgt b₁
prev' := ((prev'.set! b₄ a).set! tgt' b₃).set! b₁ tgt
{ cur := b₄, next := next', prev := prev' }

def main : IO Unit := do
let input ← IO.FS.lines "a.in"
let init₀ := (Array.mk input[0].toList).map (String.toNat! ∘ String.singleton)
let mut init := Array.mkArray MAX 0
for i in [0:MAX] do
  init := init.set! i (i+1)
for i in [0:init₀.size] do
  init := init.set! i init₀[i]
let N := init.size
let mut next := Array.mkArray (N + 1) 0
let mut prev := Array.mkArray (N + 1) 0
for i in [0:init.size] do
  next := next.set! init[i] init[(i+1)%N]
  prev := prev.set! init[(i+1)%N] init[i]
let st := ⟨init[0], next, prev⟩
let after := Nat.repeat step 10000000 st
let a₁ := after.next[1]
let a₂ := after.next[a₁]
IO.print s!"{a₁ * a₂}\n"
