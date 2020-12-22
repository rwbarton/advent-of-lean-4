import Std.Data.HashSet

open Std

def parseInput (input : String) : List Nat × List Nat:=
match input.trim.splitOn "\n\n" with
| [p1, p2] => (((p1.splitOn "\n").drop 1).map String.toNat!, ((p2.splitOn "\n").drop 1).map String.toNat!)
| _ => panic! "invalid parse"

def score (deck : List Nat) : Nat :=
let N := deck.length
deck.enum.foldl (λ sum ⟨i, val⟩ => sum + (N-i) * val) 0

partial def play' (seen : HashSet (List Nat × List Nat)) (d₁ d₂ : List Nat) : Bool :=
if seen.contains (d₁, d₂) then false else
match d₁, d₂ with
| [], xs => true
| xs, [] => false
| (x :: xs), (y :: ys) =>
  let winner2 :=
    if xs.length ≥ x ∧ ys.length ≥ y
    then play' HashSet.empty (xs.take x) (ys.take y)
    else x < y
  if winner2
  then play' (seen.insert (d₁, d₂)) xs (ys ++ [y, x])
  else play' (seen.insert (d₁, d₂)) (xs ++ [x, y]) ys

partial def play : List Nat → List Nat → Nat
| [], xs => score xs
| xs, [] => score xs
| (x :: xs), (y :: ys) =>
  let winner2 :=
    if xs.length ≥ x ∧ ys.length ≥ y
    then play' HashSet.empty (xs.take x) (ys.take y)
    else x < y
  if winner2
  then play xs (ys ++ [y, x])
  else play (xs ++ [x, y]) ys

def main : IO Unit := do
let input ← IO.FS.readFile "a.in"
let decks := parseInput input
IO.print s!"{play decks.1 decks.2}\n"
