def parseInput (input : String) : List Nat × List Nat:=
match input.trim.splitOn "\n\n" with
| [p1, p2] => (((p1.splitOn "\n").drop 1).map String.toNat!, ((p2.splitOn "\n").drop 1).map String.toNat!)
| _ => panic! "invalid parse"

def score (deck : List Nat) : Nat :=
let N := deck.length
deck.enum.foldl (λ sum ⟨i, val⟩ => sum + (N-i) * val) 0

partial def play : List Nat → List Nat → Nat
| [], xs => score xs
| xs, [] => score xs
| (x :: xs), (y :: ys) =>
  if x < y
  then play xs (ys ++ [y, x])
  else play (xs ++ [x, y]) ys

def main : IO Unit := do
let input ← IO.FS.readFile "1.in"
let decks := parseInput input
IO.print s!"{play decks.1 decks.2}\n"
