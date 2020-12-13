def List.foldl1 [Inhabited α] (f : α → α → α) : List α → α
| x :: xs => xs.foldl f x
| _ => panic! "empty"

def solve (start : Int) (buses : List Int) : Int :=
let durs := buses.map (λ d => (d, (d - start % d) % d))
let best := durs.foldl1 (λ ⟨d₁, t₁⟩ ⟨d₂, t₂⟩ => if t₁ < t₂ then ⟨d₁, t₁⟩ else ⟨d₂, t₂⟩)
best.1 * best.2

def parseInput : List String → Int × List (Option Int)
| [l1, l2] => (l1.toInt!, (l2.splitOn ",").map String.toInt?)
| _ => panic! "invalid input"

def main : IO Unit := do
let i ← IO.FS.lines "a.in"
let input := parseInput i.toList
IO.print s!"{solve input.1 (input.2.filterMap id)}\n"
