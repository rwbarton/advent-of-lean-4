def value (s : String) : Nat :=
let cval (c : Char) : Nat := if c == 'B' || c == 'R' then 1 else 0
let rec v : List Char → Nat
        | [] => 0
        | (c :: cs) => 2 * v cs + cval c
v s.toList.reverse

def solve (input : List String) : Nat :=
(input.map value).foldl Nat.max 0

def main : IO Unit := do
  let input ← IO.FS.lines "a.in"
  IO.print s!"{solve input.toList}\n"
