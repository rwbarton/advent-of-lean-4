def List.head [Inhabited α] : List α → α
| [] => panic! "empty"
| (x :: xs) => x

def List.findAux [BEq α] (n : Int) (y : α) : List α -> Int
| [] => 0
| (x :: xs) => if x == y then n else xs.findAux (n+1) y

def solve (inlst : List Int) : Int := do
  let mut lst := inlst.reverse
  for _ in [inlst.length:2020] do
    lst := ((lst.drop 1).findAux 1 lst.head :: lst)
  lst.head

def main : IO Unit := do
let lines ← IO.FS.lines "a.in"
let vals := ((lines.get! 0).splitOn ",").map String.toInt!
IO.print s!"{solve vals}"
