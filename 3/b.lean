def solve (num denom : Nat) (xs : List String) : Int :=
let f n x :=
  Nat.mod n denom == 0 &&
  x.get (Nat.mod (num * n.div denom) x.length) == '#';
((List.zipWith f (List.range xs.length) xs).filter id).length

def main : IO Unit :=
do let input ← IO.FS.lines "a.in"
   let xs := input.toList
   let result := solve 1 1 xs * solve 3 1 xs * solve 5 1 xs  * solve 7 1 xs * solve 1 2 xs
   IO.print s!"{result}\n"
