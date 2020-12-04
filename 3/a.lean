def solve (xs : List String) : Int :=
let f n x := x.get (Nat.mod (3*n) x.length) == '#';
((List.zipWith f (List.range xs.length) xs).filter id).length

def main : IO Unit :=
do let input ← IO.FS.lines "a.in"
   IO.print s!"{solve input.toList}\n"
