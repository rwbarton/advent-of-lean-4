structure Passwd where
  min : Int
  max : Int
  letter : Char
  str : String

instance : Inhabited Passwd := ⟨⟨0,0,'a',"a"⟩⟩

instance : ToString Passwd :=
⟨λ p => s!"{p.min}-{p.max} {p.letter}: {p.str}"⟩

def parseLine (s : String) : Passwd :=
match s.splitOn with
| [a,b,c] => match a.splitOn "-" with
  | [x,y] => ⟨x.toInt!, y.toInt!, b.front, c⟩
  | _ => panic! "bad format"
| _ => panic! "bad format"

def isValid (p : Passwd) : Bool :=
(p.str.get (p.min.toNat - 1) == p.letter) != (p.str.get (p.max.toNat - 1) == p.letter)

def solve (xs : List String) : Int :=
(xs.filter (λ x => isValid (parseLine x))).length

def main : IO Unit :=
do let input ← IO.FS.lines "a.in"
   IO.print s!"{solve input.toList}\n"
