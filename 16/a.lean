def List.sum (xs : List Int) : Int := xs.foldl (· + ·) 0

@[reducible] def Ticket : Type := List Int

def somewhatValid (flds : List (String × List (Int × Int))) (t : Ticket) : Int :=
(t.map (λ n => if flds.any (λ ⟨_, ps⟩ => ps.any (λ ⟨a, b⟩ => a ≤ n ∧ n ≤ b)) then 0 else n)).sum

def parseRange (str : String) : Int × Int :=
match str.splitOn "-" with
| [a, b] => (a.toInt!, b.toInt!)
| _ => panic! "invalid range"

def parseType (str : String) : String × List (Int × Int) :=
match str.splitOn ": " with
| [fld, vals] => (fld, (vals.splitOn " or ").map parseRange)
| _ => panic! "invalid type"

def parseTicket (str : String) : Ticket :=
(str.splitOn ",").map String.toInt!

def parse (str : String) : List (String × List (Int × Int)) × Ticket × List Ticket :=
match str.splitOn "\n\n" with
| [ty, your, nearby] =>
  ((ty.splitOn "\n").map parseType,
   parseTicket ((your.splitOn "\n").getD 1 ""),
   ((nearby.splitOn "\n").drop 1).map parseTicket)
| _ => panic! "invalid input"

def main : IO Unit := do
let input := parse (← IO.FS.readFile "a.in")
IO.print s!"{List.sum (input.2.2.map (somewhatValid input.1))}\n"
