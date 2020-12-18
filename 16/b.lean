def List.sum (xs : List Int) : Int := xs.foldl (· + ·) 0

@[reducible] def Ticket : Type := List Int

def somewhatValid (flds : List (String × List (Int × Int))) (t : Ticket) : Bool :=
t.all (λ n => flds.any (λ ⟨_, ps⟩ => ps.any (λ ⟨a, b⟩ => a ≤ n ∧ n ≤ b)))

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
   ((nearby.splitOn "\n").drop 1).init.map parseTicket)
| _ => panic! "invalid input"

def matchable (fld : String × List (Int × Int)) (ts : Array Ticket) (i : Nat) : Bool :=
ts.all (λ t => match t.get? i with
  | some n => fld.2.any (λ ⟨a, b⟩ => a ≤ n ∧ n ≤ b)
  | _ => panic! "impossible")

def main : IO Unit := do
let input := parse (← IO.FS.readFile "a.in")
let goodTickets := (input.2.2.filter (somewhatValid input.1)).toArray
let mut matches := (List.range (goodTickets.get! 0).length).toArray.map (λ i => input.1.toArray.map (λ fld => matchable fld goodTickets i))
let mut assign := Array.mkArray input.1.length 0
let N := input.1.length
for _ in [0:N] do
  for r in [0:N] do
    let mut count := 0
    let mut found := 0
    for c in [0:N] do
      if (matches.get! r).get! c then
        count := count + 1
        found := c
    if count == 1 then
      assign := assign.set! found r
      for r' in [0:N] do
        matches := matches.modify r' (λ m => m.set! found false)
      break
let mut prod := 1
for ⟨i, fld⟩ in input.1.enum do
  if "departure ".isPrefixOf fld.1 then
    prod := prod * input.2.1.get! (assign.get! i)
IO.print s!"{prod}\n"
