import Lean.Util.SCC
import Std.Data.HashMap

def solve (rules : Std.HashMap String (List (Int × String))) : Int := do
let sccs := Lean.SCC.scc (rules.toList.map Prod.fst) (λ bag => (rules.find! bag).map Prod.snd)
-- which bags can reach "shiny gold" after ≥ 0 steps?
let mut valid : Std.HashMap String Bool := Std.HashMap.empty
for scc in sccs do
  match scc with
  | [color] =>
    valid := valid.insert color $
      color == "shiny gold" ||
      List.any (rules.find! color) (λ rule => valid.find! rule.snd)
  | _ => panic! "cycle"
pure $ ((valid.toList.filter Prod.snd).length : Int) - 1 -- don't count "shiny gold" itself

def parseRule (str : String) : String × List (Int × String) :=
match str.splitOn " bags contain " with
| [color, contentsStr] =>
  if contentsStr == "no other bags."
  then (color, [])
  else Prod.mk color $ (contentsStr.splitOn ", ").map $ λ b =>
    match b.splitOn " " with
    | count :: desc => Prod.mk count.toInt! (" ".intercalate desc.init)
    | [] => panic! "bad rule"
| _ => panic! "bad rule"

def main : IO Unit := do
  let input ← IO.FS.lines "a.in"
  let rules : Std.HashMap String (List (Int × String)) :=
    input.foldl (λ h r => match parseRule r with | (color, contents) => h.insert color contents) Std.HashMap.empty
  IO.print s!"{solve rules}\n"
