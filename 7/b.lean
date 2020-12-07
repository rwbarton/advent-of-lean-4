import Lean.Util.SCC
import Std.Data.HashMap

def solve (rules : Std.HashMap String (List (Int × String))) : Int := do
let sccs := Lean.SCC.scc (rules.toList.map Prod.fst) (λ bag => (rules.find! bag).map Prod.snd)
-- how many bags must a bag hold, including itself?
let mut size : Std.HashMap String Int := Std.HashMap.empty
for scc in sccs do
  match scc with
  | [color] =>
    size := size.insert color $
      1 + (rules.find! color).foldl (λ sum ⟨n, color'⟩ => sum + n * size.find! color') 0
  | _ => panic! "cycle"
pure $ size.find! "shiny gold" - 1

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
