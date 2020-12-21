import Std.Data.HashSet
import Std.Data.HashMap

open Std

def parseLine (str : String) : List String × List String :=
match (str.dropRight 1).splitOn " (contains " with
| [l, r] => (l.splitOn " ", r.splitOn ", ")
| _ => panic! "invalid parse"

structure Graph where
  ingredients : HashSet String
  options : HashMap String (HashSet String)

def buildGraph (input : Array (List String × List String)) : Graph := do
let mut ingredients := HashSet.empty
let mut options := HashMap.empty
for ⟨ing, allerg⟩ in input do
  for i in ing do
    ingredients := ingredients.insert i
  let ings := ing.foldl HashSet.insert HashSet.empty
  for a in allerg do
    match options.find? a with
    | (some old) =>
        options := options.insert a $
        (ing.filter old.contains).foldl HashSet.insert HashSet.empty
    | none => options := options.insert a ings
⟨ingredients, options⟩

def forcedAssignments (g₀ : Graph) : HashMap String String := do
let mut g := g₀
let mut out := HashMap.empty
for _ in [0:g.options.size] do
  for ⟨a, opts⟩ in g.options.toList do
    if opts.size = 1 then do
      let o := opts.toArray[0]
      out := out.insert a o
      let mut newOptions := g.options.erase a
      for ⟨a', opts'⟩ in newOptions.toList do
        newOptions := newOptions.insert a' (opts'.erase o)
      g := { g with options := newOptions }
if g.options.isEmpty
then out
else panic! "couldn't assign all allergens"

def main : IO Unit := do
let inputStr ← IO.FS.lines "a.in"
let input := inputStr.map parseLine
let a := forcedAssignments (buildGraph input)
let pairs := a.toArray.qsort (· < ·)
let result := ",".intercalate (pairs.toList.map Prod.snd)
IO.print s!"{result}\n"
