import Std.Data.HashMap

open Std

inductive Production
| terminal : Char → Production
| nonterminal : List (List Nat) → Production

open Production

instance : Inhabited Production := ⟨terminal '@'⟩

instance : ToString Production where
  toString
  | (terminal c) => s!"\"{c}\""
  | (nonterminal opts) => s!"{opts}"

def parseProd (str : String) : Nat × Production :=
match str.splitOn ": " with
| [idx, rule] => Prod.mk idx.toNat! $ match rule.toList with
  | ['"', c, '"'] => terminal c
  | _ => nonterminal $ (rule.splitOn " | ").map (λ s => (s.splitOn " ").map String.toNat!)
| _ => panic! "invalid production"

def parseInput (input : String) : HashMap Nat Production × List String :=
match input.splitOn "\n\n" with
| [prods, strs] =>
  ((prods.splitOn "\n").foldl (λ m s => let p := parseProd s; m.insert p.1 p.2) HashMap.empty,
   (strs.splitOn "\n").filter (not ∘ String.isEmpty))
| _ => panic! "invalid input"

partial def matchesZero (prods : HashMap Nat Production) (str : String) : Bool :=
let N := str.length
let lookup (i j k : Nat) : StateM (Array (Option Bool)) (Option Bool) := do
  let ar ← get
  pure (ar.get! ((i * (N+1) + j) * (N+1) + k))
let rec go (i j k : Nat) : StateM (Array (Option Bool)) Bool := do
  match (← lookup i j k) with
  | some res => pure res
  | none => do
    let res ← match prods.find! i with
    | (terminal c) => pure $ str.extract j k == String.singleton c
    | (nonterminal opts) =>
      let rec matchOpt
      | [],      j' => str.extract j' k == ""
      | (o::os), j' => do
        for j'' in [j'+1:k+1] do
          if (← go o j' j'') && (← matchOpt os j'') then return true
        return false
      opts.anyM $ λ opt => matchOpt opt j
    modify (·.set! ((i * (N+1) + j) * (N+1) + k) res)
    pure res
(go 0 0 str.length).run' (Array.mkArray (prods.size * (N+1) * (N+1)) Option.none)

def main : IO Unit := do
let input ← IO.FS.readFile "a.in"
let ⟨prods, strs⟩ := parseInput input
IO.print s!"{(strs.filter (matchesZero prods)).length}\n"
