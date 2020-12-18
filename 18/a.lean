import Lean.Data.Json.Parser

open Lean Lean.Quickparse Lean.Json.Parser

inductive Node
| num : Int → Node
| add : Node → Node → Node
| mul : Node → Node → Node

instance : Inhabited Node := ⟨Node.num 0⟩

def Node.toString
| (Node.num n) => s!"{n}"
| (Node.add e₁ e₂) => "(" ++ e₁.toString ++ " + " ++ e₂.toString ++ ")"
| (Node.mul e₁ e₂) => "(" ++ e₁.toString ++ " * " ++ e₂.toString ++ ")"

instance : ToString Node := ⟨Node.toString⟩

partial def parseParens : Quickparse Node := do
  let c ← peek!
  if c.isDigit then Node.num <*> natMaybeZero
  else if c == '(' then do
    skip
    let rec go (n : Node) : Quickparse Node := do
      match (← peek!) with
      | '+' => do skip; go (Node.add n (← parseParens))
      | '*' => do skip; go (Node.mul n (← parseParens))
      | ')' => do skip; pure n
      | _ => fail "bad parse"
    go (← parseParens)
  else fail "bad parse"

def mungeString (s : String) : String :=
"(" ++ String.mk (s.toList.filter (· ≠ ' ')) ++ ")"

def parseLine (s : String) : Node :=
match (parseParens <* eoi) (mungeString s).mkIterator with
| Result.success _ n => n
| Result.error _ err => panic! err

def Node.eval
| (Node.num n) => n
| (Node.add e₁ e₂) => e₁.eval + e₂.eval
| (Node.mul e₁ e₂) => e₁.eval * e₂.eval

def main : IO Unit := do
let input ← IO.FS.lines "a.in"
let result := (input.map (λ l => (parseLine l).eval)).foldl (· + ·) 0
IO.print s!"{result}\n"
