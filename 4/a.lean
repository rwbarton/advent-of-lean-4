def isValid (para : String) : Bool :=
let fieldStrs : List String := para.split (λ c => c == ' ' || c == '\n');
let fields := List.map (λ s => s.takeWhile (λ c => c ≠ ':')) fieldStrs;
List.all ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"] (λ f => fields.elem f)

def solve (input : String) : Int :=
let paras := input.splitOn "\n\n"
(paras.filter isValid).length

def main : IO Unit := do
  let input ← IO.FS.readFile "a.in"
  IO.print s!"{solve input}\n"
