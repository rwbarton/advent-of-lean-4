def solve1 (para : String) : Int :=
let wds := (para.splitOn "\n").filter (λ s => s.length ≠ 0) -- remove empty string from \n at end of file
("abcdefghijklmnopqrstuvwxyz".toList.filter (λ c => wds.all (λ s => s.toList.elem c))).length

def main : IO Unit := do
  let input ← IO.FS.readFile "a.in"
  let paras := input.splitOn "\n\n"
  IO.print s!"{(paras.map solve1).foldl Int.add 0}\n"
