def solve1 (s : String) : Int :=
("abcdefghijklmnopqrstuvwxyz".toList.filter (λ c => s.toList.elem c)).length

def main : IO Unit := do
  let input ← IO.FS.readFile "a.in"
  let paras := input.splitOn "\n\n"
  IO.print s!"{(paras.map solve1).foldl Int.add 0}\n"
