def solve₂ (x₀ x₁ : Int) : List Int → Option Int
| [] => Option.none
| (x :: xs) =>
  if x₀ + x₁ + x = 2020
  then Option.some (x₀ * x₁ * x)
  else solve₂ x₀ x₁ xs

def solve₁ (x₀ : Int) : List Int → Option Int
| [] => Option.none
| (x :: xs) => solve₂ x₀ x xs <|> solve₁ x₀ xs

def solve : List Int → Option Int
| [] => Option.none
| (x :: xs) => solve₁ x xs <|> solve xs

def main (args : List String) : IO UInt32 :=
do let input ← IO.FS.lines "a.in"
   IO.print $ (solve (input.toList.map String.toInt!)).getD 0
   IO.print "\n"
   return 0
