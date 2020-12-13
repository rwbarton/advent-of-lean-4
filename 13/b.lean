partial def egcd (x y : Int) : Int × Int :=
if x == 1 then (1, 0) else
let ⟨q, r⟩ := (y / x, y % x)
let ⟨a, b⟩ := egcd r x
-- y = q * x + r
-- a * r + b * x = 1
-- (b - a * q) * x + a * y = 1
(b - a * q, a)

def crt (a₁ n₁ a₂ n₂ : Int) : Int :=
let ⟨b₁, b₂⟩ := egcd n₁ n₂
-- b₁ * n₁ + b₂ * n₂ = 1
-- b₁ * n₁ ≡ 0 (mod n₁), 1 (mod n₂)
(a₁ * b₂ * n₂ + a₂ * b₁ * n₁) % (n₁ * n₂)

def solve (input : List (Int × Int)) : Int :=
let res := input.foldl (λ ⟨a, n⟩ ⟨a', n'⟩ => (crt a n a' n', n * n')) (0, 1)
(res.1 + res.2) % res.2

def parseInput : List String → List (Int × Int)
| [l1, l2] => (l2.splitOn ",").enum.filterMap $ λ ⟨i, str⟩ =>
  match str.toInt? with
  | Option.some n => some (-i, n)
  | Option.none => none
| _ => panic! "invalid input"

def main : IO Unit := do
let i ← IO.FS.lines "a.in"
let input := parseInput i.toList
IO.print s!"{solve input}\n"
