import Std.Data.HashMap

open Std

@[reducible] def Bits := UInt32

structure OrientedPiece where
  -- bits read from left to right or top to bottom in big-endian order
  left right top bottom : Bits

instance : ToString OrientedPiece where
  toString p := s!"{p.left} - {p.right}  {p.top} | {p.bottom}"

def rev (N : Nat) (x : Bits) : Bits := do
let mut res : Bits := 0
for i in [0:N] do
  res := res + if (x.toNat / 2^i) % 2 == 1 then UInt32.ofNat (2^(N-1-i)) else 0
res

def parsePiece (str : List String) : OrientedPiece := do
let N := str.length
let val (row col : Nat) : Bits :=
  if (str.get! row).get col == '#' then 1 else 0
let mut res : OrientedPiece := ⟨0, 0, 0, 0⟩
for i in [0:N] do
  res := { left := 2 * res.left + val i 0,
           right := 2 * res.right + val i (N-1),
           top := 2 * res.top + val 0 i,
           bottom := 2 * res.bottom + val (N-1) i }
res

instance : Monad List :=
{ pure := λ a => [a],
  map := List.map,
  bind := λ x f => (x.map f).join }

def orientPiece (N : Nat) (p : OrientedPiece) : Array OrientedPiece :=
Array.mk $ do
  let rot q := { top := q.right, right := rev N q.bottom, bottom := q.left, left := rev N q.top }
  let p₁ := rot p
  let p₂ := rot p₁
  let p₃ := rot p₂
  let flip q := { top := q.left, left := q.top, bottom := q.right, right := q.bottom }
  let p' ← [p, p₁, p₂, p₃]
  [p', flip p']

@[reducible] def Tiles := List (Nat × Array OrientedPiece)

def buildInput (str : String) : Tiles :=
(str.splitOn "\n\n").map $ λ para =>
match para.trim.splitOn "\n" with
| header :: rest =>
  (((header.dropRight 1).drop 5).toNat!, orientPiece rest.length (parsePiece rest))
| [] => panic! "bad para"

partial def go (S : Nat) (tiles : Tiles) (used : HashMap Nat (Nat × Nat))
  (above : Array Bits) (left : Bits) (row col : Nat) : List Nat :=
if row == S
then pure $ do
  let mut prod := 1
  for ⟨i, ⟨row, col⟩⟩ in used.toArray do
    if (row == 0 ∨ row == S-1) ∧ (col == 0 ∨ col == S-1) then prod := prod * i
  prod
else do
  let p ← tiles
  if used.contains p.1 then []
  let ⟨nextRow, nextCol⟩ := if col + 1 == S then (row+1, 0) else (row, col+1)
  let o ← p.2.toList
  if col > 0 ∧ left ≠ o.left then []
  if row > 0 ∧ above.get! col ≠ o.top then []
  go S tiles (used.insert p.1 (row, col)) (above.set! col o.bottom) o.right nextRow nextCol

def solve (S : Nat) (tiles : Tiles) :=
go S tiles HashMap.empty (Array.mkArray S 0) 0 0 0

def theUnique [Inhabited α] [BEq α] : List α → α
| [] => panic! "nothing"
| (x::xs) => if xs.all (· == x) then x else panic! "multiple values"

def main : IO Unit := do
let input ← IO.FS.readFile "a.in"
IO.print (theUnique $ solve 12 (buildInput input))
IO.print "\n"
