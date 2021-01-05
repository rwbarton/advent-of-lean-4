import Std.Data.HashMap

open Std

@[reducible] def Bits := UInt32

structure OrientedPiece where
  -- bits read from left to right or top to bottom in big-endian order
  left : Bits
  right : Bits
  top : Bits
  bottom : Bits
  payload : Array (Array Bool)

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
let mut res : OrientedPiece := ⟨0, 0, 0, 0, Array.mkArray (N-2) (Array.mkArray (N-2) false)⟩
for i in [0:N] do
  res := { left := 2 * res.left + val i 0,
           right := 2 * res.right + val i (N-1),
           top := 2 * res.top + val 0 i,
           bottom := 2 * res.bottom + val (N-1) i,
           payload := res.payload }
for i in [1:N-1] do
  for j in [1:N-1] do
    if val i j ≠ 0
    then res := { res with payload := res.payload.modify (i-1) (·.set! (j-1) true) }
res

instance : Monad List :=
{ pure := λ a => [a],
  map := List.map,
  bind := λ x f => (x.map f).join }

def orientPiece (N : Nat) (p : OrientedPiece) : Array OrientedPiece :=
Array.mk $ do
  let rot q :=
    { top := q.right, right := rev N q.bottom, bottom := q.left, left := rev N q.top,
      payload :=
        Array.mk $ (List.range (N-2)).map $ λ row =>
        Array.mk $ (List.range (N-2)).map $ λ col =>
        q.payload[col][N-3-row] }
  let p₁ := rot p
  let p₂ := rot p₁
  let p₃ := rot p₂
  let flip q :=
    { top := q.left, left := q.top, bottom := q.right, right := q.bottom,
      payload :=
        Array.mk $ (List.range (N-2)).map $ λ row =>
        Array.mk $ (List.range (N-2)).map $ λ col =>
        q.payload[col][row] }
  let p' ← [p, p₁, p₂, p₃]
  [p', flip p']

@[reducible] def Tiles := List (Nat × Array OrientedPiece)

def buildInput (str : String) : Tiles :=
(str.splitOn "\n\n").map $ λ para =>
match para.trim.splitOn "\n" with
| header :: rest =>
  (((header.dropRight 1).drop 5).toNat!, orientPiece rest.length (parsePiece rest))
| [] => panic! "bad para"

partial def go (S N : Nat) (tiles : Tiles) (used : HashMap Nat (Nat × Nat × OrientedPiece))
  (above : Array Bits) (left : Bits) (row col : Nat) : List (Array (Array Bool)) :=
if row == S
then pure $ do
  let mut out := Array.mkArray (S*(N-2)) (Array.mkArray (S*(N-2)) false)
  for ⟨i, ⟨row, ⟨col, o⟩⟩⟩ in used.toArray do
    for r in [0:N-2] do
      for c in [0:N-2] do
        out := out.modify ((N-2)*row + r) (·.set! ((N-2)*col + c) o.payload[r][c])
  out
else do
  let p ← tiles
  if used.contains p.1 then []
  let ⟨nextRow, nextCol⟩ := if col + 1 == S then (row+1, 0) else (row, col+1)
  let o ← p.2.toList
  if col > 0 ∧ left ≠ o.left then []
  if row > 0 ∧ above.get! col ≠ o.top then []
  go S N tiles (used.insert p.1 (row, col, o)) (above.set! col o.bottom) o.right nextRow nextCol

def solve (S N : Nat) (tiles : Tiles) :=
go S N tiles HashMap.empty (Array.mkArray S 0) 0 0 0

def printGrid (g : Array (Array Bool)) : IO Unit := do
for r in g do
  for c in r do
    IO.print (if c then '#' else '.')
  IO.print "\n"
IO.print "\n"

def nessie : Array String :=
#["                  # ",
  "#    ##    ##    ###",
  " #  #  #  #  #  #   "]

def npos : Array (Nat × Nat) := do
let nr := nessie.size
let nc := nessie[0].length
let mut res := #[]
for r in [0:nr] do
  for c in [0:nc] do
    if nessie[r][c] == '#' then
      res := res.push (r, c)
res

def removeNessies (g : Array (Array Bool)) : Array (Array Bool) := do
let mut nessieFree := g
let nr := nessie.size
let nc := nessie[0].length
for row in [0:g.size-nr+1] do
  for col in [0:g[0].size-nc+1] do
    let isNessie := npos.all (λ ⟨r, c⟩ => g[row+r][col+c])
    if isNessie then
      for ⟨r, c⟩ in npos do
        nessieFree := nessieFree.modify (row+r) (·.set! (col+c) false)
nessieFree

def countRough (g : Array (Array Bool)) : Nat :=
g.foldl (λ s r => s + r.foldl (λ s' c => s' + if (c : Bool) then 1 else 0) 0) 0

def main : IO Unit := do
let input ← IO.FS.readFile "a.in"
let result := (solve 12 10 (buildInput input)).map (countRough ∘ removeNessies)
let min := result.foldl Nat.min (12*10*12*10)
IO.print s!"{min}\n"
