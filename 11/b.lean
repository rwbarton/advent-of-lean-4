def Array.mkFun {α : Type} (n : Nat) (f : Nat → α) : Array α :=
Array.mk ((List.range n).map f)

inductive Seat
| floor | empty | occupied

open Seat

def Seat.beq : Seat → Seat → Bool
| floor, floor => true
| empty, empty => true
| occupied, occupied => true
| _, _ => false

instance : BEq Seat := ⟨Seat.beq⟩

instance : Inhabited Seat := ⟨floor⟩

def State := Array (Array Seat)

instance : BEq State := show BEq (Array (Array Seat)) from inferInstance

partial def State.get_first_in (s : State) (row col : Int) (dr dc : Int) : Seat :=
let r' := row + dr
let c' := col + dc
if r' < 0 || r' ≥ s.size then floor else
if c' < 0 || c' ≥ (s.get! r'.toNat).size then floor else
match (s.get! r'.toNat).get! c'.toNat with
| floor => s.get_first_in r' c' dr dc
| empty => empty
| occupied => occupied

instance : Monad List :=
{ pure := λ a => [a],
  map := List.map,
  bind := λ x f => (x.map f).join }

def diffs : List (Int × Int) := do
let dr ← [-1, 0, 1]
let dc ← [-1, 0, 1]
if dr == 0 && dc == 0 then []
pure (dr, dc)

def step (s : State) : State :=
Array.mkFun s.size $ λ row =>
Array.mkFun (s.get! 0).size $ λ col =>
let nbrs := (diffs.filter (λ ⟨dr, dc⟩ => s.get_first_in row col dr dc == occupied)).length
match (s.get! row).get! col with
| floor => floor
| empty => if nbrs == 0 then occupied else empty
| occupied => if nbrs ≥ 5 then empty else occupied

def count (s : State) : Int :=
s.foldl (λ c s' => c + s'.foldl (λ c' seat => c' + if seat == occupied then 1 else 0) 0) 0

partial def solve (s : State) : Int :=
-- dbgTrace s!"{count s}" $ λ _ =>
let s' := step s
if s == s' then count s else solve s'

def parse (str : Array String) : State :=
str.map $ λ s =>
Array.mk $ s.toList.map $ λ c =>
match c with
| '.' => floor
| 'L' => empty
| '#' => occupied
| _ => panic! "invalid input"

def main : IO Unit := do
  let input ← IO.FS.lines "a.in"
  IO.print s!"{solve (parse input)}\n"
