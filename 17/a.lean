instance : Monad List :=
{ pure := λ a => [a],
  map := List.map,
  bind := λ x f => (x.map f).join }

def fromCoord (x y z : Int) : Nat :=
Int.toNat (400 * x + 20 * y + z)

@[reducible] def Grid : Type := Array Bool

def Grid.get (g : Grid) (x y z : Int) : Bool :=
if 0 ≤ x ∧ x < 13 ∧ 0 ≤ y ∧ y < 20 ∧ 0 ≤ z ∧ z < 20
then g.get! (fromCoord x y z) else false

def Grid.neighborCount (g : Grid) (x y z : Int) : Nat :=
List.length $ do
  let dx ← [(-1 : Int), 0, 1]
  let dy ← [(-1 : Int), 0, 1]
  let dz ← [(-1 : Int), 0, 1]
  if dx == 0 ∧ dy == 0 ∧ dz == 0 then []
  if g.get (x+dx) (y+dy) (z+dz) then [()] else []

def Grid.evolve (g : Grid) : Grid :=
Array.mk $ do
  let x ← List.range 13
  let y ← List.range 20
  let z ← List.range 20
  let c := g.neighborCount x y z
  pure $ if g.get x y z then c == 2 || c == 3 else c == 3

def main : IO Unit := do
let input ← IO.FS.lines "a.in"
let mut g : Grid := Array.mkArray (13*20*20) false
for i in [0:input.size] do
  for j in [0:(input.get! i).length] do
    g := g.set! (fromCoord 6 ((i:Int)+6) ((j:Int)+6)) ((input.get! i).get j == '#')
for _ in [0:6] do
  g := g.evolve
IO.print s!"{(g.toList.filter id).length}\n"
