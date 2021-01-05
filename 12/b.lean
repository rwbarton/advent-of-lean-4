structure Ship :=
x : Int
y : Int
dx : Int
dy : Int

instance : Inhabited Ship := ⟨⟨0,0,1,0⟩⟩

def fakeCos (n : Int) : Int :=
match (((n / 90) % 4 + 4) % 4 : Int) with
| 0 => 1
| 1 => 0
| 2 => -1
| 3 => 0
| _ => panic! "impossible"

def fakeSin (n : Int) : Int := fakeCos (90 - n)

def move (s : Ship) : Char → Int → Ship
| 'N', n => { s with dy := s.dy + n }
| 'S', n => { s with dy := s.dy - n }
| 'E', n => { s with dx := s.dx + n }
| 'W', n => { s with dx := s.dx - n }
| 'L', n => { s with dx := fakeCos n * s.dx - fakeSin n * s.dy, dy := fakeSin n * s.dx + fakeCos n * s.dy }
| 'R', n => { s with dx := fakeCos n * s.dx + fakeSin n * s.dy, dy := - fakeSin n * s.dx + fakeCos n * s.dy }
| 'F', n => { s with x := s.x + n * s.dx, y := s.y + n * s.dy }
| _, _ => panic! "invalid command"

def moveStr (s : Ship) (str : String) : Ship :=
match str.toList with
| c :: rest => move s c (String.mk rest).toNat!
| _ => panic! "invalid command"

def Int.abs (n : Int) : Int :=
if n < 0 then -n else n

def main : IO Unit := do
let instrs ← IO.FS.lines "a.in"
let result : Ship := instrs.foldl moveStr { x := 0, y := 0, dx := 10, dy := 1 }
IO.print s!"{result.x.abs + result.y.abs}\n"
