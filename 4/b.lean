import Lean.Data.Json.Parser

open Lean Lean.Quickparse Lean.Json.Parser

def qguard (p : Bool) : Quickparse Unit :=
if p then return () else fail "bad"

def byr : Quickparse Unit := do
  expect "byr:"
  let ⟨n, d⟩ ← natNumDigits
  qguard $ d == 4 && 1920 ≤ n && n ≤ 2002

def iyr : Quickparse Unit := do
  expect "iyr:"
  let ⟨n, d⟩ ← natNumDigits
  qguard $ d == 4 && 2010 ≤ n && n ≤ 2020

def eyr : Quickparse Unit := do
  expect "eyr:"
  let ⟨n, d⟩ ← natNumDigits
  qguard $ d == 4 && 2020 ≤ n && n ≤ 2030

def hgt : Quickparse Unit := do
  expect "hgt:"
  let ⟨n, d⟩ ← natNumDigits
  let c ← peek!
  if c == 'c' then do
    qguard $ 150 ≤ n && n ≤ 193
    expect "cm"
  else if c == 'i' then do
    qguard $ 59 ≤ n && n ≤ 76
    expect "in"
  else fail ""

def hcl : Quickparse Unit := do
  expect "hcl:#"
  let hex1 : Quickparse Unit := do
    let c ← next
    qguard $ '0' ≤ c && c ≤ '9' || 'a' ≤ c && c ≤ 'f'
  for i in [:6] do hex1

def ecl : Quickparse Unit := do
  expect "ecl:"
  let mut acc := ""
  for i in [:3] do
    acc := acc.push (← next)
  qguard $ List.elem acc ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

def pid : Quickparse Unit := do
  expect "pid:"
  for i in [:9] do
    let c ← next
    qguard c.isDigit

def testParse (p : Quickparse Unit) (f : String) : Bool :=
match (p.bind (λ _ => eoi)) f.mkIterator with
| Result.success _ _ => True
| Result.error _ _ => False

def isValid (para : String) : Bool :=
let fieldStrs : List String := para.split (λ c => c == ' ' || c == '\n');
List.all [byr, iyr, eyr, hgt, hcl, ecl, pid] $ λ p =>
List.any fieldStrs $ λ f =>
testParse p f

def solve (input : String) : Int :=
let paras := input.splitOn "\n\n"
(paras.filter isValid).length

def main : IO Unit := do
  let input ← IO.FS.readFile "a.in"
  IO.print s!"{solve input}\n"
