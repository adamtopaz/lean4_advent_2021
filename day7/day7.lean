open System

def Int.abs (a : Int) : Int := if a < 0 then -a else a

def computeGas (L : List Nat) (n : Nat) : Nat := do
let L' : List Nat := L.map (λ t => if t ≤ n then n - t else t - n)
let mut sum := 0 
for i in L' do
  sum := sum + i
return sum

def binom (n : Nat) : Nat := n * (n+1) / 2

def computeGas2 (L : List Nat) (n : Nat) : Nat := do
let L' : List Nat := L.map (λ t => if t ≤ n then n - t else t - n)
let mut sum := 0 
for i in L' do
  sum := sum + binom i
return sum

def fp : FilePath := "/home/adam/Lean4-Projects/advent2021/day7/input"

def go : IO Nat := do
  let F := (((← IO.FS.lines fp).get! 0).split (· == ',')).map String.toNat!
  let max : Nat := 
    match F.maximum? with
    | none => 0
    | some n => n
  let G : List Nat := (List.range max).map (λ i => computeGas F i)
  match G.minimum? with 
  | none => return 0
  | some n => return n

def go2 : IO Nat := do
  let F := (((← IO.FS.lines fp).get! 0).split (· == ',')).map String.toNat!
  let max : Nat := 
    match F.maximum? with
    | none => 0
    | some n => n
  let G : List Nat := (List.range max).map (λ i => computeGas2 F i)
  match G.minimum? with 
  | none => return 0
  | some n => return n

#eval go
#eval go2