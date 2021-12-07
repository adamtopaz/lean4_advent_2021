import Std.Data.HashMap

open Std
open System

def fp : FilePath := "/home/adam/Lean4-Projects/advent2021/day6/input"

/- This works for small numbers.
def count (N : Nat) (n : Nat) : Nat := do
match N, n with 
| N, 0 => 1
| 0, (n+1) => count 6 n + count 8 n
| (N+1), (n+1) => count N n
-/

def go : IO Nat := do
  let F : List Nat := (((← IO.FS.lines fp).get! 0).split (· == ',')).map String.toNat!
  let mut H : HashMap (Nat × Nat) Nat := HashMap.empty
  for g in [0:300] do
    for a in [0:20] do
      if g == 0 then H := H.insert (a,g) 1 
      else if a == 0 then H := H.insert (0,g) (H.find! (6,g-1) + H.find! (8,g-1));
      else H := H.insert (a,g) (H.find! (a-1,g-1))
  let mut sum := 0
  for i in F do sum := sum + H.find! (i,256)
  return sum

#eval go
