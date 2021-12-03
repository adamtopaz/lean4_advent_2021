open System

def fp : FilePath := "/home/adam/Lean4-Projects/advent2021/day3/input"

/-- Convert a character to a bit. -/
def Char.toFin2 (S : Char) : Fin 2 := 
match S with
| '0' => 0
| '1' => 1
| _   => 0

/-- Count the number of zeros and ones in a list of bits. -/
def count (L : List (Fin 2)) : Nat × Nat :=
match L with 
| [] => (0,0)
| (x :: xs) => 
    if x == 0 
    then ((count xs).fst + 1, (count xs).snd) 
    else ((count xs).fst, (count xs).snd + 1) 

/-- Convert a string to a list of bits. -/
def mkBitList (S : String) : List (Fin 2) :=
S.toList.map Char.toFin2

def col (L : List (List (Fin 2))) (n : Nat) : List (Fin 2) := 
L.map (List.get! n)

def List.maxLen (L : List (List α)) : Nat :=
match (L.map List.length).maximum? with
| none => 0
| some n => n

def List.cols (L : List (List (Fin 2))) : List (List (Fin 2)) :=
(List.range L.maxLen).map (col L)

partial def List.fromBinary (L : List (Fin 2)) : Nat :=
match L.reverse with 
| [] => 0
| (x :: xs) => x + 2 * xs.reverse.fromBinary

def solution : IO Nat := do
  let L := ((← IO.FS.lines fp).toList.map mkBitList).cols.map count
  let A := L.map (λ i => if i.fst > i.snd then (0 : Fin 2) else (1 : Fin 2))
  let B := L.map (λ i => if i.fst < i.snd then (0 : Fin 2) else (1 : Fin 2))
  return A.fromBinary * B.fromBinary

#eval solution