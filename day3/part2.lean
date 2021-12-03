open System

def fp : FilePath := "/home/adam/Lean4-Projects/advent2021/day3/input"

/-- Convert a character to a bit. -/
def Char.toBool (S : Char) : Bool := 
match S with
| '0' => false 
| '1' => true
| _   => false

/-- Count the number of zeros and ones in a list of bits. -/
def List.count (L : List Bool) : Nat × Nat :=
match L with 
| [] => (0,0)
| (x :: xs) => 
    if x 
    then ((count xs).fst, (count xs).snd + 1)  -- add one to true
    else ((count xs).fst + 1, (count xs).snd)  -- add one to false

/-- Convert a string to a list of Bools. -/
def mkBitList (S : String) : List Bool := S.toList.map Char.toBool

def List.col (L : List (List α)) [Inhabited α] (n : Nat) : List α := 
L.map (List.get! n)

def List.maxLen (L : List (List α)) : Nat :=
match (L.map List.length).maximum? with
| none => 0
| some n => n

def List.cols (L : List (List α)) [Inhabited α] : List (List α) :=
(List.range L.maxLen).map (col L)

partial def List.fromBinary (L : List Bool) : Nat :=
match L.reverse with 
| [] => 0
| (x :: xs) => (if x then 1 else 0) + 2 * xs.reverse.fromBinary

def filter (L : List α) (F : List Bool) : List α := 
match L, F with 
| [], [] => []
| (x :: xs), [] => (x :: xs)
| [], (y :: ys) => []
| (x :: xs), (y :: ys) => if y then x :: filter xs ys else filter xs ys

def go1 (L : List (List Bool)) : List Bool := do
let N := L.maxLen
let mut L' := L
for i in List.range N do
  if L'.length == 1 then return L'.get! 0 else
  let F := L'.col i
  let C := F.count
  if C.fst > C.snd 
  then L' := filter L' (F.map not) 
  else L' := filter L' F 
return L'.get! 0

#eval go1 
  [ [true,false,true,true]
  , [true,false,true,false]
  , [true,true,false,false]
  , [false,false,true,false]
  ]

def go2 (L : List (List Bool)) : List Bool := do
let N := L.maxLen
let mut L' := L
for i in List.range N do
  if L'.length == 1 then return L'.get! 0 else
  let F := L'.col i
  let C := F.count
  if C.fst > C.snd 
  then L' := filter L' F 
  else L' := filter L' (F.map not) 
return L'.get! 0

#eval go2 
  [ [true,false,true,true]
  , [true,false,true,false]
  , [true,true,false,false]
  , [false,false,true,false]
  ]

def solution : IO Nat := do
  let L := ((← IO.FS.lines fp).toList.map mkBitList)
  let A := (go1 L).fromBinary
  IO.println A
  let B := (go2 L).fromBinary
  IO.println B
  return A * B

#eval solution