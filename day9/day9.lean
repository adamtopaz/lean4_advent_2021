open System

def fp : FilePath := "/home/adam/Lean4-Projects/advent2021/day9/input"

def Char.digit (e : Char) : Nat :=  
match e with 
| '0' => 0
| '1' => 1
| '2' => 2
| '3' => 3
| '4' => 4
| '5' => 5
| '6' => 6
| '7' => 7
| '8' => 8
| '9' => 9
| _   => 0

def adjacent (H W : Nat) (h w : Nat) : List (Nat × Nat) :=
if h = 0 ∧ w = 0 then [(1,0),(0,1)] -- top left corner
else if h = 0 ∧ w = W-1 then [(0,W-2),(1,W-1)] -- top right corner
else if h = H-1 ∧ w = 0 then [(H-1,1),(H-2,0)] -- bottom left corner
else if h = H-1 ∧ w = W - 1 then [(H-2,W-1),(H-1,W-2)] -- bottom right corner
else if h = 0 then [(0,w-1),(0,w+1),(1,w)] -- top row
else if h = H-1 then [(h,w-1),(h,w+1),(h-1,w)] -- bottom row
else if w = 0 then [(h-1,0),(h+1,0),(h,1)] -- left row
else if w = W-1 then [(h-1,W-1),(h+1,W-1),(h,W-2)] -- right row
else [(h-1,w),(h+1,w),(h,w+1),(h,w-1)] -- generic

-- The list of locations higher than the given location, and which are not at height 9.
def flowsFrom (f : Nat × Nat → Nat) (H W : Nat) (h w : Nat) : List (Nat × Nat) :=
(adjacent H W h w).filter (λ t => f (h,w) < f t ∧ f t != 9)

def isLocalMin (f : Nat × Nat → Nat) (H W : Nat) (h w : Nat) : Bool :=
f (h,w) < ((adjacent H W h w).map f).foldl min 10

def List.mem (L : List α) (a : α) [BEq α] : Bool := 
match L with 
| [] => false
| x :: xs => if a == x then true else xs.mem a

def List.dedup (L : List α) [BEq α] : List α := 
match L with 
| [] => []
| x :: xs => if xs.mem x then xs.dedup else x :: xs.dedup

partial def findRegion (f : Nat × Nat → Nat) (H W : Nat) (h w : Nat) : List (Nat × Nat) :=
let A : List (List (Nat × Nat)) := (flowsFrom f H W h w).map (λ t => findRegion f H W t.1 t.2)
((h,w) :: (A.foldl (· ++ ·) [])).dedup

def findRegionSize (f : Nat × Nat → Nat) (H W : Nat) (h w : Nat) : Nat := 
(findRegion f H W h w).length

def go : IO Nat := do
  let data := ((← IO.FS.lines fp).toList.map String.toList).map
    (λ L => L.map Char.digit)
  let H := data.length
  let W := (data.get! 0).length
  let mut sum := 0
  let f : Nat × Nat → Nat := λ t => (data.get! t.1).get! t.2
  for i in [0:H] do for j in [0:W] do
    if isLocalMin f H W i j then sum := sum + 1 + f (i,j)
  return sum 

-- Do we have quicksort somewhere?
partial def List.sort (L : List Nat) : List Nat := 
match L with 
| [] => []
| x :: xs => (xs.filter (· ≤ x)).sort ++ [x] ++ (xs.filter (x < ·)).sort

#eval [5,1,2,65,110,1,1,1,1,1,0,2,3,1,54,12,23,2].sort

def go2 : IO Nat := do
  let data := ((← IO.FS.lines fp).toList.map String.toList).map
    (λ L => L.map Char.digit)
  let H := data.length
  let W := (data.get! 0).length
  let f : Nat × Nat → Nat := λ t => (data.get! t.1).get! t.2
  let mut mins : List (Nat × Nat) := []
  for i in [0:H] do for j in [0:W] do if isLocalMin f H W i j then mins := (i,j) :: mins
  let mut E : List Nat := (mins.map (λ t => findRegionSize f H W t.1 t.2))
  E := E.sort.reverse
  return (E.get! 0) * (E.get! 1) * (E.get! 2)

#eval go
#eval go2