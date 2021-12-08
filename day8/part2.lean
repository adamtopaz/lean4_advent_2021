open System

structure Set (α) where
  data : List α
deriving Repr

instance [ToString α] : ToString (Set α) where
  toString t := toString t.data

def Set.empty (α) : Set α := ⟨[]⟩

def Set.insert (S : Set α) (a : α) : Set α := ⟨a :: S.data⟩

def Set.contains [BEq α] (S : Set α) (a : α) : Bool := 
(S.data.map (· == a)).foldl or false

def Set.union (S T : Set α) : Set α := ⟨S.data ++ T.data⟩

def Set.inter [BEq α] (S T : Set α) : Set α := do
let mut D : List α := []
for i in S.data do
  if T.contains i then D := i :: D
return ⟨D⟩

def Set.compl [BEq α] (S T : Set α) : Set α := do
let mut D : List α := []
for i in S.data do
  if not (T.contains i) then D := i :: D
return ⟨D⟩

def Set.get [Inhabited α] (S : Set α) : α := S.data.get! 0

def Set.size [BEq α] (S : Set α) : Nat := do
let mut D : List α := []
for i in S.data do
  if not ((D.map (· == i)).foldl or false) then D := i :: D 
return D.length

def Set.map (S : Set α) (f : α → β) : Set β := ⟨S.data.map f⟩

instance : Inhabited (Set α) := ⟨Set.empty _⟩

def List.toSet (L : List α) : Set α := ⟨L⟩

def String.toSet (S : String) : Set Char := S.toList.toSet

-- Now the puzzle starts

def Set.isOne (S : Set Char) : Bool := S.size == 2
def Set.isSeven (S : Set Char) : Bool := S.size == 3
def Set.isFour (S : Set Char) : Bool := S.size == 4
def Set.isEight (S : Set Char) : Bool := S.size == 7

def List.getOne (L : List (Set Char)) : Set Char := (L.filter Set.isOne).get! 0
def List.getSeven (L : List (Set Char)) : Set Char := (L.filter Set.isSeven).get! 0
def List.getFour (L : List (Set Char)) : Set Char := (L.filter Set.isFour).get! 0
def List.getEight (L : List (Set Char)) : Set Char := (L.filter Set.isEight).get! 0

def List.count (L : List (Set Char)) (a : Char) : Nat := 
(L.filter (λ S => S.contains a)).length

/-
Reference:

 AAA
B   C
B   C
 DDD
E   F
E   F
 GGG

How many times each line appears
A : 8 x
B : 6 x
C : 8 x
D : 7 x
E : 4 x
F : 9 X
G : 7 x
-/

def List.getCF (L : List (Set Char)) : Set Char := L.getOne
def List.getBD (L : List (Set Char)) : Set Char := L.getFour.compl L.getOne
def List.getEG (L : List (Set Char)) : Set Char := (L.getEight.compl L.getSeven).compl L.getFour

-- The following takes a list of sets of characters and returns what should be the character corresponding to
-- each of A,...,G
def List.A (L : List (Set Char)) : Char := (L.getSeven.compl L.getOne).get
def List.F (L : List (Set Char)) : Char := (['a','b','c','d','e','f','g'].filter $ λ t => L.count t == 9).get! 0
def List.B (L : List (Set Char)) : Char := (['a','b','c','d','e','f','g'].filter $ λ t => L.count t == 6).get! 0
def List.E (L : List (Set Char)) : Char := (['a','b','c','d','e','f','g'].filter $ λ t => L.count t == 4).get! 0
def List.C (L : List (Set Char)) : Char := (L.getCF.compl [L.F].toSet).get
def List.D (L : List (Set Char)) : Char := (L.getBD.compl [L.B].toSet).get
def List.G (L : List (Set Char)) : Char := (L.getEG.compl [L.E].toSet).get

def Char.transform (t : Char) (L : List (Set Char)) : Char := 
if t == L.A then 'A'
else if t == L.B then 'B'
else if t == L.C then 'C'
else if t == L.D then 'D'
else if t == L.E then 'E'
else if t == L.F then 'F'
else if t == L.G then 'G'
else ' '

instance [BEq α] : BEq (Set α) where
  beq (S T) := ((S.compl T).union (T.compl S)).size == 0

/-
 AAA
B   C
B   C
 DDD
E   F
E   F
 GGG
-/

def Set.digit (S : Set Char) : Nat := 
if S == ['A','B','C','E','F','G'].toSet then 0
else if S == ['C','F'].toSet then 1
else if S == ['A','C','D','E','G'].toSet then 2
else if S == ['A','C','D','F','G'].toSet then 3
else if S == ['B','C','D','F'].toSet then 4
else if S == ['A','B','D','F','G'].toSet then 5
else if S == ['A','B','D','E','F','G'].toSet then 6
else if S == ['A','C','F'].toSet then 7
else if S == ['A','B','C','D','E','F','G'].toSet then 8
else if S == ['A','B','C','D','F','G'].toSet then 9
else 0

def List.toNat (L : List Nat) : Nat := do
let T := L.reverse
let mut i := 0
let mut sum := 0
for t in T do
  sum := sum + 10^i * t
  i := i + 1
return sum

def fp : FilePath := "/home/adam/Lean4-Projects/advent2021/day8/input"

def go : IO Nat := do
  let E := (← IO.FS.lines fp).toList.map (λ S => S.split (· == '|'))
  let mut sum := 0
  for e in E do 
    let H := (((e.get! 0).split (· == ' ')).filter (· != "")).map String.toSet
    let T := ((((((e.get! 1).split (· == ' ')).filter (· != "")).map String.toSet).map 
      (λ U => U.map (λ i => i.transform H))).map Set.digit).toNat
    sum := sum + T
  return sum

#eval go