import Std.Data.HashMap
import Std.Data.HashSet

open System Std

def fp : FilePath := "/home/adam/Lean4-Projects/advent2021/day14/input"

instance : Hashable Char where hash := hash ∘ toString

structure System where
  count : HashMap Char Nat
  pairs : HashMap String Nat

structure Rule where
  input : String
  output : Char
deriving Inhabited

instance : ToString Rule where toString r := toString r.input ++ " -> " ++ toString r.output

def String.toRule (S : String) : Rule := 
let T := S.toList.toArray
⟨(T[0].toString ++ T[1].toString), T[T.size-1]⟩

def increase [Hashable α] [BEq α] (H : HashMap α Nat) (a : α) (n : Nat) : HashMap α Nat := 
match H.find? a with | some t => H.insert a (t + n) | none => H.insert a n

def decrease [Hashable α] [BEq α] (H : HashMap α Nat) (a : α) (n : Nat) : HashMap α Nat := 
match H.find? a with | some t => H.insert a (t - n) | none => H

def iterate (S : System) (R : List Rule) : System := Id.run $ do 
let mut count : HashMap Char Nat := S.count
let mut pairs : HashMap String Nat := S.pairs
for (p,n) in pairs.toList do 
  let ⟨_,c⟩ := (R.filter (λ t => t.input == p)).get! 0
  let c1 := p.toList.toArray[0].toString ++ c.toString
  let c2 := c.toString ++ p.toList.toArray[1].toString
  count := increase count c n
  pairs := decrease pairs p n
  pairs := increase pairs c1 n
  pairs := increase pairs c2 n
return ⟨count,pairs⟩

def pairs (S : String) : List String := 
let E := S.toList
let F := match E with
| [] => []
| (x :: xs) => E.zip xs
F.map $ λ t => t.1.toString ++ t.2.toString

def go : IO Nat := do
  let data := ((← IO.FS.lines fp).filter (· != "")).split (·.length > 10)
  let init := data.fst[0]
  let rules : List Rule := data.snd.toList.map String.toRule
  let mut initialCount : HashMap Char Nat := HashMap.empty
  for i in init.toList do
    match initialCount.find? i with 
    | some n => initialCount := initialCount.insert i (n+1)
    | none => initialCount := initialCount.insert i 1 
  let mut initialPairs : HashMap String Nat := HashMap.empty
  for i in pairs init do
    match initialPairs.find? i with 
    | some n => initialPairs := initialPairs.insert i (n+1)
    | none => initialPairs := initialPairs.insert i 1
  let mut initialSystem : System := ⟨initialCount, initialPairs⟩ 
  for _ in [0:40] do initialSystem := iterate initialSystem rules
  let E := initialSystem.count.toList.map Prod.snd
  match E.maximum?, E.minimum? with 
  | some a, some b => return (a-b)
  | _, _ => return 0

#eval go