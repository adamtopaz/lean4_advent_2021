import Std.Data.HashMap
import Std.Data.Stack
open System Std

def fp : FilePath := "/home/adam/Lean4-Projects/advent2021/day10/input"

def scores : HashMap String Nat := 
[(")", 3), ("]",57), ("}",1197), (">",25137)].foldl (λ H t => H.insert t.1 t.2) mkHashMap

def scores2 : HashMap String Nat := 
[(")", 1), ("]",2), ("}",3), (">",4)].foldl (λ H t => H.insert t.1 t.2) mkHashMap

def matching : HashMap String String := 
[("(", ")"), ("[","]"), ("{","}"), ("<",">")].foldl (λ H t => H.insert t.1 t.2) mkHashMap

def Char.foo (t : Char) : String := toString t

def opening : List String := ["(","[","{","<"]

def test : IO Unit := do
  let mut S : Stack Nat := Stack.empty
  S := S.push 1
  S := S.push 2
  S := S.push 3
  for i in S.vals do
    IO.println i
  
#eval test

def go : IO Nat := do
  let data := ((← IO.FS.lines fp).toList.map String.toList).map (λ L => L.map Char.foo)
  let mut score := 0
  for d in data do 
    let mut S : Stack String := Stack.empty
    for i in d do 
      if opening.contains i then do
        S := S.push i
      else if i == matching.find! S.peek! then S := S.pop
      else do
        score := score + (scores.find! i)
        break
  return score

partial def List.quicksort (L : List Nat) : List Nat := 
match L with 
| [] => []
| (x :: xs) => (xs.filter (· ≤ x)).quicksort ++ [x] ++ (xs.filter (· > x)).quicksort

def go2 : IO Nat := do
  let data := ((← IO.FS.lines fp).toList.map String.toList).map (λ L => L.map Char.foo)
  let mut scores : List Nat := [] 
  for d in data do 
    let mut good : Bool := true
    let mut S : Stack String := Stack.empty
    for i in d do 
      if opening.contains i then do
        S := S.push i
      else if i == matching.find! S.peek! then S := S.pop
      else do 
        good := false
        break
    if good then do 
      let E := (S.vals.reverse.map (λ t => matching.find! t))
      let mut s := 0
      for i in E do
        s := 5 * s + scores2.find! i
      scores := scores.append [s]
  return scores.quicksort.get! (scores.length/2)

#eval go2