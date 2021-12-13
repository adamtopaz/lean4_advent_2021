import Std.Data.HashSet
open System Std

def fp : FilePath := "/home/adam/Lean4-Projects/advent2021/day13/input"

def foldx (H : HashSet (Nat × Nat)) (x : Nat) : HashSet (Nat × Nat) := do
let mut E : HashSet (Nat × Nat) := HashSet.empty
let A := H.toArray
for a in A do
  if a.fst < x then E := E.insert a
  else if a.fst > x then E := E.insert (2 * x - a.fst, a.snd)
return E

def foldy (H : HashSet (Nat × Nat)) (y : Nat) : HashSet (Nat × Nat) := do
let mut E : HashSet (Nat × Nat) := HashSet.empty
let A := H.toArray
for a in A do
  if a.snd < y then E := E.insert a
  else if a.snd > y then E := E.insert (a.fst, 2 * y - a.snd)
return E

def fold (H : HashSet (Nat × Nat)) (t : Char) (e : Nat) : HashSet (Nat × Nat) := 
if t == 'x' then foldx H e else if t == 'y' then foldy H e else HashSet.empty

def go : IO Nat := do
  let data := ((← IO.FS.lines fp).split (·.contains ','))
  let points := data.fst.map (λ S => S.split (· == ','))
  let folds := 
    ((data.snd.filter (· != "")).map (λ S => S.split (· == '='))).map
    (λ S => (if (S.get! 0).contains 'x' then 'x' else 'y', (S.get! 1).toNat!))
  let mut H : HashSet (Nat × Nat) := HashSet.empty
  for i in points do
    H := H.insert ((i.get! 0).toNat!, (i.get! 1).toNat!) 
  let e := folds[0]
  return (fold H e.fst e.snd).toList.length

def go2 : IO Unit := do
  let data := ((← IO.FS.lines fp).split (·.contains ','))
  let points := data.fst.map (λ S => S.split (· == ','))
  let folds := 
    ((data.snd.filter (· != "")).map (λ S => S.split (· == '='))).map
    (λ S => (if (S.get! 0).contains 'x' then 'x' else 'y', (S.get! 1).toNat!))
  let mut H : HashSet (Nat × Nat) := HashSet.empty
  for i in points do
    H := H.insert ((i.get! 0).toNat!, (i.get! 1).toNat!) 
  for e in folds do
    H := fold H e.fst e.snd
  let Mx : Nat := match (H.toList.map (λ t => t.fst)).maximum? with | some n => n | none => 0
  let My : Nat := match (H.toList.map (λ t => t.snd)).maximum? with | some n => n | none => 0
  for y in [0:My+1] do 
    let mut row : String := ""
    for x in [0:Mx+1] do
      if H.contains (x,y) then row := row ++ "#" else row := row ++ " "
    IO.println row

#eval go2