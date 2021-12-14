import Std.Data.HashMap

open System Std

structure Rule where
  input : Char × Char
  output : Char
deriving Inhabited 

instance : ToString Rule where
  toString r := toString r.input ++ " -> " ++ toString r.output

def String.toRule (S : String) : Rule := 
let T := S.toList.toArray
⟨(T[0],T[1]), T[T.size-1]⟩

def Rules := Array Rule deriving ToString

def Rules.find (R : Rules) (t : Char × Char) : Rules := 
R.filter $ λ i => i.input == t

def Rules.get (R : Rules) (t : Char × Char) : Option Rule := 
(R.find t).get? 0

def List.pairs (S : List α) : List (α × α) := 
match S with
| [] => []
| (x :: []) => []
| (x :: y :: ys) => (x, y) :: ys.pairs

def String.pairs (S : String) : List (Char × Char) := S.toList.pairs

def iterate (S : String) (rules : Rules) : String := Id.run $ do 
let mut out : String := ""
for t in S.toList.zip S.toList.tail! do
  match rules.get t with 
  | some ⟨_,c⟩ => out := out ++ toString t.1 ++ toString c 
  | none => out := out ++ toString t.1
--  out := out ++ toString t.2
out := out ++ toString S.toList.reverse.head!
return out

def fp : FilePath := "/home/adam/Lean4-Projects/advent2021/day14/input"

instance : Hashable Char where hash c := hash (toString c)

def go : IO Nat := do
  let data := ((← IO.FS.lines fp).filter (· != "")).split (·.length > 10)
  let mut init := data.fst[0]
  let rules : Rules := data.snd.map String.toRule
  IO.println init
  for i in [0:10] do
    init := iterate init rules
    --IO.println init
  let mut H : HashMap Char Nat := HashMap.empty
  for i in init.toList do 
    H := H.insert i (H.find! i + 1)
  let S : List Nat := H.toList.map Prod.snd
  match S.maximum?, S.minimum? with
  | some a, some b => return a - b
  | _, _ => return 0

#eval go