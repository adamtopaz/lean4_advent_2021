open System

inductive Node where
| down : Node
| right : Node
| empty : Node
deriving BEq

structure Board where
  data : Array (Array Node)

instance : ToString Board where
  toString B := Id.run $ do 
    let mut out := ""
    for i in B.data do 
      for j in i do
        out := out ++ if j == Node.down then "v" else if j == Node.right then ">" else "."
      out := out ++ "\n"
    return out

def Nat.truncate (n m : Nat) : Fin m := ⟨n % m, sorry⟩

def Board.get (B : Board) (i j : Nat) : Node := 
(B.data.get (i.truncate _)).get (j.truncate _)

def Board.empty (m n : Nat) : Board := 
⟨Array.mkArray m (Array.mkArray n Node.empty)⟩

def Board.update (B : Board) (m n : Nat) (e : Node) : Board := 
⟨B.data.set (m.truncate _) ((B.data.get (m.truncate _)).set (n.truncate _) e)⟩

def Board.dim (B : Board) : Nat × Nat := 
(B.data.size, (B.data.get! 0).size)

def Board.iterate (B : Board) : Board := Id.run $ do
let ⟨x,y⟩ := B.dim
let mut aux := B
for i in [0:x] do 
  for j in [0:y] do 
    let current := B.get i j
    let right := B.get i (j+1)
    match current, right with
    | Node.right, Node.empty =>
        aux := (aux.update i j Node.empty).update i (j+1) Node.right
    | _, _ => continue
let mut out := aux
for i in [0:x] do 
  for j in [0:y] do
    let current := aux.get i j
    let down := aux.get (i+1) j
    match current, down with
    | Node.down, Node.empty =>
        out := (out.update i j Node.empty).update (i+1) j Node.down
    | _, _ => continue
return out

def fp : FilePath := "/home/adam/Lean4-Projects/advent2021/Advent2021/day25/input"

instance : BEq Board where
  beq A B := A.data == B.data

def parseNode (S : Char) : Node := 
match S with 
| '>' => Node.right
| 'v' => Node.down
| _ => Node.empty

partial def loop (a : IO Unit) : IO Unit := do
  a; loop a

partial def go : IO Nat := do
  let mut b : Board := Board.mk $ 
    ((← IO.FS.lines fp).map String.toList).map (λ L => (L.map parseNode).toArray)
  let rec loop (n : Nat) (b : Board) : IO Nat := do
    let c := b
    let b := b.iterate
    IO.sleep 10
    IO.println c
    if c == b then return (n+1) else loop (n+1) b
  loop 0 b