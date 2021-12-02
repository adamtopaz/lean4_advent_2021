open System

def fp : FilePath := "/home/adam/Lean4-Projects/advent2021/day2/input"

inductive Command
| forward (n : Nat) : Command 
| down (n : Nat) : Command
| up (n : Nat) : Command
deriving Repr

structure Position :=
(depth : Nat)
(dist : Nat)

def initialPosition : Position := ⟨0,0⟩

def iterate : Position → Command → Position := λ x c =>
match c with 
| Command.forward n => ⟨x.depth, x.dist + n⟩
| Command.down n => ⟨x.depth + n, x.dist⟩
| Command.up n => ⟨x.depth - n, x.dist⟩

def String.toCommand (c : String) : Command := 
match c.splitOn with
| ["forward",num] => Command.forward num.toNat!
| ["down",num] => Command.down num.toNat!
| ["up",num] => Command.up num.toNat!
| _ => Command.forward 0

def day2_part1 : IO Nat := do
  let L := ((← IO.FS.lines fp).map (λ S => S.toCommand)).toList
  let P := List.foldl iterate initialPosition L
  return P.depth * P.dist

#eval day2_part1