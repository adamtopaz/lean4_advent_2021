open System

def fp : FilePath := "/home/adam/Lean4-Projects/advent2021/day2/input"

inductive Command 
| forward (n : Nat) : Command 
| down (n : Nat) : Command
| up (n : Nat) : Command
deriving Repr

structure Position where 
  depth : Nat
  dist  : Nat
  aim   : Nat

def initialPosition : Position := ⟨0,0,0⟩

def iterate : Position → Command → Position := λ x c =>
match c with 
| Command.down n => ⟨x.depth, x.dist, x.aim + n⟩
| Command.up n => ⟨x.depth, x.dist, x.aim - n⟩
| Command.forward n => ⟨x.depth + n * x.aim, x.dist + n, x.aim⟩

def String.toCommand (c : String) : Command := 
match c.splitOn with
| ["forward",num] => Command.forward num.toNat!
| ["down",num] => Command.down num.toNat!
| ["up",num] => Command.up num.toNat!
| _ => Command.forward 0

def day2_part2 : IO Nat := do
  let L := ((← IO.FS.lines fp).map (λ S => S.toCommand)).toList
  let P := List.foldl iterate initialPosition L
  return P.depth * P.dist

#eval day2_part2