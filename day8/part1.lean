open System 

def fp : FilePath := "/home/adam/Lean4-Projects/advent2021/day8/input"

def condition (S : String) : Bool := 
[S.length == 2, S.length == 3, S.length == 4, S.length == 7].foldl or false 

def go : IO Nat := do
  let mut L := (← IO.FS.lines fp).toList.map 
    (λ t => (((t.split (· == '|')).get! 1).split (· == ' ')).tail!)
  L := L.map $ λ t => t.filter condition
  let E := L.map (λ t => t.length)
  let mut sum := 0
  for i in E do sum := sum + i
  IO.println (L.get! 6)
  return sum 

#eval go