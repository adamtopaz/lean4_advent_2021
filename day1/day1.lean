open System 

def countIncrease : List Nat → Nat 
| [] => 0
| (x :: []) => 0
| (x :: (y :: ys)) => if x < y then 1 + countIncrease (y :: ys) else countIncrease (y :: ys)

def tripleSums : List Nat → List Nat 
| [] => [] 
| (x :: []) => []
| (x :: y :: []) => []
| (x :: y :: z :: zs) => (x + y + z) :: tripleSums (y :: z :: zs)

def fp : FilePath := "/home/adam/Lean4-Projects/advent2021/day1/input"

def evalFunOnNatList (f : List Nat → α) (fp : FilePath) : IO α := do
  let L := (← IO.FS.lines fp).map (λ S => S.toNat!)
  return f L.toList

#eval evalFunOnNatList countIncrease fp
#eval evalFunOnNatList (countIncrease ∘ tripleSums) fp