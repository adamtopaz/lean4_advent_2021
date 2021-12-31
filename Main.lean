import Advent2021.day25.part1

def main : IO Unit := do
  let n := ← go
  IO.println n 