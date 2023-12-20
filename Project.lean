/-
This file will be the file which runs and displays the policy results. These policy results will
be matched up against the known solutions from the Book in the two examples which I have created.
In the case of policy improvment actions will be added by passing the update algorithms.
-/

def uniformFloat (resolution : Nat) : IO Float := do
  let n ← IO.rand 0 resolution
  return n.toFloat / resolution.toFloat

/-
Testing out the random generator in what will be the final project file
-/

def main : IO Unit := do
  let resolution : Nat := 1000
  let randomFloat ← uniformFloat resolution
  IO.println s!"Random float: {randomFloat}"
  if randomFloat > 0.5 then
    IO.println s!"Above 0.5"
  else
    IO.println "Below 0.5"
