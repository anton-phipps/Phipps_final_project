/-
The Random Float generator that will give a value between 0 and 1
-/
def uniformFloat (resolution : Nat) : IO Float := do
  let n ← IO.rand 0 resolution
  return n.toFloat / resolution.toFloat

/-
This structure represents the policy, esentially what are the odds of the agent
choosing to go in the specified direction.
-/
structure Policy :=
  up : Float
  down : Float
  left : Float
  right : Float
deriving Repr

instance : Inhabited Policy :=
  ⟨
    { up := 0.25, down := 0.25, left := 0.25, right := 0.25}
  ⟩

/-
An Enumeration for movement direction
-/
inductive Move
| up
| down
| left
| right

/-
This structure is an internal representation of the Gridworld for an agent working through
the world. The worlds are tabular with discrete spaces. The behaviour of the Gridworld is
managed by a function. This also will decide if the behaviour is episodic or continuous.
-/
structure GridWorld :=
  nRows : Nat -- Number of rows in the Grid
  nColumns : Nat -- Number of coluns in the Grid
  stateValues := mkArray (nRows * nColumns) (0 : Float) -- Creating a array of state values
  -- Start the policy by default of moving in all directions (actions) with an equal probability
  -- for all states
  policy := mkArray (nRows * nColumns) ({up := 0.25, down := 0.25, left := 0.25, right := 0.25} : Policy)
deriving Repr

/-
A structure that carries the agent state, including the value of the world and actions
that it will take in each state
-/
structure Agent :=
  row : Nat
  column : Nat
  states : GridWorld
deriving Repr

/-
A function that displays all the agent states in a formatted way
-/
def printAgentStates (a : Agent) :=
  let range : Nat := a.states.nRows * a.states.nColumns
  for i in [0 : range] do
    if i % a.states.nColumns = 0 then
      IO.println "\n"
    IO.print s!"{a.states.stateValues[i]!}\t"




def agentPosition (a : Agent) : Nat :=
  a.row * a.states.nColumns + a.column

/-
This function handles the behaviour of the Gridworld in Example 3.5 of
Sutton, R. S., & Barto, A. G. (2018). Reinforcement Learning: An Introduction. MIT Press.
This will be the environment that will be used to get state values and polices
-/
def gridWorld35 (a : Agent) (m : Move) : Float × Agent :=
  match m with
  | Move.up =>
    let newAgent : Agent :=
    {
      row :=
        if a.row = 0 && a.column = 1 then 4
        else if a.row = 0 && a.column = 3 then 2
        else if a.row = 0 then 0
        else a.row - 1,
      column := a.column,
      states := a.states
    }
    let reward : Float :=
      if a.row = 0 && a.column = 1 then
        10
      else if a.row = 0 && a.column = 3 then
        5
      else if a.row = 0 then
        -1
      else
        0
    (reward, newAgent)
  | Move.down =>
    let newAgent : Agent :=
    {
      row :=
        if a.row = 0 && a.column = 1 then 4
        else if a.row = 0 && a.column = 3 then 2
        else if a.row + 1 = a.states.nRows then a.row
        else a.row + 1,
      column := a.column,
      states := a.states
    }
    let reward : Float :=
      if a.row = 0 && a.column = 1 then
        10
      else if a.row = 0 && a.column = 3 then
        5
      else if a.row + 1 = a.states.nRows then
        -1
      else
        0
    (reward, newAgent)
  | Move.left =>
    let newAgent : Agent :=
    {
      row :=
        if a.row = 0 && a.column = 1 then 4
        else if a.row = 0 && a.column = 3 then 2
        else a.row,
      column :=
        if a.row = 0 && a.column = 1 then 1
        else if a.row = 0 && a.column = 3 then 3
        else if a.column = 0 then 0
        else a.column - 1,
      states := a.states
    }
    let reward : Float :=
      if a.row = 0 && a.column = 1 then
        10
      else if a.row = 0 && a.column = 3 then
        5
      else if a.column = 0 then
        -1
      else
        0
    (reward, newAgent)
  | Move.right =>
    let newAgent : Agent :=
    {
      row :=
        if a.row = 0 && a.column = 1 then 4
        else if a.row = 0 && a.column = 3 then 2
        else a.row,
      column :=
        if a.row = 0 && a.column = 1 then 1
        else if a.row = 0 && a.column = 3 then 3
        else if a.column + 1 = a.states.nColumns then a.column
        else a.column + 1,
      states := a.states
    }
    let reward : Float :=
      if a.row = 0 && a.column = 1 then
        10
      else if a.row = 0 && a.column = 3 then
        5
      else if a.column + 1 = a.states.nColumns then
        -1
      else
        0
    (reward, newAgent)

/-
This function handles the behaviour of the Gridworld in Example 4.1 of
Sutton, R. S., & Barto, A. G. (2018). Reinforcement Learning: An Introduction. MIT Press.
This will be the environment that will be used to get state values and polices of a
terminating discrete process
-/
def gridWorld41 (a : Agent) (m : Move) : Float × Agent :=
  match m with
  | Move.up =>
    let newAgent : Agent :=
    {
      row :=
        if a.row = 0 then 0
        else a.row - 1,
      column := a.column,
      states := a.states
    }
    let reward : Float :=
      if (newAgent.row = 0 && newAgent.column = 0) ||
        (newAgent.row = newAgent.states.nRows - 1 && newAgent.column = newAgent.states.nColumns - 1) then
        0
      else
        -1
    (reward, newAgent)
  | Move.down =>
    let newAgent : Agent :=
    {
      row :=
        if a.row + 1 = a.states.nRows then a.row
        else a.row + 1,
      column := a.column,
      states := a.states
    }
    let reward : Float :=
      if (newAgent.row = 0 && newAgent.column = 0) ||
        (newAgent.row = newAgent.states.nRows - 1 && newAgent.column = newAgent.states.nColumns - 1) then
        0
      else
        -1
    (reward, newAgent)
  | Move.left =>
    let newAgent : Agent :=
    {
      row := a.row,
      column :=
        if a.column = 0 then 0
        else a.column - 1,
      states := a.states
    }
    let reward : Float :=
      if (newAgent.row = 0 && newAgent.column = 0) ||
        (newAgent.row = newAgent.states.nRows - 1 && newAgent.column = newAgent.states.nColumns - 1) then
        0
      else
        -1
    (reward, newAgent)
  | Move.right =>
    let newAgent : Agent :=
    {
      row := a.row,
      column :=
        if a.column + 1 = a.states.nColumns then a.column
        else a.column + 1,
      states := a.states
    }
    let reward : Float :=
      if (newAgent.row = 0 && newAgent.column = 0) ||
        (newAgent.row = a.states.nRows - 1 && newAgent.column = a.states.nColumns - 1) then
        0
      else
        -1
    (reward, newAgent)

def tdUpdate (a : Actor) : IO Unit :=
  sorry
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
