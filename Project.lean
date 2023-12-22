/-
This structure represents the policy, esentially what are the odds of the agent
choosing to go in the specified direction.
-/
structure Policy :=
  left : Float
  right : Float
deriving Repr

instance : Inhabited Policy :=
  ⟨
    { left := 0.5, right := 0.5}
  ⟩

/-
The Random Float generator that will give a value between 0 and 1
-/
def uniformFloat (resolution : Nat) : IO Float := do
  let n ← IO.rand 0 resolution
  return n.toFloat / resolution.toFloat

/-
An Enumeration for movement direction
-/
inductive Move
| left
| right
deriving Repr

/-
This structure is an internal representation of the Random Walk to use the TD(0) algorithm
for estimating state values. How this differes from the last example is this learns from
experience and also updates the values
-/
structure RandomWalk :=
  states : Nat -- Number of states
  stateValues := mkArray (states) (0 : Float) -- Creating a array of state values
  policy := ({left := 0.5, right := 0.5} : Policy)
deriving Repr

/-
A structure that carries the agent state, including the value of the world and actions
that it will take in each state
-/
structure Agent :=
  currentPosition : Nat
  environment : RandomWalk
deriving Repr

def printAgentStates (a : Agent) :=
  let range : Nat := a.environment.states
  for i in [0 : range] do
    IO.print s!"{a.environment.stateValues[i]!}\t"

def tdUpdate (a : Agent) (m : Move) (α γ : Float) : Agent :=
  match m with
  | Move.right =>
    let newPosition := a.currentPosition + 1
    let reward : Float :=
      if newPosition = a.environment.states + 1 then
        1
      else
        0
    let Vs : Float := a.environment.stateValues[a.currentPosition - 1]!
    let Vs': Float :=
      if newPosition = a.environment.states + 1 then
        0
      else
        a.environment.stateValues[newPosition - 1]!
    let newVs := Vs + α * (reward + γ * Vs' - Vs)
    let newArr := a.environment.stateValues.set! (a.currentPosition - 1) newVs
    {
      currentPosition := newPosition,
      environment :=
      {
        states := a.environment.states,
        stateValues := newArr,
        policy := a.environment.policy
      }
    }

  | Move.left =>
    let newPosition := a.currentPosition - 1
    let reward : Float := 0
    let Vs : Float := a.environment.stateValues[a.currentPosition - 1]!
    let Vs': Float :=
      if newPosition = 0 then
        0
      else
        a.environment.stateValues[newPosition - 1]!
    let newVs := Vs + α * (reward + γ * Vs' - Vs)
    let newArr := a.environment.stateValues.set! (a.currentPosition - 1) newVs
    {
      currentPosition := newPosition,
      environment :=
      {
        states := a.environment.states,
        stateValues := newArr,
        policy := a.environment.policy
      }
    }

/-

-/
partial def runEpisode (a : Agent) (α γ : Float) : IO Agent := do
  if a.currentPosition = 0 || a.currentPosition = a.environment.states + 1 then
    return a
  else do
    let val ← uniformFloat 100
    if val < a.environment.policy.left then
      let tempAgent := tdUpdate a Move.left α γ
      runEpisode tempAgent α γ
    else do
      let tempAgent := tdUpdate a Move.right α γ
      runEpisode tempAgent α γ

def runEpisodes (numEpisodes : Nat) (initialAgent : Agent) (α γ : Float) : IO Agent := do
  if numEpisodes = 0 then
    return initialAgent
  else do
    let a : Agent := {currentPosition := 3, environment := initialAgent.environment}
    let updatedAgent ← runEpisode a α γ
    runEpisodes (numEpisodes - 1) updatedAgent α γ

def main : IO Unit := do
  let α : Float := 0.1
  let γ : Float := 1
  let initialAgent : Agent := {currentPosition := 3, environment := {states := 5}}
  let finalAgent ← runEpisodes 1000 initialAgent α γ
  IO.println "\n\nHere are the state values as \"Discovered\" by the TD(0) algorithm."
  printAgentStates finalAgent
  IO.println "\n\nDone!"
