import Project.Basic

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
deriving Repr

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


def myAgent35 : Agent := {row := 3, column := 2, states := {nRows := 5, nColumns := 5}}
def myAgent41 : Agent := {row := 0, column := 1, states := {nRows := 4, nColumns := 4}}

/-
  My absolulte value function
-/
def abs (x : Float) : Float :=
  if x < 0 then -x else x

/-
  The function to find the maximum change between the state values to setup
  a stopping condition.
-/

partial def maxChange (agent1 : Agent) (agent2 : Agent) : Float :=
  let range : Nat := agent1.states.nRows * agent1.states.nColumns
  let rec loop (i : Nat) (maxDiff : Float) : Float :=
    if i >= range then maxDiff
    else
      let v1 : Float := agent1.states.stateValues[i]!
      let v2 : Float := agent2.states.stateValues[i]!
      let diff : Float := abs (v1 - v2)
      let newMaxDiff : Float := if diff > maxDiff then diff else maxDiff
      loop (i + 1) newMaxDiff
  loop 0 0

/-
Reccursive functions to update the state values, because this is
functional programming variables can not be reassigned and thus
recursion is the way it must be solved
-/

partial def updateAgent35 (i : Nat) (agent : Agent) (γ : Float) : Agent :=
  let range : Nat := agent.states.nRows * agent.states.nColumns
  if i >= range then agent
  else
    let row := i / agent.states.nColumns
    let column := i % agent.states.nRows
    let tempAgent : Agent := {row := row, column := column, states := agent.states}

    let reward_agent := gridWorld35 tempAgent Move.up
    let reward := reward_agent.1 -- The reward
    let π_up : Float := tempAgent.states.policy[i]!.up -- the probability of going up
    let v_up : Float := tempAgent.states.stateValues[agentPosition reward_agent.2]!
    let Vs_up : Float := π_up * (reward + γ * v_up)

    let reward_agent := gridWorld35 tempAgent Move.down
    let reward := reward_agent.1 -- The reward
    let π_down : Float := tempAgent.states.policy[i]!.down -- the probability of going down
    let v_down : Float := tempAgent.states.stateValues[agentPosition reward_agent.2]!
    let Vs_down : Float := π_down * (reward + γ * v_down)

    let reward_agent := gridWorld35 tempAgent Move.left
    let reward := reward_agent.1 -- The reward
    let π_left : Float := tempAgent.states.policy[i]!.left -- the probability of going left
    let v_left : Float := tempAgent.states.stateValues[agentPosition reward_agent.2]!
    let Vs_left : Float := π_left * (reward + γ * v_left)

    let reward_agent := gridWorld35 tempAgent Move.right
    let reward := reward_agent.1 -- The reward
    let π_right : Float := tempAgent.states.policy[i]!.right -- the probability of going right
    let v_right : Float := tempAgent.states.stateValues[agentPosition reward_agent.2]!
    let Vs_right : Float := π_right * (reward + γ * v_right)

    let Vs : Float := Vs_up + Vs_down + Vs_left + Vs_right
    let position : Nat := agentPosition tempAgent
    let newArr := agent.states.stateValues.set! position Vs
    let newAgent : Agent :=
      {
        row := agent.row,
        column := agent.column,
        states :=
        {
          nRows := agent.states.nRows,
          nColumns := agent.states.nColumns,
          policy := agent.states.policy,
          stateValues := newArr
        }
      }
    updateAgent35 (i + 1) newAgent γ

/-
A function to call which handles the max number of itterations needed.
-/

partial def dynamicProgramming35 (a : Agent) (γ : Float) (maxDelta : Float) (maxIterations : Nat) : Agent :=
  let rec iterate (iteration : Nat) (agent : Agent) : Agent :=
    if iteration >= maxIterations then agent
    else
      let newAgent := updateAgent35 0 agent γ
      if maxChange newAgent agent < maxDelta then newAgent
      else iterate (iteration + 1) newAgent
  iterate 0 a

partial def updateAgent41 (i : Nat) (agent : Agent) (γ : Float) : Agent :=
  let range : Nat := agent.states.nRows * agent.states.nColumns
  if i >= range then agent
  else
    let row := i / agent.states.nColumns
    let column := i % agent.states.nRows
    let tempAgent : Agent := {row := row, column := column, states := agent.states}

    let reward_agent := gridWorld41 tempAgent Move.up
    let reward := reward_agent.1 -- The reward
    let π_up : Float := tempAgent.states.policy[i]!.up -- the probability of going up
    let v_up : Float := tempAgent.states.stateValues[agentPosition reward_agent.2]!
    let Vs_up : Float := π_up * (reward + γ * v_up)

    let reward_agent := gridWorld41 tempAgent Move.down
    let reward := reward_agent.1 -- The reward
    let π_down : Float := tempAgent.states.policy[i]!.down -- the probability of going down
    let v_down : Float := tempAgent.states.stateValues[agentPosition reward_agent.2]!
    let Vs_down : Float := π_down * (reward + γ * v_down)

    let reward_agent := gridWorld41 tempAgent Move.left
    let reward := reward_agent.1 -- The reward
    let π_left : Float := tempAgent.states.policy[i]!.left -- the probability of going left
    let v_left : Float := tempAgent.states.stateValues[agentPosition reward_agent.2]!
    let Vs_left : Float := π_left * (reward + γ * v_left)

    let reward_agent := gridWorld41 tempAgent Move.right
    let reward := reward_agent.1 -- The reward
    let π_right : Float := tempAgent.states.policy[i]!.right -- the probability of going right
    let v_right : Float := tempAgent.states.stateValues[agentPosition reward_agent.2]!
    let Vs_right : Float := π_right * (reward + γ * v_right)

    let Vs : Float := Vs_up + Vs_down + Vs_left + Vs_right
    let position : Nat := agentPosition tempAgent
    let newArr := agent.states.stateValues.set! position Vs
    let newAgent : Agent :=
      {
        row := agent.row,
        column := agent.column,
        states :=
        {
          nRows := agent.states.nRows,
          nColumns := agent.states.nColumns,
          policy := agent.states.policy,
          stateValues := newArr
        }
      }
    updateAgent41 (i + 1) newAgent γ

partial def dynamicProgramming41 (a : Agent) (γ : Float) (maxDelta : Float) (maxIterations : Nat) : Agent :=
  let rec iterate (iteration : Nat) (agent : Agent) : Agent :=
    if iteration >= maxIterations then agent
    else
      let newAgent := updateAgent41 0 agent γ
      if maxChange newAgent agent < maxDelta then newAgent
      else iterate (iteration + 1) newAgent
  iterate 0 a

-- This shows (and in the info view displays) the values of each state under the uniform policy

#eval printAgentStates (dynamicProgramming35 myAgent35 0.9 0.01 1000)
#eval printAgentStates (dynamicProgramming41 myAgent41 0.9 0.00001 1000)
