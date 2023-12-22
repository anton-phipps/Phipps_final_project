# Implementing Reinforcment Learning Algorithms in Lean4


1. This project is centered around implementing Reinforcement Learning Algorithms in Lean4 as a method to demostrate an understanding of how to use Lean4 as a fully fledged programming language.

2. First the BellmanEquationsUniformPolicy.lean file will demonstrate how to update state values through recursive functions as a form of iteration. Also having functions which return state updated state values and rewards according to the environmental behaviour. This file displays its behaviour simply by visiting the BellmanEquationsUniformPolicy.lean and looking at the Lean Infoview.

Project.lean will perform state value estimates using TD(0), a temoral difference algorithm which learns from experience rather than a full knowledge of the environement and performing structured sweeps like in the Bellman Equation. For this example a 5 state random walk was used with a reward only given for exiting the right hand side of the  states. This involved keeping track of state values through multiple itterations of an episodic task. With a γ = 1, and equal likelyhood of going to the right or left, each state will have a value approximately starting at 1/6 for the furthest point, and ending in 5/6 for the closest point to the right. To run this project, please use "lean --run Project.lean"



3. Bellman Equation to $ v_{\pi} $ (p. 59).
$ v_{\pi}(s) \doteq \sum_{a} \pi (a|s) \sum_{s',r} p(s',r|s,a)[r + \gamma v_{\pi}(s')] $, for all $ s \in S $

TD(0) (p. 120)
$ V(S_t) \l V(S_t) + \alpha [R_{t+1} + \gamma V(S_{t+1}) - V(S_t)]  $



4.  The full text from which the algorithms are taken can be found at the following address:

http://www.incompleteideas.net/book/RLbook2020.pdf
Sutton, R. S., & Barto, A. G. (2018). Reinforcement Learning: An Introduction. MIT Press.

Future work in functional programming will be to setup environments which are unchanging, and passing the value functions, and update functions as paramterters to the functions.