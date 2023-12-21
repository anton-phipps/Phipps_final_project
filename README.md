# Implementing Reinforcment Learning Algorithms in Lean4


1. This project is centered around implementing Reinforcement Learning Algorithms in Lean4 as a method to demostrate an understanding of how to use Lean4 as a fully fledged programming language.

2. First the BellmanEquationsUniformPolicy.lean file will demonstrate how to update state values through recursive functions as a form of iteration. Also having functions which return state updated state values and rewards according to the environmental behaviour.

Project.lean will perform policy updates using TD(0), a temoral difference algorithm with policy updates to try and discover the optimal policy for the same two environments.

3. Bellman Equation to $ v_{\pi} $.
$ v_{\pi}(s) \doteq \sum_{a} \pi (a|s) \sum_{s',r} p(s',r|s,a)[r + \gamma v_{\pi}(s')] $, for all $ s \in S $

TD(0) (p. 120)
$ V(S_t) \l V(S_t) + \alpha [R_{t+1} + \gamma V(S_{t+1}) - V(S_t)]  $



4.  The full text from which the algorithms are taken can be found at the following address:

http://www.incompleteideas.net/book/RLbook2020.pdf
Sutton, R. S., & Barto, A. G. (2018). Reinforcement Learning: An Introduction. MIT Press.