## Definitions
### Policy
#### Regular
A **policy** is a solution that specifies what an agent should do for any state the agent might reach.
Typically denoted using $\pi$
$\pi(s)$ is the action recommended by the policy $\pi$ for state $s$.

If the agent has a complete policy, then no matter what the outcome of any action, the agent will always know what to do next.
Number of policies: num. actions ^ num. states
(e.g. 3 states, 2 actions, num. policies = $2^3=8$)

#### Optimal
An **optimal policy** is a policy that yields the highest expected utlity by maximising the reward function. There can be more than 1 optimal policy, but only one optimal policy function.
Denoted $\pi^{*}$.
Also called **$\gamma$-optimal policy**, calculated by $$\pi(s) := argmax \ q_{n+1}(s, a)$$
Given this, the agent decides what to do by consulting its current percept, which tells it the current state s, and then executing the action π∗(s). A policy represents the agent function explicitly and is therefore a description of a simple reflex agent, computed from the information used for a utility-based agent.

### Markov Decision Process
An MDP is an algorithm that decides which action $a \in A$ to take when in a particular state $s \in S$, with probability function $p$ that calculates the probability of an action being taken. This function makes use of the reward function $r$ and the $\gamma$ discount value.
Denoted as following:
An MDP is a 5-tuple $\langle S, A, p, r, \gamma \rangle$ consisting of
1. Finite set S of states s, s', ...
2. Finite set A of actions a, a, ....
3. Function p: $S\times A \times S \rightarrow$ [0,1]
	1. $p(s,a,s')$ = prob($s'|s,a$) = how probable is s' after doing a at s
$$\Sigma_{s'}p(s,a,s')=1 \ for \ all \ a \in A, \ s \in S$$
4. Function r:  $S\times A \times S \rightarrow \mathbb{R}$
	1. $r(s,a,s')$ = immediate reward at $s'$ after $a$ is done at $s$
5. Discount factor $\gamma \in$ [0,1]

Missing is the policy $\pi: S \rightarrow A$ (what to do at s)

### Exploration-Exploitation Tradeoff
**Exploration**: learning from experiencing a new environment. Has the goal of finding a path.
**Exploitation**: learning from using current experience. Has the goal of maximising the reward.
**Tradeoff**: the exploration-exploitation tradeoff is the level of priority one must give towards learning from exploring new environments and learning by exploiting preexisting knowledge of previous environments. 
### Learning Rate
The [learning rate](https://en.wikipedia.org/wiki/Learning_rate "Learning rate") or _step size_ determines to what extent newly acquired information overrides old information. A factor of 0 makes the agent learn nothing (exclusively exploiting prior knowledge), while a factor of 1 makes the agent consider only the most recent information (ignoring prior knowledge to explore possibilities). In fully [deterministic](https://en.wikipedia.org/wiki/Deterministic_system "Deterministic system") environments, a learning rate of $\alpha_{t} = 1$ is optimal. This value directly affects the [[#Exploration-Exploitation Tradeoff]].


### Determinism
A **deterministic system** is one that contains no randomness in the development of future states of the system.
A **non-deterministic system** is one that *does* contains randomness.
### Discount Factor ($\gamma$)
The discount factor $\gamma$ is a value between 0 and 1 that describes the preference of an agent for current rewards over future rewards.
When $\gamma$ is close to 0, rewards in the distant future are viewed as insignificant. When $\gamma$ is 1, discounted rewards are exactly equivalent to additive rewards, so additive rewards are a special case of discounted rewards.

This is **NOT** to be confused with [[#Learning Rate]], which describes how much of the current environment to use towards learning.
## Calculations
### Value iteration and $\gamma$-discounted values

![[Pasted image 20230429124406.png | 500 | 500]]
where $a'$ is the table with the largest reward (max) for that $(s, a, s')$ tuple.
#### Non-deterministic
##### (1)
###### $$q_0(s,a):=\Sigma_{s'}p(s,a,s')r(s,a,s')$$
##### (2)
###### $$q_{n+1}(s,a):=\Sigma_{s'}p(s,a,s')(r(s,a,s')+\gamma \ max_{a'} q_n(s',a'))$$
#### Deterministic
##### (3)
###### $$q_0(s,a):=r(s,a,s')$$
##### (4)
###### $$q_{n+1}(s,a):=r(s,a,s')+\gamma \ max_{a'} q_n(s',a')$$

#### Example 1 (2019 Q2 (b))
![[Pasted image 20230428204824.png | 300 | 400]]
Using formulae [[#(1)]] and [[#(2)]] in the above section,
1. $q_0(s,a):=\Sigma_{s'}p(s,a,s')r(s,a,s')$
	1. 
	$q_{0}(s_{1}, a_{1}) = r(s_{1}, a_{1}, s_{1})p(s_{1}, a_{1}, s_{1}) + r(s_{1}, a_{1}, s_{2})p(s_{1}, a_{1}, s_{2})$
	$= (0.7)(3) + (0.3)(0)$
	$= 2.1$
	2. 
	$q_{0}(s_{1}, a_{2}) = r(s_{1}, a_{2}, s_{1})p(s_{1}, a_{2}, s_{1}) + r(s_{1}, a_{2}, s_{2})p(s_{1}, a_{2}, s_{2})$
	$= (0.2)(4) + (0.8)(2)$
	$= 2.4$
	3. 
	$q_{0}(s_{2}, a_{1}) = r(s_{2}, a_{1}, s_{1})p(s_{2}, a_{1}, s_{1}) + r(s_{2}, a_{1}, s_{2})p(s_{2}, a_{1}, s_{2})$
	$= (0.3)(0) + (0.7)(1)$
	$= 0.7$
	4. 
	$q_{0}(s_{2}, a_{2}) = r(s_{2}, a_{2}, s_{1})p(s_{2}, a_{2}, s_{1}) + r(s_{2}, a_{2}, s_{2})p(s_{2}, a_{2}, s_{2})$
	$= (0.1)(1) + (0.9)(6)$
	$= 5.5$

2. $q_{n+1}(s,a):=\Sigma_{s'}p(s,a,s')(r(s,a,s')+\gamma \ max_{a'} q_n(s',a'))$
	1. 
	$q_{1}(s,a):=p(s_{1},a_{1},s_{1})\left(r(s_{1},a_{1},s_{1})+\frac{1}{2} \ max_{a'} q_0(s_{1},a')\right)+ p(s_{1},a_{1},s_{2})(r(s_{1},a_{1},s_{2})+\frac{1}{2} \ max_{a'} q_0(s_{2},a'))$
	$= (0.7)\left(3+\frac{1}{2}(2.4)\right)+ (0.3)(0+\frac{1}{2}(5.5))$
##### $= 3.765$


#### Example 2 (2019 Q2 (c))

![[Pasted image 20230428210842.png | 500 | 500]]
Using formula [[#(3)]] and [[#(4)]] in the above section:
1. 
##### $= q(s,a):=r(s,a,next(s, a))+\gamma \ max_{a'} q_n(next(s, a),a')$



#### Example 3 (2018 Q2 (b))
![[Pasted image 20230429015324.png | 300 | 400]]
$q_{0}(s_{1}, a_{1}) = p(s_{1}, a_{1}, s_{1})r(s_{1}, a_{1}, s_{1}) + p(s_{1}, a_{1}, s_{2})r(s_{1}, a_{1}, s_{2}) + p(s_{1}, a_{1}, s_{3})r(s_{1}, a_{1}, s_{3})$
$= (.5)(3)+(.3)(0)+(.2)(-2)$
$= 1.1$
(only need to find where first $s_{n}$ matches, so only $s_{3}$)
$q_{0}(s_{3}, a_{1}) = 1$
$q_{0}(s_{3}, a_{2}) = 0$
(probability of one of the states at $s_{3}$ is 1 (deterministic), so can use simplified version at [[#4]])
$q_{1}(s_{3}, a_{1}) = r(s_{3}, a_{1}, s')+0.1max_{a'}(q_{0}(s_{3}, a'))$
$= 1+0.1(1)$
$=1.1$
$q_{1}(s_{3}, a_{2}) = r(s_{3}, a_{2}, s')+0.1max_{a'}(q_{0}(s_{3}, a'))$
$= 0+0.1(1)$
$=0.1$

$q_{2}(s_{3}, a_{2}) = r(s_{3}, a_{2}, s')+0.1max_{a'}(q_{1}(s_{3}, a'))$
$= 0+0.1(1.1)$
##### $=0.11$


