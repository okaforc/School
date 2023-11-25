Backpropagation (BP) is one of the most popular algorithms for ANNs.

There are two basic definitions for BP:
1. Backpropagation is a procedure for efficiently calculating the derivatives of some output quantity of a non-linear system, with respect to all inputs and parameters of that system, through calculations proceeding *backwards* from outputs to inputs.
2. Backpropagation is any technique for adapting the weights or parameters of a nonlinear system by somehow using such derivatives or the equivalent.

Paul Werbos provided a rule for updating the weights of a multi-layered network undergoing "supervised learning". It is the weight adaptation rule, which is called backpropagation.

Typically, a fully connected feedforward network is used to be trained using the BP algorithm; activation in such networks travels in a direction from the input to the output layer and the units in one layer are connected to every other unit in the next layer.

"We create mathematical objects that provide us the design output of hidden neurons."

There are two sweeps of the fully connected network: *forward sweeps* and *backward sweeps*.

***Forward Sweep:*** This sweep is similar to any other feedforward ANN -- the input stimuli is given to the network, the network computes the weighted average from all the input units, and then passes the average through a "squash" (flatten) function. The ANN may have a number of *hidden layers*, for example, the multi-net perceptron, and the output from each hidden layer becomes the input to the next layer forward.
### 
***Backwards Sweep:*** This sweep is similar to the forward sweep, except that what is swept are the error values. These values essentially are the differences between the actual output and a desired output: $$e_{j}=d_{j}-o_{j}$$
In the backward sweep, the output unit sends errors back to the first proximate hidden layer which in turn passes it onto the next. No error signal is sent to the input units.

###
---
The backpropagation algorithm is used to train a multilayer perceptron by using a set of training examples that are presented to the perceptron $n$ times. The training examples are usually a pair: an input value $j$ and the expected/desired value $i$.

It is expected that after $n$ iterations, the perceptron will learn the relationship between all or many of the input-output value pairs.

### Backpropagation Algorithm
The following is a multi-layer perceptron (MLP). We intend to find out how to calculate the weight of all the neurons, where *one neuron* is input-output pair (in this case, these can be input-hidden or hidden-output)
(j input, i output)