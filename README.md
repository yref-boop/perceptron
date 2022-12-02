# artificial neural network
repository for the implementation of a perceptron in different functional programming languages

### structure
all different implementations are conceptually identical:
- there exists a list of pairs, of the form (x1,y1), (x2,y2) ... (xn,yn) representing the inputs of a neuron:
    - the first value of each pair, x represents the weight of the connection
    - the second the current value
- since there is just a single neuron implemented, the learning algorithm is the delta rule

in its current state the code should be able to approximate an answer to any linearly sepparable problem

### input
data is written on the data.txt file, with the following structure

```math
\begin{pmatrix}a_1&b_1&c_1&\ldots&n_1 \\a_2&b_2&c_2&\ldots&n_2\\a_3&b_3&c_3&\ldots&n_3\\\vdots&\vdots&\vdots&\ddots&\vdots\\a_m&b_m&c_m&\ldots&n_m
\end{pmatrix}
```
where columns signify dimension & rows specific elements 
