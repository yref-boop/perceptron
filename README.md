# perceptron 
### structure
all different implementations are conceptually identical:
- a list of pairs, of the form $(w_0,v_1),(w_1,v_1), (w_2,v_2) \ldots (w_n,v_n)$ represents the inputs of a neuron:
    - the first value of each pair, $w$ represents the weight of the connection
    - the second, $v$, the current value
- the current learning algorithm is the delta rule: 

the code should be able to approximate an answer to any linearly separable problem

### input
data is written on the data.txt file such that:

```math
\begin{matrix}
\begin{array}{c|cccc}
a_0&a_1&a_2&\ldots&a_n \\b_0&b_1&b_2&\ldots&b_n\\c_0&c_1&c_2&\ldots&c_n\\\vdots&\vdots&\vdots&\ddots&\vdots\\m_0&m_1&m_2&\ldots&m_n
\end{array}
\end{matrix}
$$
```

- the number of rows $m$ is the number of instances available for the problem
- the number of columns $n$ signifies the dimensionality of the problem
    - the first column $n=0$ represents the expected result for the corresponding data

on the file *per se*, each number must be separated with an space & newlines for each row

the actual data read will be a `[[a']]`, such that, for the previous example:
```data =[[a0,a1,a2,...an],[b0,b1,b2,...bn],[c0,c1,c2,...cn],...[m0,m1,m2,...mn]]```

- the size of the primary list is the number of instances
- the size of any of the sub-lists (-1) is the dimensionality of the problem
    - there is easy access to the expected result as `head` of each sub-list
