# perceptron 
perceptron implementation using haskell that aims to be a generalized perceptron for any given dataset
the idea is not for it to be competitive at all, but rather for me to test some functional programming concepts & learn more about haskell & ai

## technologies
this repo only uses haskell, trying to rely on external modules as little as possible
- ghc version 9.4.4 
- Data.Vector
- Data.List
- System.Random

## execution
compiling the Main.hs file `ghc Main.hs` & running it `./Main` should suffice
at the moment all performance options (epochs, error, validation...) are hard-coded on this file for my confort
(i may add them as inputs later)

## code structure
the code can be divided into two different parts:
- `Normalize.hs`: auxiliar functionality that parses & normalizes the dataset written into `data.tx`
- `Main.hs`: main functionality (training, validation & testing) that calculates an optimal weight approximation for the given data

## specifications
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

on the file *per se*, each number must be separated with a tab '\t' & newlines for each row

the actual data read will be a `[[a']]`, such that, for the previous example:
```data =[[a0,a1,a2,...an],[b0,b1,b2,...bn],[c0,c1,c2,...cn],...[m0,m1,m2,...mn]]```

- the size of the primary list is the number of instances
- the size of any of the sub-lists (-1) is the dimensionality of the problem
    - there is easy access to the expected result as `head` of each sub-list

this data is normalized automatically by trasposing the [[Float]] and dividing each value of each list by the maximum value on that list
this normalization is not perfect, it does not take into account boolean/categorizing values (which should be made into different properties) but for this project, it'll do


### current dataset example

the current [data](https://archive.ics.uci.edu/ml/datasets/Wine+Quality) has the following input variables:
- fixed acidity
- volatile acidity
- citric acid
- residual sugar
- chlorides
- free sulfur dioxide
- total sulfur dioxide
- density
- pH
- suphates
the objective is for the ann to be able to predict the quality (represented as an integer [1,10])

this problem, at first glance incites solving with a more complex ann, but given the results of external tests, a perceptron should suffice
