# artificial neural network
repository for the implementation of an multilayered perceptron in different functional programming languages

### structure
all different implementations are conceptually identical, the oly changes are the syntaxis of each language
- there exists a list of pairs, of the form (x1,y1), (x2,y2) ... (xn,yn) representing the inputs of a neuron:
    - the first value of each pair, x represents the weight of the connection
    - the second the current value being transmitted
- since there is just a single neuron implemented, the learning algorithm is the delta rule
