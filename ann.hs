-- relu
relu x = max(0,x)

-- sigmoid
sigmoid x =
    1.0/(1+exp(-x))

-- sigmoid derivative
sigmoid_derivative x =
    sigmoid(x) * (1.0 - sigmoid(x))

-- auxiliar for inputs
untuple (weight, value) =
    weight*value

-- neuron function
neuron action_function inputs =
    action_function (foldl (+) (map untuple inputs))


