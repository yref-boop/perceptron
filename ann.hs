µ = 1

-- step 
step x = 
    if x < 0
        then -1
    else 1

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
neuron action_function inputs bias =
    action_function ( bias + foldl (+) (map untuple inputs))

-- delta rule
delta_rule (weight, value) µ w d y x =
 (w + µ * (d - y) * x, value)

weight_correction inputs =
    map delta_rule inputs

