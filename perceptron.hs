-- auxiliar absolute value
absolute_float x =
    if (x < 0.0) then -1.0 * x else x

-- relu
relu x = max(0,x)

-- sigmoid
sigmoid x = 1.0/(1+exp(-x))

-- sigmoid derivative
sigmoid_derivative x =
    sigmoid x * (1.0 - sigmoid x )

-- auxiliar for inputs
untuple (weight, value) = weight * value

-- neuron function
neuron activation_function inputs bias =
    activation_function ( bias + (foldl (+) 0.0 (map untuple inputs)))

-- delta rule
delta_rule n error x (weight, value) =
    (weight + n * error * x, value)

-- weight modification
learn inputs error =
    map (delta_rule 0.1 error 0.1) inputs

-- training function
train inputs expected_result error_threshold =
    let error = expected_result - (neuron sigmoid inputs 1.0) in
    if (absolute_float error > error_threshold)
        then train (learn inputs error) expected_result error_threshold
    else inputs


-- main
main :: IO ()
main = return ()
