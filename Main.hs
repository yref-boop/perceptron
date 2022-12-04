module Main (main) where
import Reader (read_data)

-- auxiliar absolute value
absolute_float x =
    if (x < 0.0) then -1.0 * x else x

-- relu
relu x = max(0,x)

-- sigmoid
sigmoid :: Float -> Float
sigmoid x = 1.0/(1+exp(-x))

-- sigmoid derivative
sigmoid_derivative x =
    sigmoid x * (1.0 - sigmoid x )

-- auxiliar for inputs
untuple (weight, value) = weight * value

-- neuron function
neuron :: (Float -> Float) -> [(Float,Float)] -> Float -> Float
neuron activation_function inputs bias =
    activation_function ( bias + (foldl (+) 0.0 (map untuple inputs)))

-- delta rule
delta_rule :: Float -> Float -> Float -> (Float, Float) -> (Float, Float)
delta_rule n error x (weight, value) =
    (weight + n * error * x, value)

-- weight modification
learn :: [(Float,Float)] -> Float -> [(Float,Float)]
learn inputs error =
    fmap (delta_rule 0.1 error 0.1) inputs

-- training function
--train_iteration inputs expected_result error_threshold =
--    let error = expected_result - (neuron sigmoid inputs 1.0) in
--    if (absolute_float error > error_threshold)
--        then train (learn inputs error) expected_result error_threshold
--    else inputs

dataAt :: Int -> [a] -> a
dataAt _ [] = error "Empty List!"
dataAt y (x:xs)  | y <= 0 = x
                 | otherwise = dataAt (y-1) xs

-- check how bias worked
train :: [[(Float,Float)]] -> [Float] -> Float -> Float -> Int -> Int -> (Float -> Float) -> Int -> [[Float]]

train inputs expected_results error error_threshold epoch max_epoch act_function element =
    
    if (epoch > max_epoch || error < error_threshold)
        then return (fmap (fst) (dataAt element inputs))

    else train (learn (dataAt element inputs) ((dataAt element expected_results) - (neuron sigmoid (dataAt element inputs) 1.0))) expected_results ((dataAt element expected_results) - (neuron sigmoid (dataAt element inputs) 1.0)) error_threshold (epoch + 1) max_epoch act_function 0


-- add (weight, value)
format_input :: Float -> (Float, Float)
format_input value = (0, value)

-- main
main :: IO ()
main = do
    
    all_data <- Reader.read_data
 
    let instances = length all_data
    let dimension = length (tail (head all_data))
    let inputs = fmap (tail) all_data
    let formatted_inputs = fmap (fmap (format_input)) inputs
    let real_out = fmap (head) all_data

    let error_threshold = 1.0
    let max_epoch = 1000
    let act_function = sigmoid

    let final_inputs = train formatted_inputs real_out 1000 error_threshold 0 max_epoch act_function 0
    return ()
