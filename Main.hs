module Main (main) where
import Normalize (normalize_data)
import System.Random 


-- sigmoid function
sigmoid :: Float -> Float
sigmoid x = 1.0 / ( 1 + exp (-x) )


-- sigmoid derivative
sigmoid_derivative :: Float -> Float
sigmoid_derivative x =
    sigmoid x * (1.0 - sigmoid x )


-- neuron function (weighted sum of inputs)
neuron :: (Float -> Float) -> [Float] -> [Float] -> Float
neuron activation_function inputs weight=
    activation_function (foldl (+) 0.0 (zipWith (*) inputs weight))


-- delta rule 
delta :: Float -> Float -> Float -> Float -> Float 
delta expected_result actual_result old_weight input_value = 
    let error = (expected_result - actual_result) in
    old_weight + (sigmoid_derivative error) * (error) * input_value


-- get next random integer
next_rng :: Int -> Int -> Int
next_rng limit seed = fst(randomR (0, limit) (mkStdGen seed))


-- iterate learn function over weights using different examples each time
update_weights :: [[Float]] -> [Float] -> (Float -> Float) -> Int -> [Float] -> [Float]
update_weights training_set expected_results act_function pos weights =
    let actual_result = (neuron act_function (training_set !! pos) (weights)) in
    zipWith (delta (expected_results !! pos) actual_result) weights (training_set !! pos)


-- list of learning functions with each position 
train :: [[Float]] -> [Float] -> (Float -> Float) -> Int -> Int -> [[Float] -> [Float]]
train training_set expected_results act_function instances seed =
    let all_rng = iterate (next_rng instances) seed in
    map (update_weights training_set expected_results act_function) all_rng


-- apply recursively functions from a list and give output as input of the next
-- idea: fn(...f4(f3(f2(f1(a))))...)
zip_iterate :: [[a] -> [a]] -> [a] -> [[a]]
zip_iterate functions initial_value =
    zip_iterate_aux functions initial_value 0
    where
        zip_iterate_aux :: [[a] -> [a]] -> [a] -> Int -> [[a]]
        zip_iterate_aux func_list input count =   
            input : (zip_iterate_aux func_list ((func_list !! count)input) (count + 1))


-- gives real result & estimated on a random example
check_weights :: [Float] -> [[Float]] -> Int -> Int -> [Float]
check_weights weights training_set random instances = 
    let 
    approximation = head (training_set !! random) - 
        (foldl (+) 0.0 (zipWith (*) weights (tail (training_set !! random))))

    in approximation : check_weights weights training_set (next_rng instances random) instances



-- main
main :: IO ([Float])
main = do
    
    -- store normalized data from data.txt
    all_data <- Normalize.normalize_data
 
    -- auxiliar data gathered from input
    let instances = length all_data
    let dimension = length (tail (head all_data))

    -- init random numbers
    rng_seed <- newStdGen
    let (random_number, seed) = randomR (0,instances) (rng_seed)

    -- training inputs calculations
    let training_set = fmap (1:) (fmap tail all_data)
    let int_weights = take (dimension + 1) (iterate (next_rng instances) random_number)
    let weights = map ((/1000).fromInteger.toInteger) int_weights
    let real_out = fmap (head) all_data

    -- hard-coded training values
    let error_threshold = 0.0000001
    let max_epoch = 100000
    let act_function = sigmoid

    -- get all train functions
    let train_functions = train training_set real_out act_function instances random_number

    -- apply train functions & print results
    let final_weight = (zip_iterate train_functions weights) !! max_epoch
    print final_weight

    -- check discrepancy with a random example
    let checks_num = 100
    let error_list = take checks_num (check_weights final_weight all_data random_number instances) 
    let error_mean = (foldl (+) 0 error_list) / fromInteger(toInteger(checks_num))
    print error_mean

    -- return value
    return final_weight
