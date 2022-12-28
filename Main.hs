module Main (main) where
import Normalize (normalize_data)
import System.Random
import Data.List


----- # TRAINING # -----

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
delta :: Float -> Float -> Float -> Float 
delta error old_weight input_value = 
    old_weight + (sigmoid_derivative error) * (error) * input_value


-- get next random integer
next_rng :: Int -> Int -> Int
next_rng limit seed = fst(randomR (0, limit) (mkStdGen seed))


-- iterate learn function over weights using different examples each time
update_weights :: [[Float]] -> [Float] -> (Float -> Float) -> Int -> [Float] -> [Float]
update_weights training_set expected_results act_function pos weights =
    let error = (expected_results !! pos) - (neuron act_function (training_set !! pos) (weights)) in
    zipWith (delta error) weights (training_set !! pos)


-- list of learning functions with each position 
train :: [[Float]] -> [Float] -> (Float -> Float) -> Int -> Int -> [[Float] -> [Float]]
train training_set expected_results act_function instances seed =
    let all_rng = iterate (next_rng instances) seed in
    map (update_weights training_set expected_results act_function) all_rng



----- # SELECTION # -----

-- test given weight
evaluate_weight :: [[Float]] -> [Float] -> Int -> Int-> [Float]
evaluate_weight training_set weight random_number instances =
    let approximation = (foldl (+) 0.0 (zipWith (*) weight (tail (training_set !! random_number))))
    in (head (training_set !! random_number) - approximation) :
        evaluate_weight training_set weight (next_rng instances random_number) instances


-- aux function
check_weight :: [[Float]] -> Int -> Int -> Int -> [Float] -> Float
check_weight  training_set random_number instances iterations weight =
    let list = (evaluate_weight training_set weight random_number instances)
    in abs(foldl (+) 0.0 (take iterations list))


-- check_weight (weight ...) !! max_value
select_optimal:: [[Float]] -> [[Float]] -> Int  -> Int -> Int -> Int -> (Float, [Float])
select_optimal training_set weights random_number instances iterations max_epoch =
    let weight_error = take max_epoch (map (check_weight training_set random_number instances iterations) weights)
        in minimum (zip weight_error weights)



----- # MAIN # -----

main :: IO (Float,[Float])
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
    let error_threshold = 0.001
    let max_epoch = 1000000
    let act_function = sigmoid

    -- get all train functions
    let train_functions = train training_set real_out act_function instances random_number

    -- apply train functions & print results
    let trained_weights = scanl' (\x f -> f x) weights train_functions

    -- get optimal value
    let checks_num = 14
    let optimal_weight = select_optimal trained_weights trained_weights random_number instances checks_num max_epoch
    --let optimal_weight = (0,(trained_weights !! max_epoch))

    -- print & return value
    print optimal_weight
    return optimal_weight
