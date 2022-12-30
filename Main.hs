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



----- # WEIGHT SELECTION # -----

-- zip + fold
zip_fold :: Num c => (a -> b -> c) -> [a] -> [b] -> c
zip_fold f = aux 0
    where
        aux n [] _ = n
        aux n _ [] = n
        aux n (x:xs) (y:ys) = aux (f x y) xs ys


-- test given weight
evaluate_weight :: [[Float]] -> [Float] -> Int -> Int -> [Float]
evaluate_weight training_set weight random_number instances =
    let training_instance = (training_set !! random_number) in 
    let approximation = zip_fold (*) weight (tail training_instance) in 
        head training_instance - approximation :
        evaluate_weight training_set weight (next_rng instances random_number) instances


-- aux function
check_weight :: [[Float]] -> Int -> Int -> Int -> [Float] -> Float
check_weight  training_set random_number instances iterations weight =
    let list = evaluate_weight training_set weight random_number instances in
        abs (foldl (+) 0.0 (take iterations list)) / (fromIntegral iterations)


-- check_weight (weight ...) !! max_value
select_optimal:: [[Float]] -> [[Float]] -> Int  -> Int -> Int -> Int -> (Float, [Float])
select_optimal training_set weights random_number instances iterations max_epoch =
    let get_error = check_weight training_set random_number instances iterations in
    let weight_error = map get_error weights in
        minimum (zip (take max_epoch weight_error) weights)



----- # MAIN # -----

-- get random_set
random_subset :: [[Float]] -> Int -> Float -> [Int]
random_subset general_set random_number percentage =
    let instances = floor (fromIntegral(length general_set) * percentage) in
    let rng_list = iterate (next_rng (length general_set)) random_number in
        take instances (drop 1 rng_list)


-- get subsets from data
split_set :: [[Float]] -> Int -> Float -> ([[Float]], [[Float]])
split_set general_set random_number percentage =
    let random_set = map (general_set !!) (random_subset general_set random_number percentage)
    in (general_set \\ random_set, random_set)


-- main
main :: IO (Float,[Float])
main = do
    
    -- store normalized data from data.txt
    all_data <- Normalize.normalize_data
 
    -- auxiliar data gathered from input
    let dimension = length (tail (head all_data))

    -- init random numbers
    rng_seed <- newStdGen
    let (random_seed, seed) = randomR (0,100000000) (rng_seed)

    -- divide all_data
    let (training_data, auxiliar_set) = split_set all_data random_seed 0.1
    let (validation_set, test_set) = split_set auxiliar_set random_seed 0.5

    print (length all_data)
    print (length training_data)
    print (length validation_set)
    print (length test_set)

    -- new random numbers
    rng_seed <- newStdGen
    let random_number = next_rng (length training_data) random_seed

    -- training inputs calculations
    let training_set = fmap (1:) (fmap tail training_data)
    let int_weights = take (dimension + 1) (iterate (next_rng (length training_set)) random_number)
    let weights = map ((/1000).fromIntegral) int_weights
    let real_out = fmap (head) all_data

    -- hard-coded training values
    let error_threshold = 0.001
    let max_epoch = 1000000
    let act_function = sigmoid

    -- get all train functions
    let train_functions = train training_set real_out act_function (length training_set) random_number

    -- apply train functions & print results
    let trained_weights = scanl' (\x f -> f x) weights train_functions

    -- get optimal value
    let checks_num = 5
    let optimal_weight = select_optimal trained_weights trained_weights random_number (length training_set) checks_num max_epoch

    -- print & return value
    print optimal_weight
    return optimal_weight
