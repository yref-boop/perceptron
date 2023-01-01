module Main (main) where
import Normalize (normalize_data)
import System.Random
import Data.List as List
import Data.Vector as V



----- # TRAINING # -----

-- sigmoid function
sigmoid :: Float -> Float
sigmoid x = 1.0 / ( 1 + exp (-x) )


-- sigmoid derivative
sigmoid_derivative :: Float -> Float
sigmoid_derivative x =
    sigmoid x * (1.0 - sigmoid x )


-- neuron function (weighted sum of inputs)
neuron :: (Float -> Float) -> Vector Float -> Vector Float -> Float
neuron activation_function inputs weight=
    activation_function (V.foldl (+) 0.0 (V.zipWith (*) inputs weight))


-- delta rule 
delta :: Float -> Float -> Float -> Float 
delta error old_weight input_value = 
    old_weight + (sigmoid_derivative error) * (error) * input_value


-- get next random integer
next_rng :: Int -> Int -> Int
next_rng limit seed = fst(randomR (0, limit) (mkStdGen seed))


-- iterate learn function over weights using different examples each time
update_weights :: Vector (Vector Float) -> Vector Float -> (Float -> Float) -> Int -> Vector Float -> Vector Float
update_weights training_set expected_results act_function pos weights =
    let error = (expected_results ! pos) - (neuron act_function (training_set ! pos) (weights)) in
    V.zipWith (delta error) weights (training_set ! pos)


-- list of learning functions with each position 
train :: Vector (Vector Float) -> Vector Float -> (Float -> Float) -> Int -> [(Vector Float -> Vector Float)]
train training_set expected_results act_function seed =
    let all_rng = iterate (next_rng (V.length training_set)) seed in
    List.map (update_weights training_set expected_results act_function) all_rng



----- # VALIDATION # -----

get_error :: Vector (Vector Float) -> Vector Float -> Int -> Float
get_error training_set weight pos =
    let
        results = (V.zipWith (*) weight (V.tail (training_set ! pos)))
        approximation = (V.foldl (+) 0.0 results) / fromIntegral(V.length results)
        real = V.head (training_set ! pos)
    in 
    abs (approximation - real)    


evaluate_weight :: Vector (Vector Float) -> Int -> Int -> Vector Float -> Float
evaluate_weight training_set random_number iterations weight =
    let 
        random_numbers = randomRs (0,V.length training_set) (mkStdGen random_number)
        pos_list = List.take iterations . nub $ (random_numbers)
    in
        (V.foldl (+) 0.0 (V.map (get_error training_set weight) (fromList pos_list)))


-- get weight with better generalization on validation_set
select_optimal :: Vector (Vector Float) -> [Vector Float] -> Int -> Int -> Float -> Int -> Vector Float
select_optimal validation_set weights max_epoch iterations error_threshold random_number =
    let 
        get_error = evaluate_weight validation_set random_number iterations
        weight_error = List.map get_error weights 
    in
    snd (List.minimum (List.take max_epoch (List.zip weight_error weights)))


----- # MAIN # -----


-- get positions  
select_split :: Vector (Vector Float) -> Int -> Float -> [Int]
select_split general_vector random_number percentage = 
    let instances = floor (fromIntegral (V.length general_vector) * percentage) in
    List.take instances . nub $ (randomRs (0,V.length general_vector) (mkStdGen random_number))


-- get remainder
delete_split :: Vector (Vector Float) -> [Int] -> Vector (Vector Float)
delete_split general_vector old_split =
    let 
        count_list = List.take (V.length general_vector) (List.iterate (+1) 0) 
        split_pos = fromList (count_list \\ old_split)
    in
    V.map (general_vector !) split_pos 
    

-- split a vector into two
split_data :: Vector (Vector Float) -> Int -> Float -> (Vector (Vector Float), Vector (Vector Float))
split_data general_vector random_number percentage =
    let 
        split_pos = (select_split general_vector random_number percentage)
        split_vector = (V.map (general_vector !) (fromList split_pos))
        remaining_vector = (delete_split general_vector split_pos)
    in 
    (split_vector, remaining_vector)


-- main
main :: IO (Vector Float)
main = do
    
    -- store normalized data from data.txt
    all_data <- Normalize.normalize_data
    let data_vector = fromList (fmap fromList all_data)
 
    -- auxiliar data gathered from input
    let dimension = V.length (V.tail (V.head data_vector))

    -- get training data
    rng_seed <- newStdGen
    let (random_seed, seed) = randomR (0, V.length data_vector) (rng_seed)
    let (training_data, aux_vector) = split_data data_vector random_seed 0.9

    -- get validation & test sets
    rng_seed <- newStdGen
    let random_number = next_rng (List.length training_data) random_seed
    let (validation_set, test_set) = split_data aux_vector random_seed 0.5

    -- training inputs calculations
    let training_vector = fmap (flip update (V.singleton (0,1.0))) training_data
    let int_weights = iterateN (dimension + 1) (next_rng (V.length training_vector)) random_number
    let weights = fmap ((/1000).fromIntegral) int_weights
    let real_out = fmap (V.head) data_vector

    -- hard-coded training values
    let error_threshold = 0.001
    let max_epoch = 1000000
    let act_function = sigmoid

    -- get & apply all train functions
    let train_functions = train training_vector real_out act_function random_number
    let trained_weights = List.scanl' (\x f -> f x) weights train_functions
   
    -- validation phase
    rng_seed <- newStdGen
    let random_test = next_rng (V.length validation_set) random_seed
    let checks_num = 10
    let optimal_weight = select_optimal validation_set trained_weights max_epoch checks_num error_threshold random_test

    -- print & return value
    print optimal_weight
    return optimal_weight
