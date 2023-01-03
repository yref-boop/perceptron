module Main (main) where
import Normalize (normalize_data)
import System.Random
import Data.List as L
import Data.Vector as V



----- # TRAINING # -----


-- sigmoid function
sigmoid :: Float -> Float
sigmoid x = 1.0 / (1 + exp (-x))


-- sigmoid derivative
sigmoid_derivative :: Float -> Float
sigmoid_derivative x =
    sigmoid x * (1.0 - sigmoid x )


-- neuron function (weighted sum of inputs)
neuron :: (Float -> Float) -> Vector Float -> Vector Float -> Float
neuron activation_function inputs weight =
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
    let 
        estimation = (neuron act_function (training_set ! pos) (weights))
        error = (expected_results ! pos) - estimation
    in
        V.zipWith (delta error) weights (training_set ! pos)


-- list of learning functions with each position 
train :: Vector (Vector Float) -> Vector Float -> (Float -> Float) -> Int -> Int -> Vector (Vector Float -> Vector Float)
train training_set expected_results act_function seed max_epoch=
    let all_rng = iterateN max_epoch (next_rng (V.length training_set)) seed in
    V.map (update_weights training_set expected_results act_function) all_rng



----- # VALIDATION # -----


-- validate one weight against one example
get_error :: Vector (Vector Float) -> Vector Float -> Int -> Float
get_error training_set weight pos =
    let
        results = V.zipWith (*) weight (V.tail (training_set ! pos))
        approximation = V.foldl (+) 0.0 results / fromIntegral (V.length results)
        real = V.head (training_set ! pos)
    in 
        abs (real - approximation)    


-- for a weight, iterate validation over a random set of examples
evaluate_weight :: Vector (Vector Float) -> Int -> Int -> Vector Float -> Float
evaluate_weight training_set random_number iterations weight =
    let 
        size = (V.length training_set - 1)
        random_numbers = randomRs (0, size) (mkStdGen random_number)
        pos_list = L.take iterations . nub $ random_numbers
        error_vector = V.map (get_error training_set weight) (fromList pos_list)
    in
        V.foldl (+) 0.0 (error_vector) / fromIntegral (V.length error_vector)


-- auxilar to get_optimal
optimize :: Vector (Float, Vector Float) -> (Float, Vector Float) -> Float -> Int -> (Int, Vector Float)
optimize error_weight minimum error_threshold count =
    case (V.length error_weight) of
    0 -> (count, snd minimum)
    _ ->
        let
            head = (V.head error_weight)
            tail = (V.tail error_weight)
        in
            if (fst head) < error_threshold then (count, snd head)
            else optimize tail (min head minimum) error_threshold (count + 1)


-- get optimal (error, weight) from the whole list
select_optimal :: Vector (Float, Vector Float) -> Float -> (Int, Vector Float)
select_optimal error_weight error_threshold =
    optimize error_weight (1/0, V.singleton 0) error_threshold 0
            

-- get weight with better generalization on validation_set
validate_results :: Vector (Vector Float) -> Vector (Vector Float) -> Int -> Int -> Float -> Int -> (Int, Vector Float)
validate_results validation_set weights max_epoch iterations error_threshold random_number =
    let 
        get_error = evaluate_weight validation_set random_number iterations
        weight_error = V.map get_error weights
        error_weights = V.take max_epoch (V.zip weight_error weights)
    in
        select_optimal error_weights error_threshold



----- # MAIN # -----


-- get positions  
select_split :: Vector (Vector Float) -> Int -> Float -> [Int]
select_split vector random_number percentage = 
    let instances = floor (fromIntegral (V.length vector) * percentage) in
    L.take instances . nub $ (randomRs (0,V.length vector - 1) (mkStdGen random_number))


-- get remainder
delete_split :: Vector (Vector Float) -> [Int] -> Vector (Vector Float)
delete_split general_vector old_split =
    let 
        count_list = L.take (V.length general_vector) (L.iterate (+1) 0) 
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
main :: IO (Int, Vector Float, Float)
main = do

    -- store normalized data from data.txt
    all_data <- Normalize.normalize_data
    let data_vector = fromList (fmap fromList all_data)
 
    -- get training data
    rng_seed <- newStdGen
    let (random_seed, seed) = randomR (0, V.length data_vector) (rng_seed)
    let (training_data, aux_vector) = split_data data_vector random_seed 0.9

    -- get validation & test sets
    let random_number = next_rng (L.length training_data) random_seed
    let (validation_set, test_set) = split_data aux_vector random_seed 0.5


    -- train phase
    let dimension = V.length (V.tail (V.head data_vector))
    let training_vector = fmap (flip update (V.singleton (0,1.0))) training_data
    let int_weights = iterateN (dimension + 1) (next_rng (V.length training_vector)) random_number
    let weights = fmap ((/1000).fromIntegral) int_weights
    let real_out = fmap (V.head) data_vector

    let error_threshold = 0.01
    let max_epoch = 100000000
    let act_function = sigmoid

    let train_functions = train training_vector real_out act_function random_number max_epoch
    let trained_weights = V.scanl' (\x f -> f x) weights train_functions


    -- validation & test phase
    let validation_random = next_rng (V.length validation_set - 1) random_seed
    let checks_num = 100
    let optimal_weight = validate_results validation_set trained_weights max_epoch checks_num error_threshold validation_random

    let test_random = next_rng (V.length test_set - 1) random_seed
    let final_error = evaluate_weight test_set test_random checks_num (snd optimal_weight)


    -- print & return value
    print (fst optimal_weight, snd optimal_weight, final_error)
    return (fst optimal_weight, snd optimal_weight, final_error)
