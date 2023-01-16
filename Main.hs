module Main (main) where
import Normalize (normalize_data)
import System.Random hiding (split)
import Data.List as L
import Data.Vector as V



----- # RANDOM VALUE GENERATING # -----

-- get next random integer
next_rng :: Int -> Int -> Int
next_rng limit seed = fst(randomR (0, limit) (mkStdGen seed))


-- get list of non-repeated random integers
unique_rng :: Int -> Int -> Int -> [Int]
unique_rng iterations size random_number =
    L.take iterations . nub $ randomRs (0, size) (mkStdGen random_number)



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


-- iterate learn function over weights using different examples each time
update_weights :: Vector (Vector Float) -> Vector Float -> (Float -> Float) -> Int -> Vector Float -> Vector Float
update_weights training_set expected_results act_function pos weights =
    let 
        estimation = (neuron act_function (training_set ! pos) (weights))
        error = (expected_results ! pos) - estimation
    in
        V.zipWith (delta error) weights (training_set ! pos)


-- list of learning functions with each position 
train_function :: Vector (Vector Float) -> Vector Float -> (Float -> Float) -> Int -> [(Vector Float -> Vector Float)]
train_function training_set expected_results act_function seed =
    let all_rng = iterate (next_rng (V.length training_set)) seed in
    L.map (update_weights training_set expected_results act_function) all_rng


-- calculate necesary values for train_function
train :: Vector (Vector Float) -> Vector (Vector Float) -> (Float -> Float) -> Int -> [Vector Float]
train data_vector training_data act_function random =
    let 
        dimension = V.length (V.tail (V.head data_vector))
        random_number = next_rng (L.length training_data) random
        training_vector = fmap (flip update (V.singleton (0,1.0))) training_data
        next_vector = next_rng (V.length training_vector)
        int_weights = iterateN (dimension + 1) next_vector random_number
        weights = fmap ((/1000).fromIntegral) int_weights
        real_out = fmap (V.head) data_vector
        
        train_functions = train_function training_vector real_out act_function random_number
    in
        L.scanl' (\x f -> f x) weights train_functions



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
        pos_vect = fromList (unique_rng iterations size random_number)
        error_vector = V.map (get_error training_set weight) pos_vect
    in
        V.foldl (+) 0.0 (error_vector) / fromIntegral (V.length error_vector)


optimize :: [(Float, Vector Float)] -> (Float, Vector Float) -> Float -> Int -> (Int, Vector Float)
optimize weights minimum threshold count =
    case weights of
    []    -> (count, snd minimum)
    (head : tail) -> 
        if (fst head < threshold) then (count, snd minimum)
        else optimize tail (min head minimum) threshold (count + 1)


select_optimal :: [(Float, Vector Float)] -> Float -> (Int, Vector Float)
select_optimal error_weights error_threshold =
    optimize error_weights (1/0, V.singleton 0) error_threshold 0


-- get weight with better generalization on validation_set
validate_results :: Vector (Vector Float) -> [Vector Float] -> Int -> Int -> Float -> Int -> (Int, Vector Float)
validate_results validation_set weights max_epoch iterations error_threshold random_number =
    let 
        get_error = evaluate_weight validation_set random_number iterations
        weight_error = L.map get_error weights
        error_weights = L.take max_epoch (L.zip weight_error weights)
    in
        select_optimal error_weights error_threshold



----- # SPLITTING DATASET # -----


-- get positions  
select_split :: Vector (Vector Float) -> Int -> Float -> [Int]
select_split vector random_number percent = 
    let instances = floor (fromIntegral (V.length vector) * percent) in
        unique_rng instances ((V.length vector) - 1) random_number


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
split_data general_vector random_number percent =
    let 
        split_pos = (select_split general_vector random_number percent)
        split_vector = (V.map (general_vector !) (fromList split_pos))
        remaining_vector = (delete_split general_vector split_pos)
    in 
        (split_vector, remaining_vector)


-- split into the three sets
split ::  Vector (Vector Float) -> Int -> (Float, Float) -> (Vector (Vector Float), Vector (Vector Float), (Vector (Vector Float)))
split vector random percents =
    let
        (training, aux_vector) = split_data vector random (fst percents)
        random_number = next_rng (L.length training) random
        (validation, test) = split_data aux_vector random (snd percents)
    in
        (training, validation, test)



----- # MAIN # -----


main :: IO [Float]
main = do

    -- store normalized data from data.txt
    all_data <- Normalize.normalize_data
    let data_set = fromList (fmap fromList all_data)
 
    -- auxiliar data gathered from input
    let dimension = V.length (V.tail (V.head data_set))

    -- generate random number
    rng_seed <- newStdGen
    let (random_seed, seed) = randomR (0, V.length data_set) (rng_seed) 
    
    -- divide data into the three necessary sets
    let (training_set, validation_set, test_set) = split data_set random_seed (0.9, 0.5)

    -- training phase
    let act_function = sigmoid
    let trained_weights = train data_set training_set act_function random_seed

    -- validation phase
    let random = next_rng (V.length validation_set - 1) random_seed
    let error_threshold = 0.00005
    let epochs = 100000
    let checks_num = 1
    let optimal_weight = (validate_results validation_set trained_weights epochs checks_num error_threshold random)
 
    -- test function
    let random = next_rng (V.length test_set - 1) random_seed
    let final_error = evaluate_weight test_set random checks_num (snd optimal_weight)

    -- print & return value
    print (fst optimal_weight, snd optimal_weight, final_error)
    return (toList (snd optimal_weight))
