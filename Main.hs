module Main (main) where
import Normalize (normalize_data)
import System.Random hiding (split)
import Data.List as L
import Data.Vector as V



----- # RANDOM VALUE GENERATING # -----


-- get next random integer
next_random :: Int -> Int -> Int
next_random limit random_seed =
    fst (randomR (0, limit) (mkStdGen random_seed))


-- get list of non-repeated random integers
unique_randoms :: Int -> Int -> Int -> [Int]
unique_randoms iterations limit random_seed =
    L.take iterations . nub $ randomRs (0, limit) (mkStdGen random_seed)



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
        estimation = neuron act_function (training_set ! pos) (weights)
        error = (expected_results ! pos) - estimation
    in
        V.zipWith (delta error) weights (training_set ! pos)


-- list of learning functions with each position
train_function :: Vector (Vector Float) -> Vector Float -> (Float -> Float) -> Int -> [(Vector Float -> Vector Float)]
train_function training_set expected_results act_function seed =
    let rng_list = iterate (next_random (V.length training_set)) seed in
    L.map (update_weights training_set expected_results act_function) rng_list


-- calculate necesary values for train_function
train :: Vector (Vector Float) -> Vector (Vector Float) -> (Float -> Float) -> Int -> [Vector Float]
train data_set training_set act_function seed =
    let
        training_vector = fmap (flip update (V.singleton (0,1.0))) training_set
        results = fmap (V.head) data_set
        train_functions =
                train_function training_vector results act_function random

        dimension = V.length (V.tail (V.head data_set))
        random = next_random (L.length training_set) seed
        next_vector = next_random (V.length training_vector)
        int_weights = iterateN (dimension + 1) next_vector random
        weights = fmap ((/1000).fromIntegral) int_weights

    in
        L.scanl' (\x f -> f x) weights train_functions



----- # VALIDATION # -----


-- validate one weight against one example
get_error :: Vector (Vector Float) -> Vector Float -> Int -> Float
get_error training_set weight position =
    let
        values = V.zipWith (*) weight (V.tail (training_set ! position))
        approximation = V.foldl (+) 0.0 values / fromIntegral (V.length values)
        real = V.head (training_set ! position)
    in
        abs (real - approximation)


-- for a weight, iterate validation over a random set of examples
evaluate_weight :: Vector (Vector Float) -> Int -> Int -> Vector Float -> Float
evaluate_weight training_set random_number iterations weight =
    let
        size = (V.length training_set - 1)
        indexes = fromList (unique_randoms iterations size random_number)
        error_vector = V.map (get_error training_set weight) indexes
    in
        V.foldl (+) 0.0 (error_vector) / fromIntegral (V.length error_vector)


-- gets the weights with the best estimation value
select_optimal :: [(Float, Vector Float)] -> Float -> (Int, Vector Float)
select_optimal error_weights error_threshold =
    let
        optimize :: [(Float, Vector Float)] -> (Float, Vector Float) -> Float -> Int -> (Int, Vector Float)
        optimize weights minimum threshold count =
            case weights of
            [] -> (count, snd minimum)
            (head : tail) ->
                if (fst head < threshold) then (count, snd minimum)
                else optimize tail (min head minimum) threshold (count + 1)
    in
        optimize error_weights (1/0, V.singleton 0) error_threshold 0


-- get weight with better generalization on validation_set
validate :: Vector (Vector Float) -> [Vector Float] -> Int -> Int -> Float -> Int -> (Int, Vector Float)
validate validation_set weights max_epoch iterations error_threshold random_seed =
    let
        random_number = next_random (V.length validation_set - 1) random_seed
        evaluate = evaluate_weight validation_set random_number iterations
        weight_error = L.map evaluate weights
        error_weights = L.take max_epoch (L.zip weight_error weights)
    in
        select_optimal error_weights error_threshold



----- # SPLITTING DATASET # -----


-- get positions  
select_indexes :: Vector (Vector Float) -> Int -> Float -> [Int]
select_indexes vector random_number percent =
    let instances = floor (fromIntegral (V.length vector) * percent) in
        unique_randoms instances ((V.length vector) - 1) random_number


-- get remainder
delete_split :: Vector (Vector Float) -> [Int] -> Vector (Vector Float)
delete_split vector old_indexes =
    let
        count = L.take (V.length vector) (L.iterate (+1) 0)
        indexes = fromList (count \\ old_indexes)
    in
        V.map (vector !) indexes
    

-- split a vector into two
split_data :: Vector (Vector Float) -> Int -> Float -> (Vector (Vector Float), Vector (Vector Float))
split_data original_vector random_number percent =
    let
        indexes = select_indexes original_vector random_number percent
        split_vector = V.map (original_vector !) (fromList indexes)
        remaining_vector = delete_split original_vector indexes
    in
        (split_vector, remaining_vector)


-- split into the three sets
split ::  Vector (Vector Float) -> Int -> (Float, Float) -> (Vector (Vector Float), Vector (Vector Float), (Vector (Vector Float)))
split vector random percents =
    let
        (training, aux_vector) = split_data vector random (fst percents)
        (validation, test) = split_data aux_vector random (snd percents)
    in
        (training, validation, test)



----- # MAIN # -----


main :: IO [Float]
main = do

    -- store normalized data from data.txt
    all_data <- Normalize.normalize_data
    let data_set = fromList (fmap fromList all_data)

    -- generate random number
    rng_seed <- newStdGen
    let (random_seed, seed) = randomR (0, V.length data_set) (rng_seed)
    
    -- divide data into the three necessary sets
    let (training_set, validation_set, test_set) = split
            data_set random_seed (0.9, 0.5)

    -- training phase
    let act_function = sigmoid
    let trained_weights = train
            data_set training_set act_function random_seed

    -- validation phase
    let accuracy = 0.01
    let epochs = 10000000
    let checks = 10
    let optimal_weight = validate
            validation_set trained_weights epochs checks accuracy random_seed
 
    -- test function
    let test_rng = next_random (V.length test_set - 1) random_seed
    let final_error = evaluate_weight
            test_set test_rng checks (snd optimal_weight)

    -- print & return value
    print (fst optimal_weight, snd optimal_weight, final_error)
    return (toList (snd optimal_weight))
