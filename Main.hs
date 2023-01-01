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
train :: Vector (Vector Float) -> Vector Float -> (Float -> Float) -> Int -> Int -> [Vector Float -> Vector Float]
train training_set expected_results act_function instances seed =
    let all_rng = iterate (next_rng instances) seed in
    List.map (update_weights training_set expected_results act_function) all_rng



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
    let 
        training_instance = (training_set !! random_number)
        approximation = zip_fold (*) weight (List.tail training_instance) 
    in
        List.head training_instance - approximation :
        evaluate_weight training_set weight (next_rng instances random_number) instances


-- aux function
check_weight :: [[Float]] -> Int -> Int -> Int -> [Float] -> Float
check_weight  training_set random_number instances iterations weight =
    let list = evaluate_weight training_set weight random_number instances in
        abs (List.foldl (+) 0.0 (List.take iterations list)) / (fromIntegral iterations)


-- check_weight (weight ...) !! max_value
select_optimal:: [[Float]] -> [[Float]] -> Int  -> Int -> Int -> Int -> (Float, [Float])
select_optimal training_set weights random_number instances iterations max_epoch =
    let 
        get_error = check_weight training_set random_number instances iterations
        weight_error = List.map get_error weights 
    in
        List.minimum (List.zip (List.take max_epoch weight_error) weights)



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
    let max_epoch = 10000000
    let act_function = sigmoid

    -- get & apply all train functions
    let train_functions = train training_vector real_out act_function (V.length training_vector) random_number
    let trained_weights = List.scanl' (\x f -> f x) weights train_functions


    print (trained_weights !! max_epoch)
    return (trained_weights !! max_epoch)
    
    {- 
    -- get optimal value
    let checks_num = 5
    let optimal_weight = select_optimal trained_weights trained_weights random_number (List.length training_set) checks_num max_epoch

    -- print & return value
    print optimal_weight
    return optimal_weight
    -}
