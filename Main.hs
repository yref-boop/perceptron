module Main (main) where
import Normalize (normalize_data)
import System.Random 

-- sigmoid function
sigmoid :: Float -> Float
sigmoid x = 1.0 / ( 1 + exp (-x) )

-- sigmoid derivative
sigmoid_derivative :: Float -> Float
sigmoid_derivative x =
    sigmoid x * (1.0 - sigmoid x )  -- font used in graphical applications


-- neuron function
neuron :: (Float -> Float) -> [Float] -> [Float] -> Float
neuron activation_function inputs weight=
    activation_function (foldl (+) 0.0 (double_map inputs weight (*) []))


-- get element at specific position
get :: Int -> [a] -> a
get _ [] = error "empty data list"
get y (x:xs) | y <= 0 = x
             | otherwise = get (y-1) xs


-- applying a function over two list
double_map :: [t1] -> [t2] -> (t1 -> t2 -> a) -> [a] -> [a]
double_map fst_list snd_list function result_list = case (fst_list, snd_list) of
        ([],[]) -> result_list
        ([],snd) -> error "snd list is bigger than fst"
        (fst,[]) -> error "fst list is bigger than snd"
        (h1:t1,h2:t2) -> double_map t1 t2 function ((function h1 h2):result_list)


-- delta rule 
delta :: Float -> Float -> Float -> Float -> Float -> Float 
delta learning_rate expected_result actual_result old_weight input_value = 
    old_weight + learning_rate * (expected_result - actual_result) * input_value


-- get next random integer
next_rng :: Int -> Int -> Int
next_rng limit seed = fst(randomR (0, limit) (mkStdGen seed))


-- update weights given specific example 
update_weights :: [Float] -> Float -> Float -> (Float -> Float) -> [Float] -> [Float]
update_weights training_set learning_rate expected_result act_function weights =

    let actual_results = (neuron act_function training_set weights) in 
    zipWith (delta learning_rate expected_result actual_results) weights training_set


-- iterate learn function over weights using different examples each time
learn :: [[Float]] -> Float -> [Float] -> (Float -> Float) -> Int -> [Float] -> [Float]
learn training_set learning_rate expected_results act_function pos weights =
    update_weights (get pos training_set) learning_rate (get pos expected_results) act_function weights


-- list of learning functions with each position 
train :: [[Float]] -> Float -> [Float] -> (Float -> Float) -> Int -> Int -> [[Float] -> [Float]]
train training_set learning_rate expected_results act_function instances seed =
    let all_rng = iterate (next_rng instances) seed in
    map (learn training_set learning_rate expected_results act_function) all_rng


zipIterate :: [[a] -> [a]] -> [a] -> [[a]]
-- zipIterate func_list initial =
--     zipIterate_aux func_list initial 0 []
--     where
--     zipIterate_aux func_list initial iteration result =
--         (func_list !! iteration) initial



-- main
main :: IO ([Float])
main = do
    
    all_data <- Normalize.normalize_data
 
    -- auxiliar data gathered from input
    let instances = length all_data
    let dimension = length (tail (head all_data))

    -- init random numbers
    rng_seed <- newStdGen
    let (random_number, seed) = randomR (0,instances) (rng_seed)

    -- training inputs calculations
    let inputs = fmap (tail) all_data
    let formatted_inputs = fmap (1:) inputs
    let int_weights = take (dimension + 1) (iterate (next_rng 1000) random_number)
    let weights = map ((/1000).fromInteger.toInteger) int_weights
    let real_out = fmap (head) all_data

    -- hard-coded training values
    let error_threshold = 0.0000001
    let max_epoch = 5001
    let act_function = sigmoid

    -- train & store result
    -- let final_weights = zipIterate (train formatted_inputs 0.75 real_out act_function instances random_number) weights

    return []
    -- return (final_weights !! max_epoch)
