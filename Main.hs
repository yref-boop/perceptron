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


-- get next inte
next_rng :: Int -> Int -> Int
next_rng limit rng = fst(randomR (0, limit) (mkStdGen rng))


-- update weights 
learn :: [Float] -> Float -> Float -> (Float -> Float) -> [Float] -> [Float]
learn training_set learning_rate expected_result act_function weights =
    
    let actual_results = (neuron act_function training_set weights) in 
    zipWith (delta learning_rate expected_result actual_results) weights training_set


-- TODO:
    -- iterate
    -- generate new random value in each iteration (?)
train :: [[Float]] -> Float -> [Float] -> (Float -> Float) -> Int -> [Float] -> [Float]
train training_set learning_rate expected_results act_function pos weights =
    learn training_set learning_rate expected_results act_function pos weights


-- create weights list
init_weights :: Int -> (Float, StdGen) -> [Float] -> [Float]
init_weights n (random, seed) list = 
    if (n == 0) then list
    else
        init_weights 
            (n-1) 
            (randomR (0,1000) (mkStdGen(floor random))) 
            ((random/1000):list) 


-- main
main :: IO ([Float], Int)
main = do
    
    all_data <- Normalize.normalize_data
 
    -- auxiliar data gathered from input
    let instances = length all_data
    let dimension = length (tail (head all_data))

    -- init random numbers 
    let (random_number, seed) = randomR (0,instances) (mkStdGen instances)

    -- training inputs calculations
    let inputs = fmap (tail) all_data
    let formatted_inputs = fmap (1:) inputs
    let weights = init_weights (dimension + 1) (fromIntegral random_number, seed) []
    let real_out = fmap (head) all_data

    -- hard-coded training values
    let error_threshold = 0.0000001
    let max_epoch = 300000
    let act_function = sigmoid

    -- train & store result
    --let (final_weights, epoch) = train formatted_inputs weights real_out 1 error_threshold 0 max_epoch act_function (random_number, seed)

    --return (final_weights, epoch) 
    return ([],0)
