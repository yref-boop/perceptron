module Main (main) where
import Normalize (normalize_data)
import System.Random 


-- heaviside function
heaviside :: Float -> Float
heaviside x =
    if (x > 0) then 1
        else 0

-- absolute value function
absolute :: Float -> Float
absolute x =
    if (x < 0) then -1.0 * x
        else x


-- relu function
relu :: Float ->  Float
relu x =
    if (x < 0) then 0
        else x


-- sigmoid function
sigmoid :: Float -> Float
sigmoid x = 1.0 / ( 1 + exp (-x) )


-- sigmoid derivative
sigmoid_derivative :: Float -> Float
sigmoid_derivative x =
    sigmoid x * (1.0 - sigmoid x )


-- neuron function
neuron :: (Float -> Float) -> [Float] -> [Float] -> Float
neuron activation_function inputs weight=
    activation_function (foldl (+) 0.0 (double_map inputs weight (*) []))

    -- w_i(t + 1) = w_i(t) + µ(d(t) - y(t))xi(t)


-- get element at specific position
data_at :: Int -> [a] -> a
data_at _ [] = error "empty data list"
data_at y (x:xs) | y <= 0 = x
                 | otherwise = data_at (y-1) xs


-- applying a function over two list
double_map :: [t1] -> [t2] -> (t1 -> t2 -> a) -> [a] -> [a]
double_map fst_list snd_list function result_list = case (fst_list, snd_list) of
        ([],[]) -> result_list
        ([],snd) -> error "snd list is bigger than fst"
        (fst,[]) -> error "fst list is bigger than snd"
        (h1:t1,h2:t2) -> double_map t1 t2 function ((function h1 h2):result_list)


-- delta (todo floats)
-- w_i(t+1) = w_1(t) + r (d_j-y_j(t))x_{j,i}
-- map con weights(w) y con input(x)
-- d_j = input_value, y_j = actual_result
delta learning_rate expected_result actual_result old_weight input_value=
    old_weight + learning_rate * (expected_result - actual_result) * input_value


-- update weights
update_weights :: [Float] -> [Float] -> Float -> Float -> Float -> [Float]
update_weights training_values weights learning_rate expected_result actual_result =
    double_map weights training_values (delta learning_rate expected_result actual_result) []


-- core function
train :: [[Float]] -> [Float] -> [Float] -> Float -> Float -> Int -> Int -> (Float -> Float) -> (Int, StdGen) -> ([Float], Int)
train inputs weights expected_results error error_threshold epoch max_epoch act_function rng =
    
    if (epoch >= max_epoch || error < error_threshold)
        then (weights, epoch) 
    else let actual_result = (neuron act_function (data_at (fst rng) inputs) weights) in
        train
            inputs
            (update_weights (data_at (fst rng) inputs) weights 0.1 (data_at (fst rng) expected_results) actual_result)
            expected_results
            (abs((data_at (fst rng) expected_results) - actual_result))
            error_threshold 
            (epoch + 1) 
            max_epoch 
            act_function 
            (randomR (0, length inputs) (mkStdGen (fst rng)))


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
    let error_threshold = 0.000001
    let max_epoch = 100000
    let act_function = sigmoid

    -- train & store result
    let (final_weights, epoch) = train formatted_inputs weights real_out 1 error_threshold 0 max_epoch act_function (random_number, seed)

    return (final_weights, epoch) 
