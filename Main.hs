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


-- auxiliar for inputs
untuple :: (Float, Float) -> Float
untuple (weight, value) = weight * value


-- neuron function
neuron :: (Float -> Float) -> [(Float,Float)] -> Float -> Float
neuron activation_function inputs bias =
    activation_function ( bias + (foldl (+) 0.0 (map untuple inputs)))


-- delta rule
delta_rule :: Float -> Float -> Float -> (Float, Float) -> (Float, Float)
delta_rule n error x (weight, value) = 
    (weight + n * error * x, value)
   

    -- w_i(t + 1) = w_i(t) + µ(d(t) - y(t))xi(t)
    --

-- get element at specific position
data_at :: Int -> [a] -> a
data_at _ [] = error "empty data list"
data_at y (x:xs) | y <= 0 = x
                 | otherwise = data_at (y-1) xs


-- weight modification
learn :: [[(Float, Float)]] -> Int -> [Float] -> (Float -> Float) -> [[(Float, Float)]]
learn inputs element expected_results activation_function =

    let error = ((data_at element expected_results) - (neuron activation_function (data_at element inputs) 1.0))
        in fmap (fmap (delta_rule 0.1 error 0.1)) inputs


-- core function
train :: [[(Float,Float)]] -> [Float] -> Float -> Float -> Int -> Int -> (Float -> Float) -> (Int, StdGen) -> ([Float],Float)
train inputs expected_results error error_threshold epoch max_epoch act_function element =
    
    if (epoch > max_epoch || error < error_threshold)
        then ((fmap (fst) (data_at (fst element) inputs)), error)
    else 
        train 
            (learn inputs (fst element) expected_results act_function) 
            expected_results 
            ((data_at (fst element) expected_results)-(neuron sigmoid (data_at (fst element) inputs) 1.0))
            error_threshold 
            (epoch + 1) 
            max_epoch 
            act_function 
            (randomR (0, length inputs) (snd element))
 

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
main :: IO ([Float], Float)
main = do
    
    all_data <- Normalize.normalize_data
 
    -- auxiliar data gathered from input
    let instances = length all_data
    let dimension = length (tail (head all_data))

    -- init random numbers 
    let (random_number, seed) = randomR (0,instances) (mkStdGen instances)

    -- training inputs calculations
    let inputs = fmap (tail) all_data
    let bias_inputs = fmap (1:) inputs
    let weights = init_weights (dimension + 1) (fromIntegral random_number, seed) []
    let real_out = fmap (head) all_data

    -- hard-coded training values
    let error_threshold = 0.000001
    let max_epoch = 700
    let act_function = heaviside

    -- train & store result
    let (final_weights, epoch) = train formatted_inputs real_out 1 error_threshold 0 max_epoch act_function (random_number, seed)

    return (final_weights, epoch) 
