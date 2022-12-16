module Normalize (normalize_data) where
import Reader (read_data)

-- transpose list
transpose :: [[Float]] -> [[Float]]


-- normalize one single list
normalize_aux :: [Float] -> [Float]
normalize_aux input_list =
    let max_value = foldl max 0 input_list in
    fmap (/ max_value) input_list

-- [[a,b,c],[e,f,g],[h,i,j]] --> [[a,e,h],[b,f,i],[]]

-- main
normalize_data :: IO([[Float]])
normalize_data = do

    -- read data
    formatted_contents <- Reader.read_data

    -- auxiliar data
    let instances = length formatted_contents
    let dimension = length (tail (head formatted_contents))

    -- normalize
    let transposed_contents = transpose formatted_contents
    let normalized_data = map (normalize_aux) transposed_contents

    return normalized_data
