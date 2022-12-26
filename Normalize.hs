module Normalize (normalize_data) where
import Read (read_data)
import Data.List


-- normalize one single list
normalize_aux :: [Float] -> [Float]
normalize_aux input_list =
    let max_value = foldl max 0 input_list in
    fmap (/ max_value) input_list


-- main
normalize_data :: IO([[Float]])
normalize_data = do

    -- read data
    formatted_contents <- Read.read_data

    -- auxiliar data
    let instances = length formatted_contents
    let dimension = length (tail (head formatted_contents))

    -- normalize
    let transposed_contents = transpose formatted_contents
    let normalized_data = map (normalize_aux) transposed_contents
    let normalized_original = transpose normalized_data

    return normalized_original
