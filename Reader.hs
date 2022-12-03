module Reader (read_data) where

-- remove last line \n
clean_last :: String -> String
clean_last (string) = case string of
    ("")   -> error "empty file"
    (a:"") -> ""
    (a:b)  -> [a] ++ clean_last b

--clean_last ""       = error "empty file"
--clean_last (a:"")   = ""
--clean_last (a:b)    = a ++ clean_last b

-- tokenize string 
tokenize :: Char -> String -> [String]
tokenize c xs = case break (==c) xs of 
    (ls, "") -> [ls]
    (ls, x:rs) -> ls : tokenize c rs

-- token list to float list
format :: [[String]] -> [[Float]]
format string_list = map (map (read :: String -> Float)) string_list

-- main
read_data :: IO ([[Float]])
read_data = do
    contents <- readFile "data.txt"
    let tokens = map (tokenize ' ') (tokenize '\n' (clean_last(contents)))
    let formatted_contents = format tokens
    return formatted_contents
