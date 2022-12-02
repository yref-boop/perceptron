-- tokenize string 
--tokenize :: [String] -> String -> [String] 
--tokenize result string = case string of
    --'\n':rest  -> tokenize ((result ++ []) rest)
    --' ':rest   -> tokenize result ++ [] rest
    --char:rest  -> tokenize last result ++ char rest
    --char:[]    -> last result ++ char 

--tokenize :: [Char] -> [[Char]]
--tokenize [] = []
--tokenize (' ':rest) = tokenize rest
--tokenize ('\n':rest) = tokenize rest
--tokenize (char:rest) = [char]:tokenize rest 

tokenize :: Char -> String -> [String]
tokenize c xs = case break (==c) xs of 
    (ls, "") -> [ls]
    (ls, x:rs) -> ls : tokenize c rs

--tokenize string =
--    let aux_tokenize tokens (aux_string)
--        ('\n':rest) = aux_tokenize((tokens ++ []) rest) 
--    in aux_tokenize [] string

-- format token list
--format :: [[Char]] -> [Float]
--format (a:[]) = [read a]
--format (a:b) = [read a] : format b

-- 
format :: [String] -> [Float]
format string = map (read :: String -> Float) string

-- main
main :: IO ([Float])
main = do
    contents <- readFile "data.txt"
    let tokens = tokenize ' ' contents
    let formatted_contents = format tokens
    print formatted_contents
    return formatted_contents
