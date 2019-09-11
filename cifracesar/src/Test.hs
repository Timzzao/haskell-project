module Test where

import Cesar

-- aplicando shift duas vezes, uma com o valor negativo, o caracter
-- deve ser o mesmo
prop_neg_shift :: Int -> Char -> Bool
prop_neg_shift n l 
    | l `elem` ['a'..'z'] = shift (-n) (shift n l) == l
    | otherwise = True

-- o tamanho da mensagem codificada deve ser o mesmo da original
prop_enc_length :: Int -> String -> Bool
prop_enc_length n ls = length ls == length (encode n ls)

-- do decode do encode deve ser a string original
prop_enc_dec :: Int -> String -> Bool
prop_enc_dec n letters 
    | foldr ((&&) . isLower) True letters = crack (encode n letters) == letters
    | otherwise = True
