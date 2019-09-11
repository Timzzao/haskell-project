module Cesar where

import Data.Char

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0, 
         0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 
         6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

-- retorna a n-ésima letra, seguinte
-- evite ultrapassar o limite com `mod` 26
shift :: Int -> Char -> Char
shift n letter
    | letter == ' ' = ' '
    | otherwise = int2let $ (let2int letter + n) `mod` 26

-- aplica a função shift em cada letra da string
encode :: Int -> String -> String
encode n = map (shift n)

crack :: String -> String
crack xs = encode (-factor) xs
    where
        factor = head (positions (minimum chitab) chitab)
        chitab = [chisqr (rotate n table') table | n <- [0..25]]
        table' = freqs xs

-- quantidade de letras minúculas em uma String
lowers :: String -> Int
lowers [] = 0
lowers (l:ls)
    | isLower l && l /= ' ' = 1 + lowers ls
    | otherwise = lowers ls

-- conta a ocorrência de um caracter em uma String
count :: Char -> String -> Int
count _ [] = 0
count letter (l:ls)
    | l == letter && l /= ' ' = 1 + count letter ls
    | otherwise = count letter ls

-- dado um n e m, calcule 100 * n/m
percent :: Int -> Int -> Float
percent n m = fromRational (100 * (fromIntegral n / fromIntegral m))

-- calcule a porcentagem de cada letra minúscula
-- do alfabeto em uma String
-- a porcentagem é a contagem de ocorrência pelo total
-- de letras minúsculas
freqs :: String -> [Float]
freqs letters = map (calcFreq letters) ['a'..'z']
    where
        calcFreq ls l = percent (count l ls) (lowers ls)

-- Calcule a medida de Chi-Quadrado de duas
-- tabelas de frequência:
-- Soma (Observado - Esperado)²/Esperado
chisqr :: [Float] -> [Float] -> Float
chisqr obs expec = sum [(x - y) ^ 2 / y | (x, y) <- zip obs expec]

-- rotaciona uma tabela em n posições
rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n table = snd splitted ++ fst splitted
    where
        splitted = splitAt n table

-- retorna a lista de posições que contém um
-- elemento x
positions :: Eq a => a -> [a] -> [Int]
positions = getOcurrences 0
    where
        getOcurrences _ _ [] = []
        getOcurrences n letter (l:ls)
            | letter == l = n : getOcurrences (n + 1) letter ls
            | otherwise = getOcurrences (n + 1) letter ls