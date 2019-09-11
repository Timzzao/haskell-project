module RecFunc
    ( and
    , concat
    , replicate
    , (!!)
    , elem
    ) where

import Prelude hiding (and, concat, replicate, (!!), elem)

--Decide se todos os valores lógicos de uma lista são True
and :: [Bool] -> Bool
and [] = True
and (x:xs) = x && and xs

--Concatena uma lista de listas
concat :: [[a]] -> [a]
concat [] = []
concat (x:xs) = x ++ concat xs

--Produz uma lista com n valores idênticos
replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x : replicate (n-1) x

--Seleciona o enésimo elemento de uma lista
(!!) :: [a] -> Int -> a
(x:_) !! 0 = x
(x:xs) !! n = xs !! (n-1)

--Verifica se um valor é um elemento de uma lista
elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem e (x:xs)
    | x == e = True
    | otherwise = elem e xs
