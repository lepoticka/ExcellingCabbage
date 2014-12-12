module Binom where

    binom :: Int -> [[Int]]
    binom n = bs'
              where bs' = [[ evaluate bs' i j | i <- [0..n]] | j <- [0..n]]

    evaluate :: [[Int]] -> Int -> Int -> Int
    evaluate bs 0 _ = 1
    evaluate bs _ 0 = 1
    evaluate bs i j = x + y
                      where x = (bs !! (i-1)) !! j
                            y = (bs !! i) !! (j-1)
