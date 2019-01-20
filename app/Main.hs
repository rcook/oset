module Main (main) where

import qualified Data.Set.Ordered as OSet

main :: IO ()
main = do
    let a = OSet.fromList [5 :: Int, 4, 6, 6, 5, 6]
        b = OSet.fromList [10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0]
        c = OSet.fromList [100, -10, 1, -55]
    print a
    print b
    print c
    print $ mempty <> a <> b <> c
    print $ a <> (b <> c) <> mempty
    print $ (a <> b) <> c
