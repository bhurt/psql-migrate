{-# LANGUAGE ScopedTypeVariables #-}

module Main (
    main
) where

    import Test.Hspec (hspec)
    import Tests (tests)

    main :: IO ()
    main = hspec tests

