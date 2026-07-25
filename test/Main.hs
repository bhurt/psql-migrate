{-# LANGUAGE ScopedTypeVariables #-}

module Main (
    main
) where

    import qualified Test.Hspec as Hspec
    import           Tests      (tests)

    main :: IO ()
    main = Hspec.hspec tests

