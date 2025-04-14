{-# LANGUAGE ScopedTypeVariables #-}

import           Test.Syd (sydTest)
import           Tests    (tests)

main :: IO ()
main = sydTest $ do
            tests

