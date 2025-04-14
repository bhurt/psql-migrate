{-# LANGUAGE ScopedTypeVariables #-}

import           Test.HUnit                 (runTestTTAndExit)
import           Tests                      (tests)

main :: IO ()
main = runTestTTAndExit tests

