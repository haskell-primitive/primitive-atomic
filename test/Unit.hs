{-# language MultiWayIf #-}

import Data.Primitive.Unlifted.Atomic
import Data.Primitive
import Control.Monad (when)

main :: IO ()
main = do
  putStrLn "Start"
  putStrLn "A"
  testA
  putStrLn "Finished"

testA :: IO ()
testA = do
  arr <- newUnliftedArray 20 =<< newByteArray 16
  x <- newByteArray 24
  y <- newByteArray 32
  z <- newByteArray 40
  writeUnliftedArray arr 0 x
  writeUnliftedArray arr 1 y
  writeUnliftedArray arr 2 z
  (success0,old0) <- casUnliftedArray arr 1 z x
  when (not (sameMutableByteArray old0 y)) $ do
    fail "old0 /= y"
  when (success0 == True) $ do
    fail "success0 == True"
  (success1,old1) <- casUnliftedArray arr 0 x z
  if | sameMutableByteArray old1 z -> pure ()
     | sameMutableByteArray old1 y -> fail "old1 == y"
     | sameMutableByteArray old1 x -> fail "old1 == x"
     | otherwise -> fail "old1 /= z"
  when (success1 == False) $ do
    fail "success1 == False"
