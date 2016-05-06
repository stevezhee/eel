{-# OPTIONS_GHC -Wall #-}
module Main where

import Eel

bar :: I Word8 -> I Word8
bar = func "bar" $ \x -> add x (lit 32)
  
foo :: (I Int32, I Word8) -> I Int32
foo = func "foo" $ \(x,y) -> cast $ ge (shr (bar y) (cast x)) (bar $ lit 11)
  
main :: IO ()
main =
  mainM $ \(argc, _argv) -> foo (argc, lit 21)
