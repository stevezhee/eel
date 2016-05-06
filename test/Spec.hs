{-# OPTIONS_GHC -Wall #-}
module Main where

import Eel

main :: IO ()
main =
  runM $ cast $ ge (shr (add (lit 21 :: M Word8) (lit 32)) (lit 1)) (lit 11)
