{-# OPTIONS_GHC -Wall #-}
module Main where

import Eel

main :: IO ()
main =
  runM $ sub (add (lit 21) (lit 32)) (lit 11)
