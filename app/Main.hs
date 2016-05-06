{-# OPTIONS_GHC -Wall #-}
module Main where

import Eel

main :: IO ()
main = mainM $ \(argc, _argv) -> add (lit 42) argc
