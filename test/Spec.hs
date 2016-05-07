{-# OPTIONS_GHC -Wall #-}
module Main where

import Eel

bar :: V Word8 -> I Word8
bar = func "bar" $ \x -> do
  i <- add x (lit 32)
  add i i
  
foo :: (V Int32, V Word8) -> I Int32
foo = func "foo" $ \(x,y) -> do
  i <- bar y
  j <- cast x
  k <- shr i j
  m <- bar $ lit 11
  l <- ge k m
  cast l

putu :: V Word32 -> M ()
putu = ffi' "putu"
puti :: V Int32 -> M ()
puti = ffi' "puti"
putf :: V Float -> M ()
putf = ffi' "putf"

main :: IO ()
main = mainM $ \(_argc, _argv) -> do
  putu (lit 0xffffffff)
  puti (lit 0xffffffff)
  x <- cast (lit 0xffffffff :: V Word32)
  putf x
  return $ lit 0
  
  -- mainM $ \(argc, _argv) -> foo (argc, lit 21)
  -- mainM $ \(argc, _argv) -> do
  --   p <- alloca
  --   store argc (return p)
  --   load (return p)
  -- mainM $ \(argc, _argv) -> do
  --   p <- alloca' (lit 10)
  --   p' <- getelementptr (return p) (lit 3)
  --   store argc (return p')
  --   load (return p')
