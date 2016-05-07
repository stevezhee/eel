{-# OPTIONS_GHC -Wall #-}
module Main where

import Eel

bar :: I Word8 -> I Word8
bar = func "bar" $ \x -> do
  i <- add x (lit 32)
  add (return i) (return i)
  
foo :: (I Int32, I Word8) -> I Int32
foo = func "foo" $ \(x,y) -> cast $ ge (shr (bar y) (cast x)) (bar $ lit 11)

putu :: I Word32 -> M ()
putu = ffi' "putu"
puti :: I Int32 -> M ()
puti = ffi' "puti"
putf :: I Float -> M ()
putf = ffi' "putf"

main :: IO ()
main = mainM $ \(_argc, _argv) -> do
  -- putu (lit 0xffffffff)
  -- puti (lit 0xffffffff)
  putf (cast (lit 0xffffffff :: I Word32) :: I Float)
  lit 0
  
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
