-- {-# OPTIONS_GHC -Wall -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import Eel hiding (alloca, mainM)
import qualified Eel as E

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
putb :: V Bool -> M ()
putb = ffi' "putb"

class Put a where put :: V a -> M ()
instance Put Bool where put = putb
instance Put Word32 where put = putu
instance Put Int32 where put = puti
instance Put Float where put = putf

true = lit True
false = lit False

alloca :: Ty a => I (Ptr a)
alloca = E.alloca (lit 1 :: V Int32)

allocn :: Ty a => V Int32 -> I (Ptr a)
allocn = E.alloca

mainM :: M () -> IO ()
mainM m = E.mainM $ \(_argc, _argv) -> m >> return (lit 0)

ix :: Ty a => V (Ptr a) -> V Int32 -> I (Ptr a)
ix = getelementptr

instance Num (V Int32) where fromInteger = lit . fromInteger
instance Num (V Float) where fromInteger = lit . fromInteger
instance Num (V Word32) where fromInteger = lit . fromInteger
  
if' :: V Bool -> M () -> M () -> M ()
if' x y z = do
  true_lbl <- newLabel
  false_lbl <- newLabel
  done_lbl <- newLabel
  br x true_lbl false_lbl
  block true_lbl
  y
  br' done_lbl
  block false_lbl
  z
  br' done_lbl
  block done_lbl
  
main :: IO ()
main = mainM $ do
  if' true (putb true) (putb false)
  -- putu (lit 0xffffffff)
  -- puti (lit 0xffffffff)
  -- x <- cast (lit 0xffffffff :: V Word32)
  -- putf x
  -- putb $ lit True
  -- putb $ lit False
  -- return $ lit 0
  -- p <- alloca
  -- store true p
  -- load p >>= put

  -- p <- allocn 42
  -- p' <- ix p 0
  -- store true p
  -- load p >>= put
  -- store false p
  -- load p >>= put
  -- p' <- ix p 13
  -- store true p
  -- load p >>= put
  -- store false p
  -- load p >>= put
  
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
