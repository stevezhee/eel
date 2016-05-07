{-# LANGUAGE ScopedTypeVariables #-}
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
putu = ffi "putu"
puti :: V Int32 -> M ()
puti = ffi "puti"
putf :: V Float -> M ()
putf = ffi "putf"
putb :: V Bool -> M ()
putb = ffi "putb"

init_sdl :: ((V Int32, V Int32), (V Int32, V Int32)) -> M ()
init_sdl = ffi "init_sdl"

poll_sdl :: I Int32
poll_sdl = ffi "poll_sdl" ()

cleanup_sdl :: V Int32 -> M ()
cleanup_sdl = ffi "cleanup_sdl"

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
  
while :: I Bool -> M ()
while x = do
  start_lbl <- newLabel
  done_lbl <- newLabel
  br' start_lbl
  block start_lbl
  a <- x
  br a start_lbl done_lbl
  block done_lbl

oneof :: Ret r => M r -> [(I Bool, M r)] -> M r
oneof = foldr (\(r,a) b -> if' r a b)

switch :: (Cmp a, Ret r) => I a -> (V a -> M r) -> [(I a, M r)] -> M r
switch x f zs = do
  a <- x
  oneof (f a) [ (e >>= eq a, r) | (e, r) <- zs ]

sdl_quit = 256

when x y = if' x y (return ())

main :: IO ()
main = mainM $ do
  init_sdl ((30,30), (1024, 1024))
  -- switch (return 0)
  --   (\(i :: V Int32) -> when (return false) (put i) >> return true)
  --   [ (return 0, return false)
  --   ]
  while $ switch poll_sdl
    (\i -> when (ne 0 i) (puti i) >> return true)
    [ (return sdl_quit, return false)
    ]
  -- oneof (puti 3) [(return false, puti 42), (return false, puti 13)]
  -- if' (3 `gt` (2 :: V Int32)) (putb true) (putb false)
  -- if' (3 `gt` (2 :: V Int32)) (return true) (return false) >>= putb
  --   (return true)
  --   [ (sdl_quit, return false)
  --   ]
  
  -- dowhile
  --   e <- poll_sdl
  --   (e == no_event || e /= quit_event)
  cleanup_sdl 0

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
