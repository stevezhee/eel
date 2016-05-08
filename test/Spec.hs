{-# LANGUAGE ScopedTypeVariables #-}
-- {-# OPTIONS_GHC -Wall -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import Eel hiding (mainM)
import qualified Eel as E

bar :: Word' -> M Word'
bar = func "bar" $ \x -> do
  i <- add x (lit 32)
  add i i
  
foo :: (Int', Word') -> M Int'
foo = func "foo" $ \(x,y) -> do
  i <- bar y
  j <- cast x
  k <- shr i j
  m <- bar $ lit 11
  l <- ge k m
  cast l

putu :: Word' -> M ()
putu = ffi "putu"
puti :: Int' -> M ()
puti = ffi "puti"
putf :: Float' -> M ()
putf = ffi "putf"
putb :: V Bool -> M ()
putb = ffi "putb"

init_sdl :: ((Int', Int'), (Int', Int')) -> M ()
init_sdl = ffi "init_sdl"

poll_sdl :: M Int'
poll_sdl = ffi "poll_sdl" ()

cleanup_sdl :: Int' -> M ()
cleanup_sdl = ffi "cleanup_sdl"

class Put a where put :: V a -> M ()
instance Put Bool where put = putb
instance Put Word32 where put = putu
instance Put Int32 where put = puti
instance Put Float where put = putf

true :: Bool'
true = lit True
false :: Bool'
false = lit False

alloc :: Ty a => M (Ptr' a)
alloc = alloca (lit 1 :: Int')

allocn :: Ty a => Int' -> M (Ptr' a)
allocn = alloca

mainM :: M () -> IO ()
mainM m = E.mainM $ \(_argc, _argv) -> m >> return (lit 0)

ix :: Ty a => V (Ptr a) -> Int' -> M (Ptr' a)
ix = getelementptr

instance Num Int' where fromInteger = lit . fromInteger
instance Num Float' where fromInteger = lit . fromInteger
instance Num Word' where fromInteger = lit . fromInteger
  
while :: M Bool' -> M ()
while x = do
  start_lbl <- newLabel
  done_lbl <- newLabel
  br' start_lbl
  block start_lbl
  a <- x
  br a start_lbl done_lbl
  block done_lbl

type Bool' = V Bool

oneof :: Ret r => M r -> [(M Bool', M r)] -> M r
oneof = foldr (\(r,a) b -> if' r a b)

when :: I Bool -> M () -> M ()
when x y = if' x y (return ())

switch :: (Cmp a, Ret r) => I a -> (V a -> M r) -> [(I a, M r)] -> M r
switch x f zs = do
  a <- x
  oneof (f a) [ (e >>= eq a, r) | (e, r) <- zs ]

sdl_quit :: Int'
sdl_quit = 256
sdlk_up :: Int'
sdlk_up = 1073741906
sdlk_down :: Int'
sdlk_down = 1073741905
sdlk_left :: Int'
sdlk_left = 1073741904
sdlk_right :: Int'
sdlk_right = 1073741903

type Char' = V Char
type Float' = V Float
type Int' = V Int32
type Word' = V Word32

type Ptr' a = V (Ptr a)

load_rgba :: (Ptr' Char, (Int', Int')) -> M (Ptr' Texture)
load_rgba = ffi "load_rgba"
                                    
present_sdl :: M ()
present_sdl = ffi "present_sdl" ()

clear_sdl :: M ()
clear_sdl = ffi "clear_sdl" ()

data Texture
instance Ty Texture where ty _ = ty (unused "Texture" :: M Int')

blit :: (Ptr' Texture, (Int', Int'), (Int', Int')) -> M ()
blit = ffi "blit"

set_color :: (Int', Int', Int', Int') -> M ()
set_color = ffi "set_color"

sdl_delay :: Int' -> M ()
sdl_delay = ffi "SDL_Delay"

cstring :: String -> M (Ptr' Char)
cstring s = newn $ fmap lit $ s ++ ['\0']

new :: Ty a => V a -> M (Ptr' a)
new x = do
  p <- alloc
  store x p
  return p
  
newn :: Ty a => [V a] -> M (Ptr' a)
newn xs = do
  p <- allocn $ lit $ fromIntegral $ length xs
  sequence_ [ ix p (lit i) >>= store x | (i,x) <- zip [0 ..] xs ]
  return p

modify :: Ty a => Ptr' a -> (V a -> I a) -> I a
modify p f = do
  a <- load p
  a' <- f a
  store a' p
  return a
  
main :: IO ()
main = mainM $ do
  init_sdl ((30,550), (1024, 256))
  set_color (0,0,0x90,0xff)
  fn <- cstring "ship.bmp.rgba"
  let sz = (64, 56)
  tex <- load_rgba (fn, sz)
  px <- new 100
  pvx <- new 0
  while $ do
    -- update
    vx <- load pvx
    modify px $ add vx
    -- render
    clear_sdl
    x <- load px
    blit (tex, (x,100), sz)
    present_sdl
    -- sleep
    sdl_delay 16
    -- input
    switch poll_sdl
      (\_ -> return true)
      [ (return sdl_quit, return false)
      , (return sdlk_up, puti 4 >> return true)
      , (return sdlk_down, puti 5 >> return true)
      , (return sdlk_left, (modify pvx (`sub` 1)) >> return true)
      , (return sdlk_right, (modify pvx $ add 1) >> return true)
      ]
  cleanup_sdl 0
