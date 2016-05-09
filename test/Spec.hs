{-# LANGUAGE ScopedTypeVariables #-}
-- {-# OPTIONS_GHC -Wall -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import Eel hiding (mainM)
import qualified Eel as E
import Control.Monad hiding (when)

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
putb :: Bool' -> M ()
putb = ffi "putb"
puts :: String' -> M ()
puts = ffi "putcstr"
putc :: Char' -> M ()
putc = ffi "putcchar"
putp :: Ty a => Ptr' a -> M ()
putp = ffi "putp"

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

instance Num Int' where fromInteger = lit . fromInteger
instance Num Float' where fromInteger = lit . fromInteger
instance Num Double' where fromInteger = lit . fromInteger
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
type Double' = V Double
type Int' = V Int32
type Word' = V Word32

type Ptr' a = V (Ptr a)

load_rgba :: (String', (Int', Int')) -> M (Ptr' Texture)
load_rgba = ffi "load_rgba"
                                    
present_sdl :: M ()
present_sdl = ffi "present_sdl" ()

clear_sdl :: M ()
clear_sdl = ffi "clear_sdl" ()

data Texture
instance Ty Texture where ty _ = ty (unused "Texture" :: M Int')

blit :: (Ptr' Texture, (Int', Int'), (Int', Int', Double')) -> M ()
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

ix :: Ty a => V (Ptr a) -> Int' -> M (Ptr' a)
ix = getelementptr

modify :: Ty a => Ptr' a -> (V a -> I a) -> I a
modify p f = do
  a <- load p
  a' <- f a
  store a' p
  return a

load_tex (fn, w, h) = do
  s <- cstring fn
  load_rgba (s, (lit w, lit h))
  
type String' = Ptr' Char

font_gen :: String -> Int32 -> M ()
font_gen fn sz = do
  font <- cstring fn
  ffi "font_gen" (font, lit sz)

loadFont :: String -> Int32 -> M Font
loadFont nm pt = do
  let fid = (nm,pt)
  let (ascent, chs) =
        maybe (error $ "unknown font:" ++ show fid) id $ lookup fid metrics
  let n = lit $ fromIntegral $ length chs
  tex_arr <- allocn n
  gs@[w_arr, h_arr, xmin_arr, advance_arr] <- sequence $ replicate 4 $ allocn n

  let fnt = (tex_arr, gs)

  let loadGlyph (i, (_, ((xmin, _), (ymin, ymax), adv), (fn, (w, h)))) = do
        let f a b = ix b (lit i) >>= store (lit a)
        s <- cstring fn
        tex <- load_rgba (s, (lit w, lit h))
        ix tex_arr (lit i) >>= store tex
        f w w_arr
        f h h_arr
        f xmin xmin_arr
        f adv advance_arr
        
  mapM_ loadGlyph $ zip [0 :: Int32 .. ] chs
  return fnt

blitString :: Font -> String' -> Int' -> Int' -> M ()
blitString fnt s x0 y = do
  pi <- new 0
  px <- new x0
  while $ do
    i <- modify pi $ add 1
    c <- ix s i >>= load
    r <- ne c $ lit '\0'
    when (return r) $ do
      void $ modify px $ \x -> do
        (tex, [w, h, xmin, advance]) <- lookupChar fnt c
        x' <- add x xmin
        blit (tex, (w, h), (x', y, 0))
        add x advance
    return r

main :: IO ()
main = mainM $ do
  init_sdl ((30,550), (1280, 256))
--  font_gen "LuckiestGuy.ttf" 36
  font <- loadFont "LuckiestGuy.ttf" 36
  set_color (0,0x30,0,0xff)
  fn <- cstring "ship.bmp.rgba"
  let sz = (64, 56)
  tex <- load_rgba (fn, sz)
  px <- new 100
  pr <- new 90
  pvx <- new 0
  pvr <- new 0
  s <- cstring "hello, world"
  while $ do
    -- update
    vx <- load pvx
    vr <- load pvr
    modify px $ add vx
    modify pr $ add vr
    -- render
    clear_sdl
    x <- load px
    r <- load pr
    blit (tex, sz, (x,100,r))
    blitString font s 10 40
    present_sdl
    -- sleep
    sdl_delay 16
    -- input
    switch poll_sdl
      (\_ -> return true)
      [ (return sdl_quit, return false)
      , (return sdlk_up, (modify pvx (add 1)) >> return true)
      , (return sdlk_down, (modify pvx (`sub` 1)) >> return true)
      , (return sdlk_left, (modify pvr (`sub` 1)) >> return true)
      , (return sdlk_right, (modify pvr $ add 1) >> return true)
      ]
  cleanup_sdl 0

type Font = (Ptr' (Ptr Texture), [Ptr' Int32])
type Glyph = (Ptr' Texture, [Int'])
  
lookupChar :: Font -> Char' -> M Glyph
lookupChar (x, ys) c = do
  n <- cast c
  i <- sub n 32
  a <- ix x i >>= load
  bs <- mapM (\y -> ix y i >>= load) ys
  return (a, bs)

metrics :: [((FilePath, Int32), (Int32, [(Char, ((Int32, Int32), (Int32, Int32), Int32), (FilePath, (Int32, Int32)))]))]
metrics = [(("LuckiestGuy.ttf",36), (23,[
      (' ', ((0, 0), (0, 0), 6),("LuckiestGuy.ttf.36.32.rgba", (6, 33)))
    , ('!', ((0, 9), (-2, 24), 9),("LuckiestGuy.ttf.36.33.rgba", (9, 33)))
    , ('"', ((0, 16), (14, 26), 15),("LuckiestGuy.ttf.36.34.rgba", (16, 33)))
    , ('#', ((0, 19), (2, 20), 19),("LuckiestGuy.ttf.36.35.rgba", (19, 33)))
    , ('$', ((0, 14), (-3, 25), 14),("LuckiestGuy.ttf.36.36.rgba", (14, 33)))
    , ('%', ((0, 23), (1, 21), 23),("LuckiestGuy.ttf.36.37.rgba", (23, 33)))
    , ('&', ((0, 21), (0, 22), 20),("LuckiestGuy.ttf.36.38.rgba", (21, 33)))
    , ('\'', ((0, 8), (15, 26), 7),("LuckiestGuy.ttf.36.39.rgba", (8, 33)))
    , ('(', ((0, 14), (-3, 25), 12),("LuckiestGuy.ttf.36.40.rgba", (14, 33)))
    , (')', ((-1, 12), (-3, 25), 12),("LuckiestGuy.ttf.36.41.rgba", (13, 33)))
    , ('*', ((0, 17), (7, 23), 17),("LuckiestGuy.ttf.36.42.rgba", (17, 33)))
    , ('+', ((0, 14), (3, 17), 15),("LuckiestGuy.ttf.36.43.rgba", (15, 33)))
    , (',', ((0, 7), (-3, 7), 7),("LuckiestGuy.ttf.36.44.rgba", (7, 33)))
    , ('-', ((1, 12), (8, 14), 12),("LuckiestGuy.ttf.36.45.rgba", (12, 33)))
    , ('.', ((0, 7), (-1, 7), 7),("LuckiestGuy.ttf.36.46.rgba", (7, 33)))
    , ('/', ((0, 16), (-2, 24), 16),("LuckiestGuy.ttf.36.47.rgba", (16, 33)))
    , ('0', ((0, 20), (-1, 23), 20),("LuckiestGuy.ttf.36.48.rgba", (20, 33)))
    , ('1', ((-1, 12), (0, 23), 12),("LuckiestGuy.ttf.36.49.rgba", (13, 33)))
    , ('2', ((0, 16), (0, 24), 16),("LuckiestGuy.ttf.36.50.rgba", (16, 33)))
    , ('3', ((0, 17), (-1, 24), 17),("LuckiestGuy.ttf.36.51.rgba", (17, 33)))
    , ('4', ((0, 17), (0, 24), 17),("LuckiestGuy.ttf.36.52.rgba", (17, 33)))
    , ('5', ((0, 17), (-1, 23), 17),("LuckiestGuy.ttf.36.53.rgba", (17, 33)))
    , ('6', ((0, 19), (-1, 24), 18),("LuckiestGuy.ttf.36.54.rgba", (19, 33)))
    , ('7', ((0, 16), (0, 23), 16),("LuckiestGuy.ttf.36.55.rgba", (16, 33)))
    , ('8', ((0, 18), (-1, 23), 18),("LuckiestGuy.ttf.36.56.rgba", (18, 33)))
    , ('9', ((0, 18), (0, 23), 18),("LuckiestGuy.ttf.36.57.rgba", (18, 33)))
    , (':', ((0, 8), (-1, 17), 8),("LuckiestGuy.ttf.36.58.rgba", (8, 33)))
    , (';', ((0, 8), (-3, 17), 8),("LuckiestGuy.ttf.36.59.rgba", (8, 33)))
    , ('<', ((0, 14), (1, 21), 15),("LuckiestGuy.ttf.36.60.rgba", (15, 33)))
    , ('=', ((1, 12), (3, 16), 13),("LuckiestGuy.ttf.36.61.rgba", (13, 33)))
    , ('>', ((0, 14), (1, 21), 15),("LuckiestGuy.ttf.36.62.rgba", (15, 33)))
    , ('?', ((0, 18), (-2, 23), 18),("LuckiestGuy.ttf.36.63.rgba", (18, 33)))
    , ('@', ((1, 21), (1, 21), 21),("LuckiestGuy.ttf.36.64.rgba", (21, 33)))
    , ('A', ((-1, 21), (0, 23), 20),("LuckiestGuy.ttf.36.65.rgba", (22, 33)))
    , ('B', ((0, 19), (-1, 23), 19),("LuckiestGuy.ttf.36.66.rgba", (19, 33)))
    , ('C', ((0, 17), (0, 23), 16),("LuckiestGuy.ttf.36.67.rgba", (17, 33)))
    , ('D', ((0, 19), (0, 23), 19),("LuckiestGuy.ttf.36.68.rgba", (19, 33)))
    , ('E', ((0, 16), (0, 23), 15),("LuckiestGuy.ttf.36.69.rgba", (16, 33)))
    , ('F', ((0, 16), (0, 23), 16),("LuckiestGuy.ttf.36.70.rgba", (16, 33)))
    , ('G', ((0, 20), (-1, 24), 20),("LuckiestGuy.ttf.36.71.rgba", (20, 33)))
    , ('H', ((0, 20), (-1, 23), 20),("LuckiestGuy.ttf.36.72.rgba", (20, 33)))
    , ('I', ((0, 10), (0, 22), 10),("LuckiestGuy.ttf.36.73.rgba", (10, 33)))
    , ('J', ((-1, 16), (-1, 23), 16),("LuckiestGuy.ttf.36.74.rgba", (17, 33)))
    , ('K', ((0, 21), (-1, 23), 20),("LuckiestGuy.ttf.36.75.rgba", (21, 33)))
    , ('L', ((0, 15), (0, 23), 14),("LuckiestGuy.ttf.36.76.rgba", (15, 33)))
    , ('M', ((0, 25), (-1, 23), 25),("LuckiestGuy.ttf.36.77.rgba", (25, 33)))
    , ('N', ((0, 23), (0, 23), 23),("LuckiestGuy.ttf.36.78.rgba", (23, 33)))
    , ('O', ((0, 21), (0, 22), 20),("LuckiestGuy.ttf.36.79.rgba", (21, 33)))
    , ('P', ((0, 19), (-1, 23), 19),("LuckiestGuy.ttf.36.80.rgba", (19, 33)))
    , ('Q', ((0, 22), (-3, 23), 22),("LuckiestGuy.ttf.36.81.rgba", (22, 33)))
    , ('R', ((0, 20), (0, 23), 19),("LuckiestGuy.ttf.36.82.rgba", (20, 33)))
    , ('S', ((0, 17), (-1, 24), 17),("LuckiestGuy.ttf.36.83.rgba", (17, 33)))
    , ('T', ((0, 18), (0, 23), 17),("LuckiestGuy.ttf.36.84.rgba", (18, 33)))
    , ('U', ((0, 20), (-1, 22), 20),("LuckiestGuy.ttf.36.85.rgba", (20, 33)))
    , ('V', ((-1, 20), (-1, 23), 20),("LuckiestGuy.ttf.36.86.rgba", (21, 33)))
    , ('W', ((0, 29), (-1, 23), 29),("LuckiestGuy.ttf.36.87.rgba", (29, 33)))
    , ('X', ((-1, 20), (0, 23), 19),("LuckiestGuy.ttf.36.88.rgba", (21, 33)))
    , ('Y', ((0, 21), (-1, 22), 19),("LuckiestGuy.ttf.36.89.rgba", (21, 33)))
    , ('Z', ((0, 16), (0, 23), 16),("LuckiestGuy.ttf.36.90.rgba", (16, 33)))
    , ('[', ((0, 12), (-3, 25), 12),("LuckiestGuy.ttf.36.91.rgba", (12, 33)))
    , ('\\', ((0, 16), (-2, 24), 16),("LuckiestGuy.ttf.36.92.rgba", (16, 33)))
    , (']', ((-1, 11), (-3, 25), 12),("LuckiestGuy.ttf.36.93.rgba", (13, 33)))
    , ('^', ((0, 16), (10, 23), 16),("LuckiestGuy.ttf.36.94.rgba", (16, 33)))
    , ('_', ((-1, 10), (-7, -1), 10),("LuckiestGuy.ttf.36.95.rgba", (11, 33)))
    , ('`', ((0, 10), (21, 30), 9),("LuckiestGuy.ttf.36.96.rgba", (10, 33)))
    , ('a', ((-1, 21), (0, 23), 20),("LuckiestGuy.ttf.36.97.rgba", (22, 33)))
    , ('b', ((0, 19), (-1, 23), 19),("LuckiestGuy.ttf.36.98.rgba", (19, 33)))
    , ('c', ((0, 17), (-1, 23), 16),("LuckiestGuy.ttf.36.99.rgba", (17, 33)))
    , ('d', ((0, 19), (0, 22), 19),("LuckiestGuy.ttf.36.100.rgba", (19, 33)))
    , ('e', ((0, 19), (-1, 23), 18),("LuckiestGuy.ttf.36.101.rgba", (19, 33)))
    , ('f', ((0, 16), (0, 23), 16),("LuckiestGuy.ttf.36.102.rgba", (16, 33)))
    , ('g', ((0, 20), (-1, 23), 20),("LuckiestGuy.ttf.36.103.rgba", (20, 33)))
    , ('h', ((0, 20), (0, 23), 20),("LuckiestGuy.ttf.36.104.rgba", (20, 33)))
    , ('i', ((0, 9), (0, 22), 9),("LuckiestGuy.ttf.36.105.rgba", (9, 33)))
    , ('j', ((-1, 16), (-1, 23), 16),("LuckiestGuy.ttf.36.106.rgba", (17, 33)))
    , ('k', ((0, 20), (-1, 23), 19),("LuckiestGuy.ttf.36.107.rgba", (20, 33)))
    , ('l', ((0, 15), (0, 23), 14),("LuckiestGuy.ttf.36.108.rgba", (15, 33)))
    , ('m', ((0, 29), (0, 23), 29),("LuckiestGuy.ttf.36.109.rgba", (29, 33)))
    , ('n', ((0, 21), (0, 23), 21),("LuckiestGuy.ttf.36.110.rgba", (21, 33)))
    , ('o', ((0, 21), (1, 22), 20),("LuckiestGuy.ttf.36.111.rgba", (21, 33)))
    , ('p', ((0, 19), (-1, 23), 19),("LuckiestGuy.ttf.36.112.rgba", (19, 33)))
    , ('q', ((0, 22), (-3, 23), 22),("LuckiestGuy.ttf.36.113.rgba", (22, 33)))
    , ('r', ((0, 20), (0, 23), 19),("LuckiestGuy.ttf.36.114.rgba", (20, 33)))
    , ('s', ((0, 17), (-1, 23), 17),("LuckiestGuy.ttf.36.115.rgba", (17, 33)))
    , ('t', ((0, 18), (0, 23), 17),("LuckiestGuy.ttf.36.116.rgba", (18, 33)))
    , ('u', ((0, 20), (-1, 22), 20),("LuckiestGuy.ttf.36.117.rgba", (20, 33)))
    , ('v', ((-1, 20), (0, 23), 20),("LuckiestGuy.ttf.36.118.rgba", (21, 33)))
    , ('w', ((0, 30), (0, 23), 29),("LuckiestGuy.ttf.36.119.rgba", (30, 33)))
    , ('x', ((-1, 20), (0, 23), 19),("LuckiestGuy.ttf.36.120.rgba", (21, 33)))
    , ('y', ((-1, 21), (0, 22), 19),("LuckiestGuy.ttf.36.121.rgba", (22, 33)))
    , ('z', ((0, 16), (0, 23), 16),("LuckiestGuy.ttf.36.122.rgba", (16, 33)))
    , ('{', ((-1, 14), (-3, 25), 13),("LuckiestGuy.ttf.36.123.rgba", (15, 33)))
    , ('|', ((1, 9), (-3, 25), 9),("LuckiestGuy.ttf.36.124.rgba", (9, 33)))
    , ('}', ((-2, 13), (-3, 25), 13),("LuckiestGuy.ttf.36.125.rgba", (15, 33)))
    , ('~', ((0, 19), (6, 18), 18),("LuckiestGuy.ttf.36.126.rgba", (19, 33)))
    ]))
  ]
{-
module Main where

import Codec.Picture
import Data.Vector.Binary
import qualified Data.ByteString as B
import Data.Binary.Put
import Data.Binary

main :: IO ()
main = toRGBA "ship.bmp"

toRGBA fn0 = do
  ImageRGBA8 img <- either error id <$> readImage fn0
  let fn = fn0 ++ ".rgba"
  encodeFile fn $ imageData img
  print (fn, (imageWidth img, imageHeight img))
-}
