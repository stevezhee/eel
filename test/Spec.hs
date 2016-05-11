{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -w #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Prelude hiding (div, and)
import Eel hiding (mainM, cast, lit, Ptr, ffi, binop)
import qualified Eel as E
import Control.Monad hiding (when)

type Int' = I Int32
type Word' = I Word32
type Double' = I Double
type Word64' = I Word64
type Bool' = I Bool
type Char' = I Char
type Float' = I Float
type Ptr a = V (E.Ptr a)
type String' = Ptr Char
type Array a = Ptr a
type Font = (Array (E.Ptr Texture), [Array Int32])
type Glyph = (Tex, [Int'])
type Tex = (Ptr Texture, (Int', Int'))
  
sputf :: Ptr Char -> Float' -> M ()
sputf p = ffi "sputf" (return p :: M (Ptr Char))

sputl :: Ptr Char -> Word64' -> M ()
sputl p = ffi "sputl" (return p :: M (Ptr Char))

putu :: Word' -> M ()
putu = ffi "putu"
puti :: Int' -> M ()
puti = ffi "puti"
putf :: Float' -> M ()
putf = ffi "putf"
putd :: Double' -> M ()
putd = ffi "putd"
putl :: Word64' -> M ()
putl = ffi "putl"
putb :: Bool' -> M ()
putb = ffi "putb"
putc :: Char' -> M ()
putc = ffi "putcchar"
puts :: String' -> M ()
puts = E.ffi "putcstr"
putp :: Ty a => Ptr a -> M ()
putp = E.ffi "putp"
poll_sdl :: Int'
poll_sdl = ffi "poll_sdl" ()
cleanup_sdl :: Int' -> M ()
cleanup_sdl = ffi "cleanup_sdl"

load_rgba :: (String', (V Int32, V Int32)) -> M (Ptr Texture)
load_rgba = E.ffi "load_rgba"
                                    
present_sdl :: M ()
present_sdl = ffi "present_sdl" ()

clear_sdl :: M ()
clear_sdl = ffi "clear_sdl" ()

sdl_getperformancecounter :: Word64'
sdl_getperformancecounter = ffi "SDL_GetPerformanceCounter" ()

sdl_getperformancefrequency :: Word64'
sdl_getperformancefrequency = ffi "SDL_GetPerformanceFrequency" ()

true :: Bool'
true = lit True
false :: Bool'
false = lit False

allocn :: Ty a => Int' -> M (Ptr a)
allocn = fun alloca

mainM :: M () -> IO ()
mainM m = E.mainM $ \(_argc, _argv) -> m >> lit 0
  
while :: Bool' -> M ()
while x = do
  start_lbl <- newLabel
  done_lbl <- newLabel
  br' start_lbl
  block start_lbl
  a <- x
  br a start_lbl done_lbl
  block done_lbl

oneof :: Ret r => M r -> [(Bool', M r)] -> M r
oneof = foldr (\(r,a) b -> if' r a b)

lit :: Lit a => a -> I a
lit = return . E.lit

when :: Bool' -> M () -> M ()
when x y = if' x y (return ())

switch :: (Cmp a, Ret r) => I a -> (I a -> M r) -> [(I a, M r)] -> M r
switch x f zs = do
  a <- let' x
  oneof (f a) [ (a ==. e, r) | (e, r) <- zs ]

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

data Texture
instance Ty Texture where ty _ = ty (unused "Texture" :: Int')

blit :: Tex -> Int' -> Int' -> Float' -> M ()
blit (t,(w,h)) = ffi "blit" (return t :: M (Ptr Texture)) w h

sdl_delay :: Int' -> M ()
sdl_delay = ffi "SDL_Delay"

cstring :: String -> M (Ptr Char)
cstring s = newn $ fmap E.lit $ s ++ ['\0']

new :: Ty a => I a -> M (Ptr a)
new x = do
  p <- alloca (E.lit 1 :: V Int32)
  p <. x
  return p

newn :: Ty a => [V a] -> M (Ptr a)
newn xs = do
  p <- allocn $ fromIntegral $ length xs
  sequence_ [ store x =<< ix p (lit i) | (i,x) <- zip [0 ..] xs ]
  return p

ix :: Ty a => Array a -> Int' -> M (Ptr a)
ix arr = fun (getelementptr arr)

modify :: Ty a => Ptr a -> (I a -> I a) -> I a
modify p f = do
  a <- f $ load p
  store a p
  return a

load_tex :: FilePath -> M Tex
load_tex fn = do
  let sz@(w,h) = maybe (error $ "unknown texture:" ++ show fn) id $ lookup fn textures
  a <- w
  b <- h
  tex <- cstring fn >>= \s -> load_rgba (s, (a,b))
  return (tex, sz)

loadFont :: String -> Int32 -> M Font
loadFont nm pt = do
  let fid = (nm,pt)
  let chs =
        maybe (error $ "unknown font:" ++ show fid) id $ lookup fid metrics
  let n = fromIntegral $ length chs
  tex_arr <- allocn n
  gs@[w_arr, h_arr, left_arr, top_arr, advance_arr] <-
    sequence $ replicate 5 $ allocn n
  let loadGlyph (i, (_, left, top, adv, (fn, _))) = do
        let upd arr a = ix arr (fromIntegral i) >>= \i -> i <. a
        (t, (w,h)) <- load_tex fn
        upd tex_arr $ return t
        upd w_arr w
        upd h_arr h
        upd left_arr left
        upd top_arr top
        upd advance_arr adv
  mapM_ loadGlyph $ zip [0 .. ] chs
  return (tex_arr, gs)

inc :: (Arith a, Lit a, Num a) => Ptr a -> I a
inc p = modify p $ (+) 1
dec :: (Arith a, Lit a, Num a) => Ptr a -> I a
dec p = modify p $ flip (-) 1

blitString :: Font -> String' -> Int' -> Int' -> M ()
blitString fnt s x0 y = do
  pi <- new 0
  px <- new x0
  while $ do
    c <- let' (loadix s (inc pi))
    r <- let' (c /=. (lit '\0'))
    when r $ do
      void $ modify px $ \x -> do
        (tex, [ left, top, advance]) <- lookupChar fnt c
        blit tex (left + x) (top + y) 0
        advance + x
    r

instance (Arith a, Lit a, Num a) => Num (I a) where
  fromInteger = lit . fromInteger
  (+) = binop add
  (-) = binop sub
  (*) = binop mul

(==.) :: Cmp a => I a -> I a -> Bool'
(==.) = binop eq

binop f = fun (uncurry f)

(/=.) :: Cmp a => I a -> I a -> Bool'
(/=.) = binop ne

instance Fractional Float' where
  fromRational = lit . fromRational
  (/) = binop div

init_sdl :: Int' -> Int' -> Int' -> Int' -> M ()
init_sdl = ffi "init_sdl"

class Fun f g | f -> g where fun :: g -> f
 
instance (Ret r) => Fun (() -> M r) (() -> M r) where
  fun = id
 
instance (Ty a, Ret r) => Fun (I a -> M r) (V a -> M r) where
  fun = joinF

pair :: Applicative f => f a -> f b -> f (a, b)
pair x y = (,) <$> x <*> y

joinF f a = join (f <$> a)

instance (Ty a, Ty b, Ret r) => Fun (I a -> I b -> M r) ((V a, V b) -> M r) where
  fun o = \a b -> joinF o (pair a b)

instance (Ty a, Ty b, Ty c, Ret r) =>
  Fun (I a -> I b -> I c -> M r) (((V a, V b), V c) -> M r) where
  fun o = \a b c -> joinF o (pair (pair a b) c)

instance (Ty a, Ty b, Ty c, Ty d, Ret r) =>
  Fun (I a -> I b -> I c -> I d -> M r) (((V a, V b), (V c, V d)) -> M r) where
  fun o = \a b c d -> joinF o (pair (pair a b) (pair c d))

instance (Ty a, Ty b, Ty c, Ty d, Ty e, Ret r) =>
  Fun (I a -> I b -> I c -> I d -> I e -> M r)
  ((((V a, V b), (V c, V d)), V e) -> M r) where
  fun o = \a b c d e -> joinF o (pair (pair (pair a b) (pair c d)) e)

instance (Ty a, Ty b, Ty c, Ty d, Ty e, Ty f, Ret r) =>
  Fun (I a -> I b -> I c -> I d -> I e -> I f -> M r)
  ((((V a, V b), (V c, V d)), (V e, V f)) -> M r) where
  fun o = \a b c d e f -> joinF o (pair (pair (pair a b) (pair c d)) (pair e f))

ffi :: (Ret r, Args a, Fun c (a -> M r)) => String -> c
ffi = fun . E.ffi

expr :: (Lit a, Fun a g) => g -> I a
expr = lit . fun

set_color :: Int' -> Int' -> Int' -> Int' -> M ()
set_color = ffi "set_color"

cast :: (Cast a, Cast b) => I a -> I b
cast = fun E.cast

(<.) :: Ty a => Ptr a -> I a -> M ()
(<.) p = fun (flip store p)

(+=) x = modify x . (+)
(-=) x = modify x . (-)

let' :: Ty a => I a -> M (I a)
let' m = m >>= \a -> return (return a)

main :: IO ()
main = mainM $ do
  outbuf <- allocn 1024
  vx <- new 0
  vr <- new 0
  t0 <- new 0
  t1 <- new 0
  x <- new 100
  r <- new 90
  init_sdl 30 550 1280 256
  font <- loadFont "LuckiestGuy.ttf" 36
  tex <- load_tex "ship.png.rgba"
  set_color 0 0x30 0 0xff
  perf_freq <- let' $ cast sdl_getperformancefrequency
  -- game loop
  while $ do
    -- update state
    x += load vx
    r += load vr
    -- render screen
    clear_sdl
    -- draw ship
    blit tex (load x) 100 (load r)
    -- compute frames per second
    t0 <. sdl_getperformancecounter
    sputf outbuf (perf_freq / cast (load t0 - load t1))
    t1 <. sdl_getperformancecounter
    blitString font outbuf 10 140
    present_sdl
    -- sleep
    sdl_delay 14
    -- handle input
    switch poll_sdl
      (\_ -> true)
      [ (sdl_quit, false)
      , (sdlk_up, inc vx >> true)
      , (sdlk_down, dec vx >> true)
      , (sdlk_left, dec vr >> true)
      , (sdlk_right, inc vr >> true)
      ]
  cleanup_sdl 0

loadix x i = ix x i >>= load

lookupChar :: Font -> Char' -> M Glyph
lookupChar (x, ys) c = do
  i <- let' (cast c - 32)
  t <- loadix x i
  (w:h:bs) <- mapM (flip loadix i) ys
  return ((t,(return w, return h)), fmap return bs)
  
textures = [("ship.png.rgba", (64,56))] ++ [ a | (_,_,_,_,a) <- concatMap snd metrics ]

metrics :: [((FilePath, Int32),[(Char, Int', Int', Int', (FilePath, (Int', Int')))])]
metrics =
  [(("LuckiestGuy.ttf",36),[(' ',0,44,9,("LuckiestGuy.ttf.36.32.rgba",(0,0))),('!',1,9,13,("LuckiestGuy.ttf.36.33.rgba",(12,37))),('"',0,6,24,("LuckiestGuy.ttf.36.34.rgba",(24,17))),('#',0,14,29,("LuckiestGuy.ttf.36.35.rgba",(28,27))),('$',1,7,21,("LuckiestGuy.ttf.36.36.rgba",(19,41))),('%',0,13,35,("LuckiestGuy.ttf.36.37.rgba",(34,29))),('&',0,11,31,("LuckiestGuy.ttf.36.38.rgba",(31,33))),('\'',0,6,12,("LuckiestGuy.ttf.36.39.rgba",(12,15))),('(',0,7,20,("LuckiestGuy.ttf.36.40.rgba",(20,41))),(')',-2,7,18,("LuckiestGuy.ttf.36.41.rgba",(20,41))),('*',0,9,26,("LuckiestGuy.ttf.36.42.rgba",(25,25))),('+',1,18,22,("LuckiestGuy.ttf.36.43.rgba",(20,21))),(',',0,33,11,("LuckiestGuy.ttf.36.44.rgba",(11,16))),('-',1,23,18,("LuckiestGuy.ttf.36.45.rgba",(16,9))),('.',0,34,11,("LuckiestGuy.ttf.36.46.rgba",(11,11))),('/',0,8,24,("LuckiestGuy.ttf.36.47.rgba",(24,38))),('0',0,10,30,("LuckiestGuy.ttf.36.48.rgba",(30,35))),('1',-1,9,19,("LuckiestGuy.ttf.36.49.rgba",(19,35))),('2',0,8,24,("LuckiestGuy.ttf.36.50.rgba",(24,36))),('3',0,8,25,("LuckiestGuy.ttf.36.51.rgba",(25,37))),('4',0,9,26,("LuckiestGuy.ttf.36.52.rgba",(26,35))),('5',0,9,25,("LuckiestGuy.ttf.36.53.rgba",(25,36))),('6',0,9,28,("LuckiestGuy.ttf.36.54.rgba",(28,36))),('7',0,9,24,("LuckiestGuy.ttf.36.55.rgba",(24,35))),('8',0,9,27,("LuckiestGuy.ttf.36.56.rgba",(27,36))),('9',0,9,27,("LuckiestGuy.ttf.36.57.rgba",(26,35))),(':',1,19,12,("LuckiestGuy.ttf.36.58.rgba",(10,26))),(';',0,19,12,("LuckiestGuy.ttf.36.59.rgba",(11,30))),('<',1,13,22,("LuckiestGuy.ttf.36.60.rgba",(20,29))),('=',1,20,19,("LuckiestGuy.ttf.36.61.rgba",(16,19))),('>',1,13,22,("LuckiestGuy.ttf.36.62.rgba",(20,29))),('?',0,9,27,("LuckiestGuy.ttf.36.63.rgba",(26,37))),('@',1,13,32,("LuckiestGuy.ttf.36.64.rgba",(30,29))),('A',-1,10,31,("LuckiestGuy.ttf.36.65.rgba",(32,34))),('B',1,9,29,("LuckiestGuy.ttf.36.66.rgba",(28,36))),('C',0,9,25,("LuckiestGuy.ttf.36.67.rgba",(25,35))),('D',1,10,28,("LuckiestGuy.ttf.36.68.rgba",(27,34))),('E',1,10,24,("LuckiestGuy.ttf.36.69.rgba",(23,34))),('F',1,10,24,("LuckiestGuy.ttf.36.70.rgba",(23,34))),('G',0,9,30,("LuckiestGuy.ttf.36.71.rgba",(30,36))),('H',0,10,30,("LuckiestGuy.ttf.36.72.rgba",(30,35))),('I',1,11,14,("LuckiestGuy.ttf.36.73.rgba",(13,33))),('J',-1,10,24,("LuckiestGuy.ttf.36.74.rgba",(25,35))),('K',1,9,31,("LuckiestGuy.ttf.36.75.rgba",(30,37))),('L',1,10,22,("LuckiestGuy.ttf.36.76.rgba",(21,33))),('M',1,9,38,("LuckiestGuy.ttf.36.77.rgba",(37,36))),('N',0,9,34,("LuckiestGuy.ttf.36.78.rgba",(34,35))),('O',0,12,31,("LuckiestGuy.ttf.36.79.rgba",(31,32))),('P',1,9,29,("LuckiestGuy.ttf.36.80.rgba",(28,36))),('Q',0,10,33,("LuckiestGuy.ttf.36.81.rgba",(33,39))),('R',1,9,29,("LuckiestGuy.ttf.36.82.rgba",(28,35))),('S',0,9,26,("LuckiestGuy.ttf.36.83.rgba",(26,36))),('T',0,10,26,("LuckiestGuy.ttf.36.84.rgba",(26,34))),('U',0,11,30,("LuckiestGuy.ttf.36.85.rgba",(30,35))),('V',-1,10,30,("LuckiestGuy.ttf.36.86.rgba",(31,35))),('W',0,10,44,("LuckiestGuy.ttf.36.87.rgba",(44,35))),('X',-2,9,30,("LuckiestGuy.ttf.36.88.rgba",(32,35))),('Y',0,11,32,("LuckiestGuy.ttf.36.89.rgba",(32,34))),('Z',0,9,23,("LuckiestGuy.ttf.36.90.rgba",(23,35))),('[',1,7,18,("LuckiestGuy.ttf.36.91.rgba",(17,41))),('\\',1,8,24,("LuckiestGuy.ttf.36.92.rgba",(23,38))),(']',-1,7,18,("LuckiestGuy.ttf.36.93.rgba",(18,41))),('^',0,10,24,("LuckiestGuy.ttf.36.94.rgba",(24,19))),('_',-1,46,15,("LuckiestGuy.ttf.36.95.rgba",(16,9))),('`',0,-1,14,("LuckiestGuy.ttf.36.96.rgba",(14,13))),('a',-1,10,31,("LuckiestGuy.ttf.36.97.rgba",(32,34))),('b',1,9,29,("LuckiestGuy.ttf.36.98.rgba",(28,36))),('c',0,10,25,("LuckiestGuy.ttf.36.99.rgba",(25,35))),('d',1,11,28,("LuckiestGuy.ttf.36.100.rgba",(27,33))),('e',0,10,28,("LuckiestGuy.ttf.36.101.rgba",(28,35))),('f',1,10,24,("LuckiestGuy.ttf.36.102.rgba",(23,34))),('g',0,9,30,("LuckiestGuy.ttf.36.103.rgba",(30,36))),('h',0,10,30,("LuckiestGuy.ttf.36.104.rgba",(30,34))),('i',0,11,14,("LuckiestGuy.ttf.36.105.rgba",(14,33))),('j',-1,10,24,("LuckiestGuy.ttf.36.106.rgba",(25,35))),('k',1,10,30,("LuckiestGuy.ttf.36.107.rgba",(29,35))),('l',1,10,22,("LuckiestGuy.ttf.36.108.rgba",(21,34))),('m',0,10,43,("LuckiestGuy.ttf.36.109.rgba",(43,34))),('n',0,9,31,("LuckiestGuy.ttf.36.110.rgba",(31,35))),('o',0,11,31,("LuckiestGuy.ttf.36.111.rgba",(31,31))),('p',1,10,29,("LuckiestGuy.ttf.36.112.rgba",(28,35))),('q',0,10,33,("LuckiestGuy.ttf.36.113.rgba",(33,39))),('r',1,9,29,("LuckiestGuy.ttf.36.114.rgba",(28,35))),('s',0,10,26,("LuckiestGuy.ttf.36.115.rgba",(26,35))),('t',0,10,27,("LuckiestGuy.ttf.36.116.rgba",(27,34))),('u',0,11,30,("LuckiestGuy.ttf.36.117.rgba",(30,34))),('v',-1,10,30,("LuckiestGuy.ttf.36.118.rgba",(31,34))),('w',0,10,44,("LuckiestGuy.ttf.36.119.rgba",(44,34))),('x',-1,10,30,("LuckiestGuy.ttf.36.120.rgba",(31,34))),('y',-1,11,31,("LuckiestGuy.ttf.36.121.rgba",(32,33))),('z',0,10,24,("LuckiestGuy.ttf.36.122.rgba",(24,34))),('{',-1,6,21,("LuckiestGuy.ttf.36.123.rgba",(22,43))),('|',1,7,14,("LuckiestGuy.ttf.36.124.rgba",(12,41))),('}',-2,6,20,("LuckiestGuy.ttf.36.125.rgba",(22,43))),('~',0,17,28,("LuckiestGuy.ttf.36.126.rgba",(28,18)))])]
