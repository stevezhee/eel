{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : CEel
Description : DSL
Copyright   : Brett Letner, 2016
License     : BSD3
Maintainer  : 
Stability   : experimental
Portability : 

The Eel EDSL is an Embedded Domain Specific Language intended for
embedded (resource constrained) development.  This is the C-like
version of the language.  It should provide roughly the same
expressivity as C (with macros).

-}

module CEel
( module CEel
, module Eel
)
  
where

import Eel hiding (div, rem, binop, lit, alloca, mainM, cast)
import qualified Eel as E
import Control.Monad hiding (when)

type Int' = I Int32
type Word' = I Word32
type Double' = I Double
type Word64' = I Word64
type Bool' = I Bool
type Char' = I Char
type Float' = I Float
type Ptr' a = V (Ptr a)
type String' = Ptr' Char
type Array a = Ptr' a

lit :: Lit a => a -> I a
lit = return . E.lit

unop :: (Ty a) => (V a -> M b) -> I a -> M b
unop f x = x >>= f

binop :: (Ty a, Ty b) => (V a -> V b -> M c) -> I a -> I b -> M c
binop f x y = join (f <$> x <*> y)

instance (Arith a, Num a) => Num (I a) where
  fromInteger = lit . fromInteger
  (+) = binop add
  (-) = binop sub
  (*) = binop mul
  signum = undefined
  abs = undefined
  
(==.) :: Cmp a => I a -> I a -> Bool'
(==.) = binop eq
(/=.) :: Cmp a => I a -> I a -> Bool'
(/=.) = binop ne
(<.) :: Cmp a => I a -> I a -> Bool'
(<.) = binop lt
(>.) :: Cmp a => I a -> I a -> Bool'
(>.) = binop gt
(>=.) :: Cmp a => I a -> I a -> Bool'
(>=.) = binop ge
(<=.) :: Cmp a => I a -> I a -> Bool'
(<=.) = binop le

instance (Arith a, Fractional a) => Fractional (I a) where
  fromRational = lit . fromRational
  (/) = binop E.div

instance (Arith a, Enum a) => Enum (I a) where
  toEnum = lit . toEnum
  fromEnum = unused "fromEnum"
  
instance (Arith a, Ord a) => Ord (I a) where compare = unused "compare"
  
instance (Arith a, Eq a) => Eq (I a) where (==) = unused "=="
  
instance (Arith a, Real a) => Real (I a) where toRational = unused "toRational"
instance (Arith a, Integral a) => Integral (I a) where
  div = binop E.div
  rem = binop E.rem
  quotRem = unused "quotRem"
  toInteger = unused "toInteger"
  
(<-.) :: Ty a => Ptr' a -> I a -> M ()
(<-.) p = unop (flip store p)

(+=) :: (Num a, Arith a) => Ptr' a -> I a -> M ()
(+=) x = modify x . (+)
(-=) :: (Num a, Arith a) => Ptr' a -> I a -> M ()
(-=) x = modify x . flip (-)
(*=) :: (Num a, Arith a) => Ptr' a -> I a -> M ()
(*=) x = modify x . (+)

inc :: (Arith a, Num a) => Ptr' a -> M ()
inc p = p += 1
dec :: (Arith a, Num a) => Ptr' a -> M ()
dec p = p -= 1

new :: Ty a => I a -> M (Ptr' a)
new x = do
  p <- allocn 1
  p <-. x
  return p

ix :: Ty a => Array a -> Int' -> M (Ptr' a)
ix arr = unop (getelementptr arr)

allocn :: Ty a => Int' -> M (Ptr' a)
allocn = unop E.alloca

newn :: Ty a => [V a] -> M (Ptr' a)
newn xs = do
  p <- allocn $ fromIntegral $ length xs
  sequence_ [ store x =<< ix p (lit i) | (i,x) <- zip [0 ..] xs ]
  return p

modify :: Ty a => Ptr' a -> (I a -> I a) -> M ()
modify p f = do
  a <- f $ load p
  store a p

cstring :: String -> M (Ptr' Char)
cstring s = newn $ fmap E.lit $ s ++ ['\0']

true :: Bool'
true = lit True

false :: Bool'
false = lit False

cast :: (Cast a, Cast b) => I a -> I b
cast = unop E.cast

let' :: Ty a => I a -> M (I a)
let' m = return <$> m

-- | 'if' function (works for both expressions and statements).
if' :: Phi r => Bool' -> M r -> M r -> M r
if' x y z = do
  a <- x
  true_lbl <- newLabel
  false_lbl <- newLabel
  done_lbl <- newLabel
  br a true_lbl false_lbl
  block true_lbl
  b <- y
  phi_true_lbl <- currBlock
  br' done_lbl
  block false_lbl
  c <- z
  phi_false_lbl <- currBlock
  br' done_lbl
  block done_lbl
  phi [(b, phi_true_lbl), (c, phi_false_lbl)]

while :: Bool' -> M ()
while x = do
  start_lbl <- newLabel
  done_lbl <- newLabel
  br' start_lbl
  block start_lbl
  a <- x
  br a start_lbl done_lbl
  block done_lbl

oneof :: Phi r => M r -> [(Bool', M r)] -> M r
oneof = foldr (\(r,a) b -> if' r a b)

when :: Bool' -> M () -> M ()
when x y = if' x y (return ())

switch :: (Cmp a, Phi r) => I a -> (I a -> M r) -> [(I a, M r)] -> M r
switch x f zs = do
  a <- let' x
  oneof (f a) [ (a ==. e, r) | (e, r) <- zs ]

mainM :: String -> (I Int32 -> Ptr' (Ptr Char) -> I Int32) -> IO ()
mainM fn f = E.mainM fn $ \argc argv -> f (return argc) argv

sputf :: Ptr' Char -> Float' -> M ()
sputf = declare "sputf"
sputl :: Ptr' Char -> Word64' -> M ()
sputl = declare "sputl"
putu :: Word' -> M ()
putu = declare "putu"
puti :: Int' -> M ()
puti = declare "puti"
putf :: Float' -> M ()
putf = declare "putf"
putd :: Double' -> M ()
putd = declare "putd"
putl :: Word64' -> M ()
putl = declare "putl"
putb :: Bool' -> M ()
putb = declare "putb"
putc :: Char' -> M ()
putc = declare "putcchar"
puts :: String' -> M ()
puts = declare "putcstr"
putp :: Ty a => Ptr' a -> M ()
putp = declare "putp"
