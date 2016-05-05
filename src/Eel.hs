{-# OPTIONS_GHC -Wall #-}
{-|
Module      : Eel
Description : Eel EDSL.
Copyright   : Brett Letner, 2016
License     : zlib
Maintainer  : 
Stability   : experimental
Portability : 

The Eel EDSL is an Embedded Domain Specific Language intended for embedded (resource constrained) development.

-}

module Eel

where

import Data.Int
import Data.Word
import Data.Unique

-- | Quick and dirty representation of LLVM values.
type Value = String
-- | Quick and dirty representation of LLVM types.
type Type = String

-- | Representation for a value in our EDSL.
-- This datatype allows us to leverage the Haskell compiler's type inference
-- for our EDSL.
-- Datatypes such as this are called phantom types.
data V a = V{ valof :: Value, tyof :: Type }

-- | Helper function for use when you want to show both the type and
-- the value, e.g. i32 7.
tyvalof :: Ty a => V a -> String
tyvalof x = unwords [ tyof x, valof x ]

-- | Right now we are simply going to output strings directly to the
-- console but later we'll probably want to add some state so let's
-- use the (wildly descriptive) name M instead of IO.
type M a = IO (V a)

-- | LLVM is strongly typed so we'll need a way to annotate each value
-- with a type.  The input parameter is a dummy value only there so
-- that the instance can resolve appropriately.  As such, it is
-- important that we never evaluate the input parameter.
class Ty a where
  ty :: M a -> Type


-- | booleans.
instance Ty Bool where ty _ = "i1"
-- | 8 bit signed integers.
instance Ty Int8 where ty _ = "i8"
-- | 16 bit signed integers.
instance Ty Int16 where ty _ = "i16"
-- | 32 bit signed integers.
instance Ty Int32 where ty _ = "i32"
-- | 8 bit unsigned integers.
instance Ty Word8 where ty _ = "i8"
-- | 16 bit unsigned integers.
instance Ty Word16 where ty _ = "i16"
-- | 32 bit unsigned integers.
instance Ty Word32 where ty _ = "i32"
-- | 32 bit floats.
instance Ty Float where ty _ = "float"
-- | 64 bit doubles.
instance Ty Double where ty _ = "double"
-- BAL: implement the LLVM Half type

-- | A class will allow us to convert Haskell values into LLVM literals.
class Ty a => Lit a where
  lit :: a -> M a

-- | Helper function for defining values.  Using laziness to good effect here.
mkV :: Ty a => String -> M a
mkV x = let m = return V{ valof = x, tyof = ty m } in m

-- | booleans.
instance Lit Bool where lit x = mkV $ if x then "true" else "false"
-- | 8 bit signed integers.
instance Lit Int8 where lit = mkV . show
-- | 16 bit signed integers.
instance Lit Int16 where lit = mkV . show
-- | 32 bit signed integers.
instance Lit Int32 where lit = mkV . show
-- | 8 bit unsigned integers.
instance Lit Word8 where lit = mkV . show
-- | 16 bit unsigned integers.
instance Lit Word16 where lit = mkV . show
-- | 32 bit unsigned integers.
instance Lit Word32 where lit = mkV . show
-- | 32 bit floats.  Currently broken.
instance Lit Float where lit = mkV . show -- BAL: implement IEEE 754 for LLVM
-- | 64 bit doubles.  Currently broken.
instance Lit Double where lit = mkV . show -- BAL: implement IEEE 754 for LLVM

-- | LLVM primitives are polymorphic so let's create a class to
-- enable/enforce that too.
class Ty a => Arith a where
  arithRec :: ArithRec a

-- | This record is used to eliminate boilerplate in the Arith
-- instance definitions.
data ArithRec a = ArithRec
  { _add :: String
  , _sub :: String
  , _mul :: String
  , _div :: String
  , _rem :: String
  }

-- | ArithRec helper for integer types.
intRec :: ArithRec a
intRec = ArithRec
  { _add = "add"
  , _sub = "sub"
  , _mul = "mul"
  , _div = "div"
  , _rem = "rem"
  }

-- | ArithRec helper for unsigned integer types.
wordRec :: ArithRec a
wordRec = intRec{ _div = "udiv", _rem = "urem" }

-- | ArithRec helper for floating types.
floatRec :: ArithRec a
floatRec = ArithRec
  { _add = "fadd"
  , _sub = "fsub"
  , _mul = "fmul"
  , _div = "fdiv"
  , _rem = "frem"
  }

-- | 8 bit signed integers.
instance Arith Int8 where arithRec = intRec
-- | 16 bit signed integers.
instance Arith Int16 where arithRec = intRec
-- | 32 bit signed integers.
instance Arith Int32 where arithRec = intRec
-- | 8 bit unsigned integers.
instance Arith Word8 where arithRec = wordRec
-- | 16 bit unsigned integers.
instance Arith Word16 where arithRec = wordRec
-- | 32 bit unsigned integers.
instance Arith Word32 where arithRec = wordRec
-- | 32 bit floats.  Currently broken.
instance Arith Float where arithRec = floatRec
-- | 64 bit doubles.  Currently broken.
instance Arith Double where arithRec = floatRec

-- | Output an LLVM statement.
stmt :: [String] -> IO ()
stmt = putStrLn . unwords

-- | LLVM 'ret' statement.
ret :: Ty a => M a -> IO ()
ret m = do
  a <- m
  stmt ["ret", tyvalof a]

-- | Generate IO from our monad.  Currently just generates a main
-- function that takes no parameters and returns a value.
-- | As an example:
--
-- >>> runM $ add (lit 1) (lit 41)
-- define i32 @main()
-- {
-- %v3 = add i32 1, 41
-- ret i32 %v3
-- }

runM :: M Int32 -> IO ()
runM m = do
  stmt ["define", ty m, "@main()"]
  putStrLn "{"
  ret m
  putStrLn "}"

-- | Construct a local unique variable.
fresh :: Ty a => M a
fresh = do
  i <- newUnique
  mkV $ "%v" ++ show (hashUnique i)

-- | Helper function to implement binary operations.  LLVM statements
-- must be in SSA form.
binop :: (Ty a, Ty b, Ty c) => String -> M a -> M b -> M c
binop f x y = do
  a <- x
  b <- y
  v <- fresh
  stmt [valof v, "=", f, tyvalof a ++ ",", valof b]
  return v

-- | Helper function to implement arithmetic operations.
arith :: Arith a => (ArithRec a -> String) -> M a -> M a -> M a
arith f = binop (f arithRec)

-- | result = (f)add ty op1, op2
add :: Arith a => M a -> M a -> M a
add = arith _add

-- | result = (f)sub ty op1, op2
sub :: Arith a => M a -> M a -> M a
sub = arith _sub

-- | result = (f)mul ty op1, op2
mul :: Arith a => M a -> M a -> M a
mul = arith _mul

-- | result = (u/f)div ty op1, op2
div :: Arith a => M a -> M a -> M a
div = arith _div

-- | result = (u/f)rem ty op1, op2
rem :: Arith a => M a -> M a -> M a
rem = arith _rem
