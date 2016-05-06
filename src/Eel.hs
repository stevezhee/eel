{-# OPTIONS_GHC -Wall #-}
{-|
Module      : Eel
Description : Eel EDSL.
Copyright   : Brett Letner, 2016
License     : BSD3
Maintainer  : 
Stability   : experimental
Portability : 

The Eel EDSL is an Embedded Domain Specific Language intended for embedded (resource constrained) development.

-}

module Eel
( module Eel
, module Data.Int
, module Data.Word
)
  
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
-- | 32 bit ordered floats.
instance Ty Float where ty _ = "float"
-- | 64 bit ordered doubles.
instance Ty Double where ty _ = "double"

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
-- | 32 bit ordered floats.  Currently broken.
instance Lit Float where lit = mkV . show -- BAL: implement IEEE 754 for LLVM
-- | 64 bit ordered doubles.  Currently broken.
instance Lit Double where lit = mkV . show -- BAL: implement IEEE 754 for LLVM

-- | LLVM arithmetic primitives are (adhoc)polymorphic so let's create a
-- class to enable/enforce that too.
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
intArith :: ArithRec a
intArith = ArithRec
  { _add = "add"
  , _sub = "sub"
  , _mul = "mul"
  , _div = "div"
  , _rem = "rem"
  }

-- | ArithRec helper for unsigned integer types.
wordArith :: ArithRec a
wordArith = intArith{ _div = "udiv", _rem = "urem" }

-- | ArithRec helper for floating types.
floatArith :: ArithRec a
floatArith = ArithRec
  { _add = "fadd"
  , _sub = "fsub"
  , _mul = "fmul"
  , _div = "fdiv"
  , _rem = "frem"
  }

-- | 8 bit signed integers.
instance Arith Int8 where arithRec = intArith
-- | 16 bit signed integers.
instance Arith Int16 where arithRec = intArith
-- | 32 bit signed integers.
instance Arith Int32 where arithRec = intArith
-- | 8 bit unsigned integers.
instance Arith Word8 where arithRec = wordArith
-- | 16 bit unsigned integers.
instance Arith Word16 where arithRec = wordArith
-- | 32 bit unsigned integers.
instance Arith Word32 where arithRec = wordArith
-- | 32 bit ordered floats.
instance Arith Float where arithRec = floatArith
-- | 64 bit ordered doubles.
instance Arith Double where arithRec = floatArith

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

-- | result = [u/f]div ty op1, op2
div :: Arith a => M a -> M a -> M a
div = arith _div

-- | result = [u/f]rem ty op1, op2
rem :: Arith a => M a -> M a -> M a
rem = arith _rem

-- | Bitwise operations.
class Ty a => BitOps a where
  bitOpsRec :: BitOpsRec a

-- | This record is used to eliminate boilerplate in the BitOps
-- instance definitions.
data BitOpsRec a = BitOpsRec
  { _shl :: String
  , _shr :: String
  , _and :: String
  , _or :: String
  , _xor :: String
  }

-- | Helper function to implement bitwise operations.
bitop :: BitOps a => (BitOpsRec a -> String) -> M a -> M a -> M a
bitop f = binop (f bitOpsRec)

-- | result = shl ty op1, op2
shl :: BitOps a => M a -> M a -> M a
shl = bitop _shl

-- | result = [l/a]shr ty op1, op2
shr :: BitOps a => M a -> M a -> M a
shr = bitop _shr

-- | result = and ty op1, op2
and :: BitOps a => M a -> M a -> M a
and = bitop _and

-- | result = or ty op1, op2
or :: BitOps a => M a -> M a -> M a
or = bitop _or

-- | result = xor ty op1, op2
xor :: BitOps a => M a -> M a -> M a
xor = bitop _xor

-- | BitOpsRec helper for integer types.
intBitOps :: BitOpsRec a
intBitOps = BitOpsRec
  { _shl = "shl"
  , _shr = "ashr"
  , _and = "and"
  , _or = "or"
  , _xor = "xor"
  }

-- | BitOpsRec helper for unsigned integer types.
wordBitOps :: BitOpsRec a
wordBitOps = intBitOps{ _shr = "lshr" }

-- | 8 bit signed integers.
instance BitOps Int8 where bitOpsRec = intBitOps
-- | 16 bit signed integers.
instance BitOps Int16 where bitOpsRec = intBitOps
-- | 32 bit signed integers.
instance BitOps Int32 where bitOpsRec = intBitOps
-- | booleans.
instance BitOps Bool where bitOpsRec = wordBitOps
-- | 8 bit unsigned integers.
instance BitOps Word8 where bitOpsRec = wordBitOps
-- | 16 bit unsigned integers.
instance BitOps Word16 where bitOpsRec = wordBitOps
-- | 32 bit unsigned integers.
instance BitOps Word32 where bitOpsRec = wordBitOps

-- | LLVM comparison primitives.
class Ty a => Cmp a where
  cmpRec :: CmpRec a

-- | This record is used to eliminate boilerplate in the Cmp
-- instance definitions.
data CmpRec a = CmpRec
  { _eq :: String
  , _ne :: String
  , _gt :: String
  , _ge :: String
  , _lt :: String
  , _le :: String
  }

-- | Helper function to implement comparison operations.
cmp :: Cmp a => (CmpRec a -> String) -> M a -> M a -> M Bool
cmp f = binop (f cmpRec)

-- | result = [f/i]cmp [o]eq ty op1, op2
eq :: Cmp a => M a -> M a -> M Bool
eq = cmp _eq

-- | result = [f/i]cmp [o]ne ty op1, op2
ne :: Cmp a => M a -> M a -> M Bool
ne = cmp _ne

-- | result = [f/i]cmp [o/s/u]gt ty op1, op2
gt :: Cmp a => M a -> M a -> M Bool
gt = cmp _gt

-- | result = [f/i]cmp [o/s/u]ge ty op1, op2
ge :: Cmp a => M a -> M a -> M Bool
ge = cmp _ge

-- | result = [f/i]cmp [o/s/u]lt ty op1, op2
lt :: Cmp a => M a -> M a -> M Bool
lt = cmp _lt

-- | result = [f/i]cmp [o/s/u]le ty op1, op2
le :: Cmp a => M a -> M a -> M Bool
le = cmp _le

-- | CmpRec helper for integer types.
intCmp :: CmpRec a
intCmp = CmpRec
  { _eq = "icmp eq"
  , _ne = "icmp ne"
  , _gt = "icmp sgt"
  , _ge = "icmp sge"
  , _lt = "icmp slt"
  , _le = "icmp sle"
  }

-- | CmpRec helper for unsigned integer types.
wordCmp :: CmpRec a
wordCmp = intCmp
  { _gt = "icmp ugt"
  , _ge = "icmp uge"
  , _lt = "icmp ult"
  , _le = "icmp ule"
  }

-- | CmpRec helper for floating types.
floatCmp :: CmpRec a
floatCmp = CmpRec
  { _eq = "fcmp oeq"
  , _gt = "fcmp ogt"
  , _ge = "fcmp oge"
  , _lt = "fcmp olt"
  , _le = "fcmp ole"
  , _ne = "fcmp one"
  }

-- | 8 bit signed integers.
instance Cmp Int8 where cmpRec = intCmp
-- | 16 bit signed integers.
instance Cmp Int16 where cmpRec = intCmp
-- | 32 bit signed integers.
instance Cmp Int32 where cmpRec = intCmp
-- | 8 bit unsigned integers.
instance Cmp Word8 where cmpRec = wordCmp
-- | 16 bit unsigned integers.
instance Cmp Word16 where cmpRec = wordCmp
-- | 32 bit unsigned integers.
instance Cmp Word32 where cmpRec = wordCmp
-- | 32 bit ordered floats.
instance Cmp Float where cmpRec = floatCmp
-- | 64 bit ordered doubles.
instance Cmp Double where cmpRec = floatCmp

-- | Class to implement polymorphic (unsafe) casts.
class Ty a => Cast a where
  cst :: M a -> Cst

-- | This datatype is used to eliminate boilerplate in the Cast
-- instance definitions.
data Cst = SInt Int | UInt Int | FP Int deriving (Show, Eq)

-- | booleans.
instance Cast Bool where cst _ = UInt 1
-- | 8 bit unsigned integers.
instance Cast Word8 where cst _ = UInt 8
-- | 16 bit unsigned integers.
instance Cast Word16 where cst _ = UInt 16
-- | 32 bit unsigned integers.
instance Cast Word32 where cst _ = UInt 32
-- | 8 bit signed integers.
instance Cast Int8 where cst _ = SInt 8
-- | 16 bit signed integers.
instance Cast Int16 where cst _ = SInt 16
-- | 32 bit signed integers.
instance Cast Int32 where cst _ = SInt 32
-- | 32 bit ordered floats.
instance Cast Float where cst _ = FP 32
-- | 64 bit ordered doubles.
instance Cast Double where cst _ = FP 64

-- | polymorphic (unsafe) casting.
cast :: (Cast a, Cast b) => M a -> M b
cast x = y
  where
    y = case (cst x, cst y) of
      (SInt i, UInt j) | i > j -> to "trunc"
      (SInt i, SInt j) | i > j -> to "trunc"
      (SInt i, UInt j) | i < j -> to "sext"
      (SInt i, SInt j) | i < j -> to "sext"
      (SInt _, FP _) -> to "sitofp"
    
      (UInt i, UInt j) | i > j -> to "trunc"
      (UInt i, SInt j) | i > j -> to "trunc"
      (UInt i, UInt j) | i < j -> to "zext"
      (UInt i, SInt j) | i < j -> to "zext"
      (UInt _, FP _) -> to "uitofp"
    
      (FP _, SInt _) -> to "fptosi"
      (FP _, UInt _) -> to "fptoui"
      (FP i, FP j) | i > j -> to "fptrunc"
      (FP i, FP j) | i < j -> to "fpext"
      _ -> do -- These types are LLVM equivalent.  Just change the EDSL types.
        V a b <- x
        return $ V a b
    to s = do
      a <- x
      v <- fresh
      stmt [valof v, "=", s, tyvalof a, "to", ty y]
      return v
