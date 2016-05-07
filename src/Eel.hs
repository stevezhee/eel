{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Eel
Description : DSL
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
import Control.Monad.State

-- | Quick and dirty representation of LLVM types.
-- http://llvm.org/docs/LangRef.html#type-system
type Type = String
-- | Quick and dirty representation of LLVM values.
type Value = String

-- | Representation for a typed value in our EDSL.  This datatype
-- allows us to leverage the Haskell compiler's type inference for our
-- EDSL.  Datatypes such as this are called phantom types.
data V a = V{ valof :: Value, tyof :: Type } deriving Show

-- | Helper function for use when you want to show both the type and
-- the value, e.g. i32 7.
tyvalof :: V a -> String
tyvalof x = unwords [ tyof x, valof x ]

-- | Current output context.
data Context = Context
  { nextVar :: Int -- ^ used to generate unique variables
  , nextLabel :: Int -- ^ used to generate uniqe labels
  , outputs :: [String] -- ^ output buffer
  } deriving Show

-- | Initial context.
initContext :: Context
initContext = Context 0 0 []

-- | Internal state used during processing.
data St = St
  { context :: Context -- ^ current context
  , contexts :: [Context] -- ^ stack of contexts
  , namespace :: [(String, [String])] -- ^ global namespace
  } deriving Show

-- | State monad to thread our state with.
type M a = State St a

-- | Shorthand for values returned from the state.
type I a = M (V a)
  
-- | LLVM pointer type.
-- http://llvm.org/docs/LangRef.html#pointer-type
data Ptr a = Ptr{ unPtr :: Int } deriving Show
  
-- | Convenience function for when we need a value but we don't have
-- one handy (so that we can extract its inferred type).
unused :: String -> a
unused s = error $ "unused:" ++ s

-- | LLVM extractvalue instruction.
-- | LLVM insertvalue instruction.

-- | LLVM is strongly typed so we'll need a way to annotate each value
-- with a type.  The input parameter is a placeholder which is only
-- there so that the instance can resolve appropriately.  As such, it
-- is important that we never evaluate the input parameter.
class Ty a where
  ty :: I a -> Type

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
-- | 32 bit ordered floats.  Does Haskell have sized Floats?
instance Ty Float where ty _ = "float"
-- | 64 bit ordered doubles.  Does Haskell have sized Doubles?
instance Ty Double where ty _ = "double"
-- | pointers.
instance Ty a => Ty (Ptr a) where ty (_ :: I (Ptr a)) = ty (unused "Ty Ptr" :: I a) ++ "*"

-- | A class will allow us to convert Haskell values into LLVM
-- literals.
class Ty a => Lit a where
  lit :: a -> I a

-- | Helper function for defining values.  Using laziness to good
-- effect here.
mkV :: Ty a => String -> I a
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

-- | LLVM arithmetic primitives are (ad hoc)polymorphic so let's create
-- a class to enable/enforce that too.
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

-- | ArithRec helper for signed integer types.
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

-- | Helper function to update the current context.
modifyCxt :: (Context -> Context) -> M Context
modifyCxt f = do
  cxt <- gets context
  let a = f cxt
  modify $ \st -> st{ context = a }
  return cxt

-- | Add a string to the output of the current context.
output :: String -> M ()
output s = void $ modifyCxt $ \cxt -> cxt{ outputs = s : outputs cxt }
      
-- | Output an LLVM instruction.
instr :: [String] -> M ()
instr = output . unwords

-- | Helper function for LLVM assignment instructions.
assign :: Ty a => [String] -> I a
assign xs = do
  x <- instantiate
  a <- x
  instr $ [valof a, "="] ++ xs
  return a

-- | A class is used to model parameters/arguments.
class Args a where
  instantiate :: M a
  unArgs :: a -> M [V ()]

-- | Single argument.
instance Ty a => Args (I a) where
  instantiate = do
    cxt <- modifyCxt $ \cxt -> cxt{ nextVar = succ $ nextVar cxt }
    v <- mkV ("%v" ++ show (nextVar cxt))
    return (return v)
  unArgs x = ((: []) . unV) <$> x
  
-- | Two arguments.
instance (Args a, Args b) => Args (a,b) where
  instantiate = (,) <$> instantiate <*> instantiate
  unArgs (x,y) = (++) <$> unArgs x <*> unArgs y
  
-- | Helper function to print out LLVM parameters.
params :: [String] -> String
params [] = "()"
params xs = "(" ++ foldr1 comma xs ++ ")"

-- | Helper function to print out LLVM global names.
global :: String -> String
global = (++) "@"

-- | Internal function used for LLVM function call instruction.
-- http://llvm.org/docs/LangRef.html#call-instruction
call :: (Args a, Ty b) => String -> a -> I b
call n x =
  let b = unArgs x >>= \a -> assign ["call", ty b, global n, params $ fmap tyvalof a] in b

-- | Internal function used for LLVM procedure call instruction.
-- http://llvm.org/docs/LangRef.html#call-instruction
call' :: (Args a) => String -> a -> M ()
call' n x = unArgs x >>= \a -> instr ["call", "void", global n, params $ fmap tyvalof a]

-- | Internal function used for LLVM function definitions. The
-- argument with type (M b -> String) must be a function that doesn't
-- evaluate it's argument.
-- http://llvm.org/docs/LangRef.html#functions
define :: Args a => String -> (a -> M b) -> (M b -> String) -> (b -> String) -> M ()
define n f g h = whenUnknown n $ do
  x <- instantiate
  a <- unArgs x
  instr ["define", g $ f x, global n, params $ fmap tyvalof a]
  output "{"
  b <- f x
  instr ["ret", h b]
  output "}"

-- | Convience function that pairs up defines and calls for LLVM
-- functions.  http://llvm.org/docs/LangRef.html#functions
func :: (Args a, Ty b) => String -> (a -> I b) -> a -> I b
func n f a = define n f ty tyvalof >> call n a
  
-- | Convience function that pairs up defines and calls for LLVM
-- procedures.  http://llvm.org/docs/LangRef.html#functions
func' :: Args a => String -> (a -> M ()) -> a -> M ()
func' n f a = define n f (const "void") (const "void") >> call' n a

-- | Helper function that adds definitions and declarations to the
-- state if they haven't been seen before.
whenUnknown :: String -> M () -> M ()
whenUnknown n m = do
  ns <- (fst . unzip) <$> gets namespace
  when (n `notElem` ns) $ do
    modify $ \st -> st{ context = initContext, contexts =  context st : contexts st }
    m
    modify $ \st -> st
      { namespace = (n, reverse $ outputs $ context st) : namespace st
      , context = head $ contexts st
      , contexts = tail $ contexts st
      }

-- | Internal function used for LLVM function declarations.
-- functions.  http://llvm.org/docs/LangRef.html#functions
declare :: (Args a) => String -> Type -> a -> M ()
declare n t x = whenUnknown n $ do
  a <- unArgs x
  instr ["declare", t, global n, params $ fmap tyof a ]

-- | LLVM foreign function interface.
-- http://llvm.org/docs/LangRef.html#functions
ffi :: (Args a, Ty b) => String -> a -> I b
ffi n a = let b = declare n (ty b) a >> call n a in b
  
-- | LLVM foreign procedure interface.
-- http://llvm.org/docs/LangRef.html#functions
ffi' :: (Args a) => String -> a -> M ()
ffi' n a = declare n "void" a >> call' n a

-- | Quick and dirty representation of LLVM labels.  http://llvm.org/docs/LangRef.html#label-type
type Label = String

-- | Helper used for comma separated things.
comma :: String -> String -> String
comma x y = x ++ ", " ++ y

-- | Helper used for branching to labels.
label :: String -> String
label = (++) "label %"

-- | LLVM conditional br instruction. http://llvm.org/docs/LangRef.html#br-instruction
br :: I Bool -> Label -> Label -> M ()
br x y z = do
  a <- x
  instr ["br", tyvalof a `comma` label y `comma` label z]

-- | LLVM unconditional br instruction. http://llvm.org/docs/LangRef.html#br-instruction
br' :: Label -> M ()
br' x = instr ["br", label x]

-- | LLVM alloca instruction without the number of elements.
-- http://llvm.org/docs/LangRef.html#alloca-instruction
alloca :: (Ty a) => I (Ptr a)
alloca = alloca' (lit 1 :: I Int32)

-- | LLVM alloca instruction with the number of elements.
-- http://llvm.org/docs/LangRef.html#alloca-instruction
alloca' :: (Ty a, IsInt b) => I b -> I (Ptr a)
alloca' x = let a = x >>= \b -> assign ["alloca", ty (load a) `comma` tyvalof b] in a

-- | LLVM load instruction.
-- http://llvm.org/docs/LangRef.html#load-instruction
load :: Ty a => I (Ptr a) -> I a
load x = let b = x >>= \a -> assign ["load", ty b `comma` tyvalof a] in b

-- | LLVM store instruction.
-- http://llvm.org/docs/LangRef.html#store-instruction
store :: Ty a => I a -> I (Ptr a) -> M ()
store x y = do
  a <- x
  p <- y
  instr ["store", tyvalof a `comma` tyvalof p]

-- | LLVM getelementptr instruction.
getelementptr :: (Ty a, IsInt b) => I (Ptr a) -> I b -> I (Ptr a)
getelementptr x y = do
  p <- x
  i <- y
  assign ["getelementptr", ty (load x) `comma` tyvalof p `comma` tyvalof i]

-- | Generate text from our monad.  This is the program generation
-- entry point.
-- | As an example:
--
-- >>> mainM $ \(argc, argv) -> add (lit 42) argc
-- 
-- define i32 @main (i32 %v0, i8** %v1)
-- {
-- %v2 = add i32 42, %v0
-- ret i32 %v2
-- }
mainM :: ((I Int32, I (Ptr (Ptr Word8))) -> I Int32) -> IO ()
mainM f = do
  let st = execState (define "main" f ty tyvalof) $ St initContext [] []
  putStrLn $ unlines $ concat $ reverse $ snd <$> namespace st
  
-- | Construct a local unique label.
newLabel :: M Label
newLabel = do
  cxt <- modifyCxt $ \cxt -> cxt{ nextLabel = succ $ nextLabel cxt }
  return $ "L" ++ show (nextLabel cxt)

-- | Helper function to implement binary operations.  LLVM instructions
-- must be in SSA form.
binop :: (Ty a, Ty b, Ty c) => String -> I a -> I b -> I c
binop f x y = do
  a <- x
  b <- y
  assign [f, tyvalof a `comma` valof b]
        
-- | Helper function to implement arithmetic operations.
arith :: Arith a => (ArithRec a -> String) -> I a -> I a -> I a
arith f = binop (f arithRec)

-- | result = (f)add ty op1, op2
-- http://llvm.org/docs/LangRef.html#add-instruction
-- http://llvm.org/docs/LangRef.html#fadd-instruction
add :: Arith a => I a -> I a -> I a
add = arith _add

-- | result = (f)sub ty op1, op2
-- http://llvm.org/docs/LangRef.html#sub-instruction
-- http://llvm.org/docs/LangRef.html#fsub-instruction
sub :: Arith a => I a -> I a -> I a
sub = arith _sub

-- | result = (f)mul ty op1, op2
-- http://llvm.org/docs/LangRef.html#mul-instruction
-- http://llvm.org/docs/LangRef.html#fmul-instruction
mul :: Arith a => I a -> I a -> I a
mul = arith _mul

-- | result = [u/f]div ty op1, op2
-- http://llvm.org/docs/LangRef.html#div-instruction
-- http://llvm.org/docs/LangRef.html#fdiv-instruction
div :: Arith a => I a -> I a -> I a
div = arith _div

-- | result = [u/f]rem ty op1, op2
-- http://llvm.org/docs/LangRef.html#rem-instruction
-- http://llvm.org/docs/LangRef.html#frem-instruction
rem :: Arith a => I a -> I a -> I a
rem = arith _rem

-- | Bitwise operations.
class Ty a => IsInt a where
  isIntRec :: IsIntRec a

-- | This record is used to eliminate boilerplate in the IsInt
-- instance definitions.
data IsIntRec a = IsIntRec{ _shr :: String }

-- | result = shl ty op1, op2
-- http://llvm.org/docs/LangRef.html#shl-instruction
shl :: IsInt a => I a -> I a -> I a
shl = binop "shl"

-- | result = [l/a]shr ty op1, op2
-- http://llvm.org/docs/LangRef.html#lshr-instruction
-- http://llvm.org/docs/LangRef.html#ashr-instruction
shr :: IsInt a => I a -> I a -> I a
shr (x :: I a) = binop (_shr (isIntRec :: IsIntRec a)) x

-- | result = and ty op1, op2
-- http://llvm.org/docs/LangRef.html#and-instruction
and :: IsInt a => I a -> I a -> I a
and = binop "and"

-- | result = or ty op1, op2
-- http://llvm.org/docs/LangRef.html#or-instruction
or :: IsInt a => I a -> I a -> I a
or = binop "or"

-- | result = xor ty op1, op2
-- http://llvm.org/docs/LangRef.html#xor-instruction
xor :: IsInt a => I a -> I a -> I a
xor = binop "xor"

-- | IsIntRec helper for signed integer types.
intIsInt :: IsIntRec a
intIsInt = IsIntRec{ _shr = "ashr" }

-- | IsIntRec helper for unsigned integer types.
wordIsInt :: IsIntRec a
wordIsInt = IsIntRec{ _shr = "lshr" }

-- | 8 bit signed integers.
instance IsInt Int8 where isIntRec = intIsInt
-- | 16 bit signed integers.
instance IsInt Int16 where isIntRec = intIsInt
-- | 32 bit signed integers.
instance IsInt Int32 where isIntRec = intIsInt
-- | booleans.
instance IsInt Bool where isIntRec = wordIsInt
-- | 8 bit unsigned integers.
instance IsInt Word8 where isIntRec = wordIsInt
-- | 16 bit unsigned integers.
instance IsInt Word16 where isIntRec = wordIsInt
-- | 32 bit unsigned integers.
instance IsInt Word32 where isIntRec = wordIsInt

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
cmp :: Cmp a => (CmpRec a -> String) -> I a -> I a -> I Bool
cmp f = binop (f cmpRec)

-- | result = [f/i]cmp [o]eq ty op1, op2
-- http://llvm.org/docs/LangRef.html#icmp-instruction
-- http://llvm.org/docs/LangRef.html#fcmp-instruction
eq :: Cmp a => I a -> I a -> I Bool
eq = cmp _eq

-- | result = [f/i]cmp [o]ne ty op1, op2
-- http://llvm.org/docs/LangRef.html#icmp-instruction
-- http://llvm.org/docs/LangRef.html#fcmp-instruction
ne :: Cmp a => I a -> I a -> I Bool
ne = cmp _ne

-- | result = [f/i]cmp [o/s/u]gt ty op1, op2
-- http://llvm.org/docs/LangRef.html#icmp-instruction
-- http://llvm.org/docs/LangRef.html#fcmp-instruction
gt :: Cmp a => I a -> I a -> I Bool
gt = cmp _gt

-- | result = [f/i]cmp [o/s/u]ge ty op1, op2
-- http://llvm.org/docs/LangRef.html#icmp-instruction
-- http://llvm.org/docs/LangRef.html#fcmp-instruction
ge :: Cmp a => I a -> I a -> I Bool
ge = cmp _ge

-- | result = [f/i]cmp [o/s/u]lt ty op1, op2
-- http://llvm.org/docs/LangRef.html#icmp-instruction
-- http://llvm.org/docs/LangRef.html#fcmp-instruction
lt :: Cmp a => I a -> I a -> I Bool
lt = cmp _lt

-- | result = [f/i]cmp [o/s/u]le ty op1, op2
-- http://llvm.org/docs/LangRef.html#icmp-instruction
-- http://llvm.org/docs/LangRef.html#fcmp-instruction
le :: Cmp a => I a -> I a -> I Bool
le = cmp _le

-- | CmpRec helper for signed integer types.
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
  cst :: I a -> Cst

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

-- | Internal function for (unsafely) forcing a change in type.
unV :: V a -> V b
unV (V a b) = V a b
  
-- | polymorphic (unsafe) casting.
-- http://llvm.org/docs/LangRef.html#trunc-to-instruction
-- http://llvm.org/docs/LangRef.html#zext-to-instruction
-- http://llvm.org/docs/LangRef.html#sext-to-instruction
-- http://llvm.org/docs/LangRef.html#fptrunc-to-instruction
-- http://llvm.org/docs/LangRef.html#fpext-to-instruction
-- http://llvm.org/docs/LangRef.html#fptoui-to-instruction
-- http://llvm.org/docs/LangRef.html#fptosi-to-instruction
-- http://llvm.org/docs/LangRef.html#uitofp-to-instruction
-- http://llvm.org/docs/LangRef.html#sitofp-to-instruction
cast :: (Cast a, Cast b) => I a -> I b
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
      _ -> unV <$> x -- These types are LLVM equivalent.  Just change the EDSL types.
    to s = do
      a <- x
      assign [s, tyvalof a, "to", ty y]
