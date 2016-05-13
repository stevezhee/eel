{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Eel
Description : DSL
Copyright   : Brett Letner, 2016
License     : BSD3
Maintainer  : 
Stability   : experimental
Portability : 

The Eel EDSL is an Embedded Domain Specific Language intended for
embedded (resource constrained) development.

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
-- import Control.Applicative


-- | Quick and dirty representation of LLVM types.
-- http://llvm.org/docs/LangRef.html#type-system
type Type = String
-- | Quick and dirty representation of LLVM values.
type Value = String

-- | Representation for a typed value in our EDSL.  This datatype
-- allows us to leverage the Haskell compiler's type inference for our
-- EDSL.  Datatypes such as this are called phantom types.
data V a = V{ valof :: Value, tyof :: Type } deriving Show

-- | Internal function for (unsafely) forcing a change in type.
castV :: V a -> V b
castV (V a b) = V a b

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

-- | Quick and dirty stack representation.
type Stack a = [a]

-- | Internal state used during processing.
data St = St
  { context :: Context -- ^ current context
  , contexts :: Stack Context -- ^ stack of contexts
  , blocks :: Stack Label -- ^ stack of the current label
  , namespace :: [(String, [String])] -- ^ global namespace
  } deriving Show

-- | State monad to thread our state with.
type M a = State St a

-- | Instruction.  Shorthand for values returned from the state monad.
type I a = M (V a)

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
  
-- | LLVM pointer type.
-- http://llvm.org/docs/LangRef.html#pointer-type
data Ptr a = Ptr{ unPtr :: Int } deriving Show
  
-- | Convenience function for when we need a value but we don't have
-- one handy (so that we can extract its inferred type).
unused :: String -> a
unused s = error $ "unused:" ++ s

-- | LLVM is strongly typed so we'll need a way to annotate each value
-- with a type.  The input parameter is a placeholder which is only
-- there so that the instance can resolve appropriately.  As such, it
-- is important that we never evaluate the input parameter.
class Ty a where
  ty :: I a -> Type

-- | booleans.
instance Ty Bool where ty _ = "i1"
-- | 8 bit unsigned characters.
instance Ty Char where ty _ = "i8"
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
-- | 64 bit unsigned integers.
instance Ty Word64 where ty _ = "i64"
-- | 32 bit ordered floats.  Does Haskell have sized Floats?
instance Ty Float where ty _ = "float"
-- | 64 bit ordered doubles.  Does Haskell have sized Doubles?
instance Ty Double where ty _ = "double"
-- | pointers.
instance Ty a => Ty (Ptr a) where
  ty (_ :: I (Ptr a)) = ty (unused "Ty Ptr" :: I a) ++ "*"

-- | A class is used to model parameters/arguments.  We only need
-- instances for zero, one, and two arguments.  Everything else can be
-- constructed from those.
class Args a where
  instantiate :: M a
  unArgs :: a -> [V ()]

-- | Zero arguments.
instance Args () where
  instantiate = return ()
  unArgs () = []

-- | Single argument.
instance Ty a => Args (V a) where
  instantiate = do
    cxt <- modifyCxt $ \cxt -> cxt{ nextVar = succ $ nextVar cxt }
    return $ mkV ("%v" ++ show (nextVar cxt))
  unArgs x = [castV x]
  
-- | Two arguments.
instance (Args a, Args b) => Args (a,b) where
  instantiate = (,) <$> instantiate <*> instantiate
  unArgs (x,y) = unArgs x ++ unArgs y

-- | A class will allow us to convert Haskell values into LLVM
-- literals.
class Ty a => Lit a where
  lit :: a -> V a

-- | Helper function for defining values.  Using laziness to good
-- effect here.
mkV :: Ty a => String -> V a
mkV x = let v = V{ valof = x, tyof = ty (return v) } in v

-- | booleans.
instance Lit Bool where lit x = mkV $ if x then "true" else "false"
-- | 8 bit unsigned characters.
instance Lit Char where lit = mkV . show . fromEnum
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
-- | 64 bit unsigned integers.
instance Lit Word64 where lit = mkV . show
-- | 32 bit ordered floats.  Currently broken.
instance Lit Float where lit = mkV . show -- BAL: implement IEEE 754 for LLVM
-- | 64 bit ordered doubles.  Currently broken.
instance Lit Double where lit = mkV . show -- BAL: implement IEEE 754 for LLVM

-- | This record is used to eliminate boilerplate in the Arith
-- instance definitions.
data ArithRec a = ArithRec
  { _add :: String
  , _sub :: String
  , _mul :: String
  , _div :: String
  , _rem :: String
  }

-- | LLVM arithmetic primitives are (ad hoc)polymorphic so let's create
-- a class to enable/enforce that too.
class Ty a => Arith a where
  arithRec :: ArithRec a

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
-- | 64 bit unsigned integers.
instance Arith Word64 where arithRec = wordArith
-- | 32 bit ordered floats.
instance Arith Float where arithRec = floatArith
-- | 64 bit ordered doubles.
instance Arith Double where arithRec = floatArith

-- | Helper function to implement binary operations.  LLVM instructions
-- must be in SSA form.
binop :: (Ty a, Ty b, Ty c) => String -> V a -> V b -> I c
binop f a b = assign [f, tyvalof a `comma` valof b]
        
-- | Helper function to implement arithmetic operations.
arith :: Arith a => (ArithRec a -> String) -> V a -> V a -> I a
arith f = binop (f arithRec)

-- | result = (f)add ty op1, op2
-- http://llvm.org/docs/LangRef.html#add-instruction
-- http://llvm.org/docs/LangRef.html#fadd-instruction
add :: Arith a => V a -> V a -> I a
add = arith _add

-- | result = (f)sub ty op1, op2
-- http://llvm.org/docs/LangRef.html#sub-instruction
-- http://llvm.org/docs/LangRef.html#fsub-instruction
sub :: Arith a => V a -> V a -> I a
sub = arith _sub

-- | result = (f)mul ty op1, op2
-- http://llvm.org/docs/LangRef.html#mul-instruction
-- http://llvm.org/docs/LangRef.html#fmul-instruction
mul :: Arith a => V a -> V a -> I a
mul = arith _mul

-- | result = [u/f]div ty op1, op2
-- http://llvm.org/docs/LangRef.html#div-instruction
-- http://llvm.org/docs/LangRef.html#fdiv-instruction
div :: Arith a => V a -> V a -> I a
div = arith _div

-- | result = [u/f]rem ty op1, op2
-- http://llvm.org/docs/LangRef.html#rem-instruction
-- http://llvm.org/docs/LangRef.html#frem-instruction
rem :: Arith a => V a -> V a -> I a
rem = arith _rem

-- | Bitwise operations.
class Ty a => IsInt a where
  isIntRec :: IsIntRec a

-- | This record is used to eliminate boilerplate in the IsInt
-- instance definitions.
data IsIntRec a = IsIntRec{ _shr :: String }

-- | result = shl ty op1, op2
-- http://llvm.org/docs/LangRef.html#shl-instruction
shl :: IsInt a => V a -> V a -> I a
shl = binop "shl"

-- | result = [l/a]shr ty op1, op2
-- http://llvm.org/docs/LangRef.html#lshr-instruction
-- http://llvm.org/docs/LangRef.html#ashr-instruction
shr :: IsInt a => V a -> V a -> I a
shr (x :: V a) = binop (_shr (isIntRec :: IsIntRec a)) x

-- | result = and ty op1, op2
-- http://llvm.org/docs/LangRef.html#and-instruction
and :: IsInt a => V a -> V a -> I a
and = binop "and"

-- | result = or ty op1, op2
-- http://llvm.org/docs/LangRef.html#or-instruction
or :: IsInt a => V a -> V a -> I a
or = binop "or"

-- | result = xor ty op1, op2
-- http://llvm.org/docs/LangRef.html#xor-instruction
xor :: IsInt a => V a -> V a -> I a
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
-- | 64 bit unsigned integers.
instance IsInt Word64 where isIntRec = wordIsInt

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
cmp :: Cmp a => (CmpRec a -> String) -> V a -> V a -> I Bool
cmp f = binop (f cmpRec)

-- | result = [f/i]cmp [o]eq ty op1, op2
-- http://llvm.org/docs/LangRef.html#icmp-instruction
-- http://llvm.org/docs/LangRef.html#fcmp-instruction
eq :: Cmp a => V a -> V a -> I Bool
eq = cmp _eq

-- | result = [f/i]cmp [o]ne ty op1, op2
-- http://llvm.org/docs/LangRef.html#icmp-instruction
-- http://llvm.org/docs/LangRef.html#fcmp-instruction
ne :: Cmp a => V a -> V a -> I Bool
ne = cmp _ne

-- | result = [f/i]cmp [o/s/u]gt ty op1, op2
-- http://llvm.org/docs/LangRef.html#icmp-instruction
-- http://llvm.org/docs/LangRef.html#fcmp-instruction
gt :: Cmp a => V a -> V a -> I Bool
gt = cmp _gt

-- | result = [f/i]cmp [o/s/u]ge ty op1, op2
-- http://llvm.org/docs/LangRef.html#icmp-instruction
-- http://llvm.org/docs/LangRef.html#fcmp-instruction
ge :: Cmp a => V a -> V a -> I Bool
ge = cmp _ge

-- | result = [f/i]cmp [o/s/u]lt ty op1, op2
-- http://llvm.org/docs/LangRef.html#icmp-instruction
-- http://llvm.org/docs/LangRef.html#fcmp-instruction
lt :: Cmp a => V a -> V a -> I Bool
lt = cmp _lt

-- | result = [f/i]cmp [o/s/u]le ty op1, op2
-- http://llvm.org/docs/LangRef.html#icmp-instruction
-- http://llvm.org/docs/LangRef.html#fcmp-instruction
le :: Cmp a => V a -> V a -> I Bool
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

-- | 8 bit unsigned characters.
instance Cmp Char where cmpRec = wordCmp
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
-- | 64 bit unsigned integers.
instance Cmp Word64 where cmpRec = wordCmp
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
-- | 8 bit unsigned characters.
instance Cast Char where cst _ = UInt 8
-- | 8 bit unsigned integers.
instance Cast Word8 where cst _ = UInt 8
-- | 16 bit unsigned integers.
instance Cast Word16 where cst _ = UInt 16
-- | 32 bit unsigned integers.
instance Cast Word32 where cst _ = UInt 32
-- | 64 bit unsigned integers.
instance Cast Word64 where cst _ = UInt 64
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
-- http://llvm.org/docs/LangRef.html#trunc-to-instruction
-- http://llvm.org/docs/LangRef.html#zext-to-instruction
-- http://llvm.org/docs/LangRef.html#sext-to-instruction
-- http://llvm.org/docs/LangRef.html#fptrunc-to-instruction
-- http://llvm.org/docs/LangRef.html#fpext-to-instruction
-- http://llvm.org/docs/LangRef.html#fptoui-to-instruction
-- http://llvm.org/docs/LangRef.html#fptosi-to-instruction
-- http://llvm.org/docs/LangRef.html#uitofp-to-instruction
-- http://llvm.org/docs/LangRef.html#sitofp-to-instruction
cast :: (Cast a, Cast b) => V a -> I b
cast x = b
  where
    b = case (cst $ return x, cst b) of
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
      _ -> return $ castV x -- Types are LLVM equivalent.  Just change the EDSL type.
    to s = assign [s, tyvalof x, "to", ty b]

-- | Internal function used for LLVM function definitions. The
-- argument with type (M b -> String) must be a function that doesn't
-- evaluate it's argument.
-- http://llvm.org/docs/LangRef.html#functions
define :: Args a => String -> (a -> M b) -> (M b -> String) -> (b -> String) -> M ()
define n f g h = whenUnknown n $ do
  x <- instantiate
  instr ["define", g $ f x, global n, params $ tyvalof <$> unArgs x]
  output "{"
  b <- f x
  instr ["ret", h b]
  output "}"

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

class Ret r where
  -- | Function definition.  Function definitions are generated
  -- automatically.
  -- http://llvm.org/docs/LangRef.html#functions
  func :: (Args a) => String -> (a -> M r) -> a -> M r
  -- | Foreign function interface.  Function declarations are
  -- generated automatically.
  -- http://llvm.org/docs/LangRef.html#functions
  ffi :: (Args a) => String -> a -> M r
  -- | Internal function used for LLVM procedure call instruction.
  -- http://llvm.org/docs/LangRef.html#call-instruction
  call :: (Args a) => String -> a -> M r
  -- | 'if' function (works for both expressions and statements).
  if' :: I Bool -> M r -> M r -> M r

instance Ty b => Ret (V b) where
  func n f a = define n f ty tyvalof >> call n a
  ffi n x = let b = declare n (ty b) x >> call n x in b
  call n x = b
    where
      b = assign ["call", ty b, global n, params $ tyvalof <$> unArgs x]
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
  
instance Ret () where
  func n f a = define n f (const "void") (const "void") >> call n a
  ffi n a = declare n "void" a >> call n a
  call n x = instr ["call", "void", global n, params $ tyvalof <$> unArgs x]
  if' x y z = do
    a <- x
    true_lbl <- newLabel
    false_lbl <- newLabel
    done_lbl <- newLabel
    br a true_lbl false_lbl
    block true_lbl
    y
    br' done_lbl
    block false_lbl
    z
    br' done_lbl
    block done_lbl
  
-- | Internal function used for LLVM function declarations.
-- functions.  http://llvm.org/docs/LangRef.html#functions
declare :: (Args a) => String -> Type -> a -> M ()
declare n t x = whenUnknown n $
  instr ["declare", t, global n, params $ tyof <$> unArgs x ]

-- | LLVM alloca instruction with the number of elements.
-- http://llvm.org/docs/LangRef.html#alloca-instruction
alloca :: (Ty a, IsInt b) => V b -> I (Ptr a)
alloca n = let a = assign ["alloca", init (ty a) `comma` tyvalof n] in a
-- ^ 'init' here removes the '*' symbol from the type

-- | LLVM load instruction. The documentation seems to be wrong.  What
-- works for me is: %val = load i32* %ptr, not %val = load i32, i32*.
-- %ptr.  http://llvm.org/docs/LangRef.html#load-instruction
load :: Ty a => V (Ptr a) -> I a
load x = assign ["load", tyvalof x]

-- | LLVM store instruction.
-- http://llvm.org/docs/LangRef.html#store-instruction
store :: Ty a => V a -> V (Ptr a) -> M ()
store x p = instr ["store", tyvalof x `comma` tyvalof p]

-- | LLVM getelementptr instruction.  Docs seem to be wrong.
-- http://llvm.org/docs/LangRef.html#getelementptr-instruction
getelementptr :: (Ty a, IsInt b) => V (Ptr a) -> V b -> I (Ptr a)
getelementptr p i = assign ["getelementptr", tyvalof p `comma` tyvalof i]

-- | Generate text from our monad.  This is the program generation
-- entry point.
-- | As an example:
--
-- >>> mainM "t.ll" $ \(argc, _argv) -> sub argc (lit 1)
-- 
-- define i32 @main (i32 %v0, i8** %v1)
-- {
-- %v2 = sub i32 %v0, 1
-- ret i32 %v2
-- }
mainM :: String -> ((V Int32, V (Ptr (Ptr Char))) -> I Int32) -> IO ()
mainM filename f = do
  let st = execState (define "eel_main" f ty tyvalof) $ St initContext [] [] []
  writeFile filename $ unlines $ concat $ reverse $ snd <$> namespace st
      
-- | Quick and dirty representation of LLVM labels.
-- http://llvm.org/docs/LangRef.html#label-type
type Label = String

-- | Helper used for branching to labels.
label :: Label -> String
label = (++) "label %"

-- | Start a new block.  There should always be a br preceding this.
block :: Label -> M ()
block x = do
  modify $ \st -> st{ blocks = x : blocks st }
  instr [x ++ ":"]

-- | Get the current label from the stack of labels.
currBlock :: M Label
currBlock = do
  xs <- gets blocks
  case xs of
    [] -> error "currBlock:empty stack"
    x:_ -> return x

-- | Remove the current label from the stack of labels.
popBlock :: M ()
popBlock = modify $ \st -> st
  { blocks = case blocks st of
      [] -> error "popBlock:empty stack"
      _:xs -> xs
  }

-- | LLVM conditional br instruction.  There should always be a block following this.
-- http://llvm.org/docs/LangRef.html#br-instruction
br :: V Bool -> Label -> Label -> M ()
br x y z = popBlock >> instr ["br", tyvalof x `comma` label y `comma` label z]

-- | LLVM unconditional br instruction.  There should always be a
-- block following this.
-- http://llvm.org/docs/LangRef.html#br-instruction
br' :: Label -> M ()
br' x = popBlock >> instr ["br", label x]

-- | LLVM phi instruction.
-- http://llvm.org/docs/LangRef.html#phi-instruction
phi :: Ty a => [(V a, Label)] -> I a
phi xs = a
  where
    a = assign ["phi", ty a, commas [ "[" ++ valof b `comma` "%" ++ c ++ "]"
                                    | (b,c) <- xs ] ]
  
-- | Construct a local unique label.
newLabel :: M Label
newLabel = do
  cxt <- modifyCxt $ \cxt -> cxt{ nextLabel = succ $ nextLabel cxt }
  return $ "L" ++ show (nextLabel cxt)

-- | Helper function for LLVM assignment instructions.
assign :: Ty a => [String] -> I a
assign xs = do
  a <- instantiate
  instr $ [valof a, "="] ++ xs
  return a

-- | Output an LLVM instruction.
instr :: [String] -> M ()
instr = output . unwords
  
-- | Helper function to print out LLVM parameters.
params :: [String] -> String
params xs = "(" ++ commas xs ++ ")"

-- | Helper function to comma separate strings.
commas :: [String] -> String
commas [] = ""
commas xs = foldr1 comma xs

-- | Helper function to print out LLVM global names.
global :: String -> String
global = (++) "@"

-- | Helper used for comma separated things.
comma :: String -> String -> String
comma x y = x ++ ", " ++ y
