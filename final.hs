{-# LANGUAGE GADTs,FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

-- Abstract Syntax Definitions
data KULang where
    Num :: Int -> KULang  
    Boolean :: Bool -> KULang
    Plus :: KULang -> KULang -> KULang 
    Minus :: KULang -> KULang -> KULang
    Mult :: KULang -> KULang -> KULang 
    Div :: KULang -> KULang -> KULang  
    Exp :: KULang -> KULang -> KULang
    If0 :: KULang -> KULang -> KULang -> KULang
    Id :: String -> KULang
    Lambda :: String -> KULang -> KULang 
    App :: KULang -> KULang -> KULang 
    And :: KULang -> KULang -> KULang   
    Or :: KULang -> KULang -> KULang  
    Leq :: KULang -> KULang -> KULang  
    IsZero :: KULang -> KULang  
    If :: KULang -> KULang -> KULang -> KULang  
    Between :: KULang -> KULang -> KULang -> KULang
    deriving (Show,Eq)

data KULangVal where
    NumV :: Int -> KULangVal
    ClosureV :: String -> KULang -> EnvVal -> KULangVal
    deriving (Show,Eq)

data KULangExt where
    NumX :: Int -> KULangExt
    PlusX :: KULangExt -> KULangExt -> KULangExt
    MinusX :: KULangExt -> KULangExt -> KULangExt
    MultX :: KULangExt -> KULangExt -> KULangExt
    DivX :: KULangExt -> KULangExt -> KULangExt
    ExpX :: KULangExt -> KULangExt -> KULangExt
    If0X :: KULangExt -> KULangExt -> KULangExt -> KULangExt
    LambdaX :: String -> KULangExt -> KULangExt
    AppX :: KULangExt -> KULangExt -> KULangExt 
    BindX :: String -> KULangExt -> KULangExt -> KULangExt
    IdX :: String -> KULangExt
    deriving (Show,Eq)

-- Environment Definitions
type Env = [(String,KULang)]
type EnvVal = [(String,KULangVal)]

-- Reader Definition
data Reader e a = Reader (e -> a)

-- Monad Definition
instance Monad (Reader e) where
 return x = Reader $ \e -> x 
 g >>= f = Reader $ \e -> runR (f (runR g e)) e 

 -- Applicative Definition
instance Applicative (Reader e) where
 pure x = Reader $ \e -> x
(Reader f) <*> (Reader g) = Reader $ \e -> (f e) (g e)

-- Functor Definition
instance Functor (Reader e) where
 fmap f (Reader g) = Reader $ \e -> (f . g) e

-- Fail Definition
instance MonadFail (Reader e) where
        fail = error "fail"

-- Helper Methods
runR :: Reader e a -> e -> a
runR (Reader f) e = f e 

ask :: Reader a a 
ask = Reader $ \e -> e

local :: (e->t) -> Reader t a -> Reader e a 
local f r = ask >>= \e -> return (runR r (f e))

useClosure :: String -> KULangVal -> EnvVal -> EnvVal -> EnvVal
useClosure i v e _ = (i,v):e 


-----------------------------
----- Project Exercises -----
-----------------------------

-- Part 1: Scoping

-- Exercise 1:
evalDyn :: Env -> KULang -> (Maybe KULang)
evalDyn e (Num x) = if x<0 then Nothing else Just (Num x)

evalDyn e (Plus l r) = do {
  Num x <- evalDyn e l;
  Num y <- evalDyn e r;
  return (Num (x+y))
}

evalDyn e (Minus l r) = do {
  Num x <- evalDyn e l;
  Num y <- evalDyn e r;
  if (x-y)<0 then Nothing else Just (Num (x-y))
}

evalDyn e (Mult l r) = do {
  Num x <- evalDyn e l;
  Num y <- evalDyn e r;
  return (Num (x*y))
}

evalDyn e (Div l r) = do {
  Num x <- evalDyn e l;
  Num y <- evalDyn e r;
  if y == 0 then Nothing else return (Num (x `div` y)) 
}

evalDyn e (Exp l r) = do {
  Num x <- evalDyn e l;
  Num y <- evalDyn e r;
  return (Num (x^y))
}

evalDyn e (If0 c t f) = do {
    c' <- evalDyn e c;
    if c' == Num 0 then evalDyn e t else evalDyn e f;
} 

evalDyn e (Id id) = lookup id e

evalDyn e (Lambda i b) = return (Lambda i b)

evalDyn e (App f a) = do {
    (Lambda i b) <- evalDyn e f;
    v <- evalDyn e a;
    evalDyn ((i,v):e) b;
} 

-- Exercise 2:
evalStat :: EnvVal -> KULang -> (Maybe KULangVal)
evalStat e (Num x) = if x<0 then Nothing else Just (NumV x)

evalStat e (Plus l r) = do {
  NumV x <- evalStat e l;
  NumV y <- evalStat e r;
  return (NumV (x+y))
}

evalStat e (Minus l r) = do {
  NumV x <- evalStat e l;
  NumV y <- evalStat e r;
  if (x-y)<0 then Nothing else Just (NumV (x-y))
}

evalStat e (Mult l r) = do {
  NumV x <- evalStat e l;
  NumV y <- evalStat e r;
  return (NumV (x*y))
}

evalStat e (Div l r) = do {
  NumV x <- evalStat e l;
  NumV y <- evalStat e r;
  if y == 0 then Nothing else return (NumV (x `div` y)) 
}

evalStat e (Exp l r) = do {
  NumV x <- evalStat e l;
  NumV y <- evalStat e r;
  return (NumV (x^y))
}

evalStat e (If0 c t f) = do {
    c' <- evalStat e c;
    if c' == NumV 0 then evalStat e t else evalStat e f;
} 

evalStat e (Id id) = lookup id e

evalStat e (Lambda i b) = return (ClosureV i b e)

evalStat e (App f a) = do {
    (ClosureV i b j) <- evalStat e f;
    v <- evalStat e a;
    evalStat ((i,v):j) b;
}

-- Part 2: Elaboration

-- Exercise 3:
elabTerm :: KULangExt -> KULang 
elabTerm (NumX n) = (Num n)
elabTerm (PlusX l r) = (Plus (elabTerm l) (elabTerm r))
elabTerm (MinusX l r) = (Minus (elabTerm l) (elabTerm r))
elabTerm (MultX l r) = (Mult (elabTerm l) (elabTerm r))
elabTerm (DivX l r) = (Div (elabTerm l) (elabTerm r))
elabTerm (ExpX l r) = (Exp (elabTerm l) (elabTerm r))
elabTerm (If0X c t f) = (If0 (elabTerm c) (elabTerm t) (elabTerm f))
elabTerm (LambdaX i b) = (Lambda i (elabTerm b))
elabTerm (IdX x) = (Id x)
elabTerm (AppX f a) = (App (elabTerm f) (elabTerm a))
elabTerm (BindX i v b) = (App (Lambda i (elabTerm b)) (elabTerm v))

-- Exercise 4:
interpElab :: EnvVal -> KULangExt -> (Maybe KULangVal)
interpElab e t = evalStat e (elabTerm t)

-- Part 3: Reader Monad

-- Exercise 5:
evalReader :: KULang -> Reader EnvVal KULangVal
evalReader (Num x) = if x<0 then error "fail" else return (NumV x)

evalReader (Plus l r) = do {
  NumV x <- evalReader l;
  NumV y <- evalReader r;
  return (NumV (x+y))
}

evalReader (Minus l r) = do {
  NumV x <- evalReader l;
  NumV y <- evalReader r;
  if (x-y)<0 then error "fail" else return (NumV (x-y))
}

evalReader (Mult l r) = do {
  NumV x <- evalReader l;
  NumV y <- evalReader r;
  return (NumV (x*y))
}

evalReader (Div l r) = do {
  NumV x <- evalReader l;
  NumV y <- evalReader r;
  if y == 0 then error "fail" else return (NumV (x `div` y)) 
}

evalReader (Exp l r) = do {
  NumV x <- evalReader l;
  NumV y <- evalReader r;
  return (NumV (x^y))
}

evalReader (If0 c t f) = do {
    c' <- evalReader c;
    if c' == NumV 0 then evalReader t else evalReader f;
} 

evalReader (Id id) = do {
  env <- ask;
    case lookup id env of 
      Nothing -> error "fail"
      Just x -> return x
}

evalReader (Lambda i b) = do {
  env <- ask;
  return (ClosureV i b env)
}

evalReader (App f a) = do {
  (ClosureV i b e) <- evalReader f;
  v <- evalReader a;
  local (useClosure i v e) (evalReader b)
}

-- Exercise 6:
interpReader :: KULangExt -> KULangVal
interpReader x = runR (evalReader (elabTerm x)) []