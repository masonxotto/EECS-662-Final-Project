{-# LANGUAGE GADTs,FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
import Data.Function (fix)

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
    Bind :: String -> KULang -> KULang -> KULang
    Between :: KULang -> KULang -> KULang -> KULang
    deriving (Show,Eq)

data KULangVal where
    NumV :: Int -> KULangVal
    BooleanV :: Bool -> KULangVal
    ClosureV :: String -> KULang -> EnvVal -> KULangVal
    UnitV :: KULangVal
    deriving (Show,Eq)

data KUTypeLang where
    TNum :: KUTypeLang
    TBool :: KUTypeLang
    TVal :: String -> KUTypeLang
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

--Evaluation 
eval :: EnvVal -> KULang -> (Maybe KULangVal)
eval e (Num x) = if x<0 then Nothing else Just (NumV x)

eval e(Boolean b) = Just (BooleanV b)

eval e (Plus l r) = do {
  NumV x <- eval e l;
  NumV y <- eval e r;
  return (NumV (x+y))
}

eval e (Minus l r) = do {
  NumV x <- eval e l;
  NumV y <- eval e r;
  if (x-y)<0 then Nothing else Just (NumV (x-y))
}

eval e (Mult l r) = do {
  NumV x <- eval e l;
  NumV y <- eval e r;
  return (NumV (x*y))
}

eval e (Div l r) = do {
  NumV x <- eval e l;
  NumV y <- eval e r;
  if y == 0 then Nothing else return (NumV (x `div` y)) 
}

eval e (Exp l r) = do {
  NumV x <- eval e l;
  NumV y <- eval e r;
  return (NumV (x^y))
}

eval e (If0 c t f) = do {
    c' <- eval e c;
    if c' == NumV 0 then eval e t else eval e f;
} 

eval e (Id id) = lookup id e

eval e (Lambda i b) = return (ClosureV i b e)

eval e (App f a) = do {
    (ClosureV i b j) <- eval e f;
    v <- eval e a;
    eval ((i,v):j) b;
}

eval e (And l r) = do {
    BooleanV x <- eval e l;
    BooleanV y <- eval e r;
    return (BooleanV (x && y))
}

eval e (Or l r) = do {
    BooleanV x <- eval e l;
    BooleanV y <- eval e r;
    return (BooleanV (x || y))
}

eval e (Leq l r) = do {
    NumV x <- eval e l;
    NumV y <- eval e r;
    if x <= y then return (BooleanV True) else return (BooleanV False)
}

eval e (IsZero x) = do {
    NumV y <- eval e x;
    if y == 0 then return (BooleanV True) else return (BooleanV False)
}

eval e (If c t e') = do {
    BooleanV c' <- eval e c;
    if c' then eval e t else eval e e'
}

eval e (Between l c r) = do {
    NumV l' <- eval e l;
    NumV c' <- eval e c;
    if c' > l' then do {
        NumV r' <- eval e r;
        if r' > c' then return (BooleanV True) else return (BooleanV False)
    } else return (BooleanV False)
}

eval e (Bind i v b) = eval e (App (Lambda i b) v)

--Type Inference
typeInfer :: TypeEnv -> KULang -> Maybe KUTypeLangVal


--Fixed Point Operator 
fixedPoint :: Eq a => (a -> a) -> a -> a
fixedPoint f x = fix (\g y -> if f y == y then y else g (f y)) x

--Reader Monad
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

--Interpretation 
interpret :: KULang -> (Maybe KULangVal)
-- interpret expr = if typeofMonad [] expr == Nothing then Nothing else evalDeferred [] expr
interpret str = eval [] str

-- interpReader :: KULang -> KULangVal
-- interpReader x = runR (evalReader x) []