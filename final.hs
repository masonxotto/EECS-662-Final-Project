{-# LANGUAGE GADTs,FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

-- Abstract Syntax Definitions
data KUTypeLang where
    TNum :: KUTypeLang
    TBool :: KUTypeLang
    (:->:) :: KUTypeLang -> KUTypeLang -> KUTypeLang 
    deriving (Show, Eq)

data KULang where
    Num :: Int -> KULang  
    Boolean :: Bool -> KULang
    -- (:->:) :: KULangVal -> KULangVal -> KULang
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
    Fix :: KULang -> KULang
    deriving (Show,Eq)

data KULangVal where
    NumV :: Int -> KULangVal
    BooleanV :: Bool -> KULangVal
    ClosureV :: String -> KULang -> EnvVal -> KULangVal
    (:->:) :: KULangVal -> KULangVal -> KULangVal
    UnitV :: KULangVal -- New Feature
    deriving (Show,Eq)

-- Environment & Gamma Definitions
type Env = [(String,KULang)]
type EnvVal = [(String,KULangVal)]
type Gamma = [(string, KUTypeLang)]

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

subst :: String -> KULang -> KULang -> KULang
subst i v (Num x) = Num x
subst i v (Boolean b) = Boolean b
subst i v (Plus l r) = Plus (subst i v l) (subst i v r)
subst i v (Minus l r) = Minus (subst i v l) (subst i v r)
subst i v (Mult l r) = Mult (subst i v l) (subst i v r)
subst i v (Div l r) = Div (subst i v l) (subst i v r)
subst i v (Exp l r) = Exp (subst i v l) (subst i v r)
subst i v (And l r) = And (subst i v l) (subst i v r)
subst i v (Or l r) = Or (subst i v l) (subst i v r)
subst i v (Leq l r) = Leq (subst i v l) (subst i v r)
subst i v (IsZero x) = IsZero (subst i v x)
subst i v (If c t e) = If (subst i v c) (subst i v t) (subst i v e)
subst i v (Between l c r) = Between (subst i v l) (subst i v c) (subst i v r)
subst i v (Id i') = if i == i' then v else Id i'
subst i v (Bind i' v' b') = if i == i'
    then Bind i' (subst i v v') b'
    else Bind i' (subst i v v') (subst i v b')
subst i v (Fix f) = Fix (subst i v f)
subst x t (Lambda i b) = if x==i 
  then Lambda i b
  else Lambda i (subst x t b)
subst x t (App f a) = App (subst x t f) (subst x t a)


-- Implementing Types
typeofMonad :: Gamma -> KULang -> Maybe KUTypeLang
typeofMonad _ (Num n) = if n>=0 then return TNum else Nothing
typeofMonad _ (Boolean b) = return TBool
typeofMonad g (Plus l r) = do {TNum <- typeofMonad g l;
                             TNum <- typeofMonad g r;
                             return TNum}
                             
typeofMonad g (Minus l r) = do {TNum <- typeofMonad g l;
                              TNum <- typeofMonad g r;
                              return TNum}

typeofMonad g (Mult l r) = do {TNum <- typeofMonad g l;
                             TNum <- typeofMonad g r;
                             return TNum}

typeofMonad g (Div n d) = do {TNum <- typeofMonad g n;
                            TNum <- typeofMonad g d;
                            return TNum}

typeofMonad g (Exp x n) = do {TNum <- typeofMonad g x;
                            TNum <- typeofMonad g n;
                            return TNum}

typeofMonad g (Id x) = lookup x g 

typeofMonad g (Lambda i d b) = do {r <- typeofMonad (i,d):g b;
                                    return d :->: r}

typeofMonad g (App f a) = do {a' <- typeofMonad g a;
                              d :->: r <- typeofMonad g f;
                              if a'==d then return r else Nothing}

typeofMonad g (And l r) = do {TBool<-typeofMonad g l;
                            TBool<-typeofMonad g r;
                            return TBool}

typeofMonad g (Or l r) = do {TBool<-typeofMonad g l;
                            TBool<-typeofMonad g r;
                            return TBool}

typeofMonad g (Leq l r) = do {TNum<-typeofMonad g l;
                            TNum<-typeofMonad g r;
                            return TBool}

typeofMonad g (IsZero x) = do{TNum<-typeofMonad g x;
                            return TBool}

typeofMonad g (If c t e) = do{TBool<-typeofMonad g c;
                            t'<- typeofMonad g t;
                            e'<- typeofMonad g e;
                            if t' == e' then return t' else Nothing}

typeofMonad g (Between s m e) = do{TNum<- typeofMonad g s;
                                 TNum<- typeofMonad g m;
                                 TNum<- typeofMonad g e';
                                 return TBool}

typeofMonad g (Fix x) = 

typeofMonad g (Bind i v b) = do {tv <- typeofMonad g v;
                                 typeofMonad (i,tv):g b}

typeofMonad _ = Nothing


-- Part 1: Scoping
--Evaluation 
eval :: EnvVal -> KULang -> Maybe KULangVal

eval e (Num x) = if x<0 then Nothing else Just (NumV x)

eval e (Boolean b) = Just (BooleanV b)

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

--Fixed Point Operator 
eval e (Fix f) = do {
  (ClosureV i b j) <- eval e f;
  eval j (subst i (Fix (Lambda i b)) b)}

--Type Inference
-- typeInfer :: TypeEnv -> KULang -> Maybe KUTypeLangVal


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
