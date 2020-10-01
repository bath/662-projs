{-# LANGUAGE GADTs,FlexibleContexts #-}

-- Imports for Monads

import Control.Monad

-- BBAE AST and Type Definitions

data TBBAE where
  TNum :: TBBAE
  TBool :: TBBAE
  deriving (Show,Eq)

data BBAE where
  Num :: Int -> BBAE
  Plus :: BBAE -> BBAE -> BBAE
  Minus :: BBAE -> BBAE -> BBAE
  Bind :: String -> BBAE -> BBAE -> BBAE
  Id :: String -> BBAE
  Boolean :: Bool -> BBAE
  And :: BBAE -> BBAE -> BBAE
  Leq :: BBAE -> BBAE -> BBAE
  IsZero :: BBAE -> BBAE
  If :: BBAE -> BBAE -> BBAE -> BBAE
  deriving (Show,Eq)

type Env = [(String,BBAE)]

type Cont = [(String,TBBAE)]

evalS :: BBAE -> (Maybe BBAE)
evalS (Num n) = Just (Num n)
evalS (Plus l r) = do {(Num l') <- evalS l;
                       (Num r') <- evalS r;
                       return (Num (l' + r'))}
evalS (Minus l r) = do {(Num l') <- evalS l;
                        (Num r') <- evalS r;
                       if l' >= r' then (Num (l' - r'))
                                   else Nothing}
evalS (Bind s a b) = do {a' <- eval a;
                         (eval (subst s a' b))}
evalS (Id a) = Nothing
evalS (Boolean b) = 
evalS _ = Nothing


evalM :: Env -> BBAE -> (Maybe BBAE)
evalM _ _ = Nothing

testBBAE :: BBAE -> Bool
testBBAE _ = True

typeofM :: Cont -> BBAE -> (Maybe TBBAE)
typeofM _ _ = Nothing

evalT :: BBAE -> (Maybe BBAE)
evalT _ = Nothing
