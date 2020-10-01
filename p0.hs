-- Miller Bath
-- 2817389, EECS 662 - P0
-- 9/15/20

{-# LANGUAGE GADTs, FlexibleContexts #-}

-- Imports for Parsec
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token

--
-- Simple caculator over naturals with no identifiers
--
-- Author: Perry Alexander
-- Date: Tue Jan 23 17:54:44 CST 2018
--
-- Source files for the Arithmetic Expressions (AE) language from PLIH
--

-- AST Definition

data AE where
  Num :: Int -> AE
  Plus :: AE -> AE -> AE
  Minus :: AE -> AE -> AE
  Mult :: AE -> AE -> AE
  Div :: AE -> AE -> AE
  If0 :: AE -> AE -> AE -> AE
  deriving (Show,Eq)

-- AE Parser (Requires ParserUtils and Parsec included above)

languageDef =
  javaStyle { identStart = letter
            , identLetter = alphaNum
            , reservedNames = [ "if0"
                              , "then"
                              , "else"
                              ]
            , reservedOpNames = [ "+","-","*","/"]
            }
  
lexer = makeTokenParser languageDef

inFix o c a = (Infix (reservedOp lexer o >> return c) a)
preFix o c = (Prefix (reservedOp lexer o >> return c))
postFix o c = (Postfix (reservedOp lexer o >> return c))

parseString p str =
  case parse p "" str of
    Left e -> error $ show e
    Right r -> r

expr :: Parser AE
expr = buildExpressionParser operators term

operators = [
  [ inFix "*" Mult AssocLeft
    , inFix "/" Div AssocLeft ]
  , [ inFix "+" Plus AssocLeft
  , inFix "-" Minus AssocLeft ]
  ]
  
numExpr :: Parser AE
numExpr = do i <- integer lexer
             return (Num (fromInteger i))

ifExpr :: Parser AE
ifExpr  = do reserved lexer "if0"
             c <- expr
             reserved lexer "then"
             t <- expr
             reserved lexer "else"
             e <- expr
             return (If0 c t e)

term = parens lexer expr
       <|> numExpr
       <|> ifExpr

-- Parser invocation
-- Call parseAE to parse a string into the AE data structure.

parseAE = parseString expr

-- Evaluation Functions
-- Replace the bodies of these functions with your implementations for
-- Exercises 1-4.  Feel free to add utility functions or testing functions as
-- you see fit, but do not change the function signatures.  Note that only
-- Exercise 4 requires you to integrate the parser above.

evalAE :: AE -> Int
evalAE (Num x) = x
evalAE (Plus l r) = evalAE l + evalAE r
evalAE (Minus l r) = evalAE l - evalAE r
evalAE (Mult l r) = evalAE l * evalAE r
evalAE (Div l r) = (evalAE l) `div` (evalAE r)
evalAE (If0 a b c) = if (evalAE a == 0)
                      then evalAE b 
                    else evalAE c

evalAEMaybe :: AE -> Maybe Int
evalAEMaybe (Num x) = Just x
evalAEMaybe (Plus l r) = case evalAEMaybe l of
                          Nothing -> Nothing
                          Just l' -> case evalAEMaybe r of
                            Nothing -> Nothing
                            Just r' -> Just (l' + r')
evalAEMaybe (Minus l r) = case evalAEMaybe l of
                            Nothing -> Nothing
                            Just l' -> case evalAEMaybe r of
                              Nothing -> Nothing
                              Just r' -> if r' <= l' then Just (l' - r') else Nothing
evalAEMaybe (Mult l r) = case evalAEMaybe l of
                            Nothing -> Nothing
                            Just l' -> case evalAEMaybe r of
                              Nothing -> Nothing
                              Just r' -> Just (l' * r')
evalAEMaybe (Div l r) = case evalAEMaybe l of
                            Nothing -> Nothing
                            Just l' -> case evalAEMaybe r of
                              Nothing -> Nothing
                              Just r' -> Just (l' `div` r')
evalAEMaybe (If0 a b c) = case evalAEMaybe a of
                            Nothing -> Nothing
                            Just a' -> if a' == 0
                              then case evalAEMaybe b of
                                Nothing -> Nothing
                                Just b' -> Just b'
                              else case evalAEMaybe c of
                                Nothing -> Nothing
                                Just c' -> Just c'

evalM :: AE -> Maybe Int
evalM (Num x) = do Just x
evalM (Plus l r) = do
  l' <- evalM l
  r' <- evalM r
  return (l' + r')
evalM (Minus l r) = do
  l' <- evalM l
  r' <- evalM r
  if (r' <= l') then return (l' - r') else Nothing
evalM (Mult l r) = do
  l' <- evalM l
  r' <- evalM r
  return (l' * r')
evalM (Div l r) = do
  l' <- evalM l
  r' <- evalM r
  if (r' <= 0) then Nothing else return (l' `div` r')
evalM (If0 a b c) = do
  a' <- evalM a
  if a' == 0 then evalM b else evalM c



interpAE :: String -> Maybe Int
interpAE x = evalM (parseAE x)

main :: IO()
main = do 
  -- test cases
  print $ evalAE (Mult (Num 3) (Num 5))
  print "expect 15"
  print $ evalAEMaybe (Div (Num 10) (Num 5))
  print "expect Just 2"
  print $ evalM (If0 (Num 0) (Div (Num 10) (Num 5)) (Num 5))
  print "expect Just 2"
  print $ interpAE "3"
  print "expect Just 3"
  return ()