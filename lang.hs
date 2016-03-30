module PrfParse (Prf(..),parse) where
import Text.ParserCombinators.ReadP
import Data.Char
import Data.Maybe
import Control.Monad

data Prf = C0 | S | P Int Int |
            Compose Prf [Prf] | 
            Recurse Prf Prf |
            Fun String
            deriving (Show, Read)
rConst name val = do
  string name
  return val

rFunName funfun = do
  s <- munch1 isLower
  return $ funfun s

rProj = do
  char 'P'
  i <- munch1 isDigit
  char ','
  n <- munch1 isDigit
  return $ P (read i) (read n)

rComp = do
  funct1 <- functSimple
  char '('
  args <- sepBy funct (char ',')
  return $ Compose funct1 args

rRec = do
  char '{'
  p1 <- functSimple
  char '|'
  p2 <- functSimple
  char '}'
  return $ Recurse p1 p2

brackets p = do
  char '('
  r <- p
  char ')'
  return r

functSimple = do
  skipSpaces
  res <- rConst "C0" C0 +++ rConst "S" S +++ rProj +++ rFunName Fun +++ brackets funct
  skipSpaces
  return res

funct = do
  res <- functSimple +++ rComp +++ rRec
  return res

varBind = do
  name <- rFunName id
  skipSpaces
  char '='
  skipSpaces
  p <- funct
  return (name, p)

pgm = do
  vars <- sepBy varBind (char '\n')
  res <- funct
  return (vars, res)

replaceVars vars fun = case fun of
  Fun s        -> fromMaybe (Fun s) (lookup s vars)
  Compose f fs -> Compose (replaceVars vars f) (map (replaceVars vars) fs)
  Recurse f g  -> Recurse (replaceVars vars f) (replaceVars vars g)
  f            -> f

replaceNestedVars vars fun = let
  folder vs (fname, f) = (fname, replaceVars vs f):vs
  in flip replaceVars fun $ foldl folder [] vars

parse :: String -> Prf
parse = uncurry replaceNestedVars . fst . last . readP_to_S pgm

main = do
  inp <- getContents
  print $ parse inp
