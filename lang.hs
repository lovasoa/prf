module PrfParse (Prf(..),parse) where
import Text.ParserCombinators.ReadP
import Data.Char
import Data.Maybe
import Control.Monad
import Debug.Trace

data Prf = C0 | S | P Int Int |
            Compose Prf [Prf] | 
            Recurse Prf Prf |
            Fun String
            deriving (Show, Read, Eq)
rConst name val = do
  string name
  return val

rFunName funfun = do
  s0 <- satisfy isLower
  s1 <- munch isAlphaNum
  return $ funfun (s0:s1)

rProj = do
  char 'P'
  i <- munch1 isDigit
  char ':'
  n <- munch1 isDigit
  return $ P (read i) (read n)

rComp = do
  funct1 <- functSimple
  char '('
  skipSpaces
  args <- sepBy funct (skipSpaces >> char ',' >> skipSpaces)
  skipSpaces
  char ')'
  return $ Compose funct1 args

rRec = do
  char '{'
  skipSpaces
  p1 <- funct
  skipSpaces
  char '|'
  skipSpaces
  p2 <- funct
  skipSpaces
  char '}'
  return $ Recurse p1 p2

brackets p = do
  char '('
  skipSpaces
  r <- p
  skipSpaces
  char ')'
  return r

functSimple = do
  res <- rConst "C0" C0 +++ rConst "S" S +++ rProj +++ rFunName Fun +++ brackets funct
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

comment = do
  char '#'
  munch (/='\n')

pgmLine = do
  var <- option Nothing (varBind >>= return . return)
  munch (`elem` "\t ")
  optional comment
  char '\n'
  return var

pgm = do
  vars <- many pgmLine
  res <- funct
  skipSpaces
  return (catMaybes vars, res)

replaceVars vars fun = case fun of
  Fun s        -> fromMaybe (Fun s) (lookup s vars)
  Compose f fs -> Compose (replaceVars vars f) (map (replaceVars vars) fs)
  Recurse f g  -> Recurse (replaceVars vars f) (replaceVars vars g)
  f            -> f

replaceNestedVars vars fun = let
  folder vs (fname, f) = (fname, replaceVars vs f):vs
  in flip replaceVars fun $ foldl folder [] vars

parse :: String -> Prf
parse codetxt = let
    parses = readP_to_S pgm codetxt
  in
  if parses == [] then
    error "Invalid Syntax!"
  else let (parsed, rest) = last parses in
    if rest == "" then uncurry replaceNestedVars parsed
    else error ("Syntax error after: «" ++ (take 10 rest) ++ " [...]».")

main = do
  inp <- getContents
  print $ parse inp
