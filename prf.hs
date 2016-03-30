module Prf (Err, Prf, parse, arity, check, exec, multiExec) where
import qualified PrfParse
import Data.Maybe
import Debug.Trace

data Prf = C0 | S | P Int Int |
            Compose Prf [Prf] | 
            Recurse Prf Prf
            deriving (Show, Read, Eq)

data Err = EProj Int Int | EComp Int Int | ERecur Int Int | ECompList [Int]
instance Show Err where
  show (EProj i n) = "Invalid projection: projection of argument n°"++
                      (show i)++" of "++(show n)
  show (ECompList arities) = "Invalid composition: not all composing functions accept the same number of arguments: "++
                              (show arities)

  show (EComp farity glen) = "Invalid composition: composing a "++
                              (show farity) ++"-ary function with "++
                              (show glen) ++ " functions"
  show (ERecur farity garity) = "Invalid recursion: if the initial function is "++
                                  (show farity) ++"-ary, the recursion function cannot be " ++
                                  (show garity) ++ "-ary"

arity :: Prf -> Int
arity C0    = 0
arity S     = 1
arity (P i n) = n
arity (Compose f gs) = fromMaybe 0 $ listToMaybe $ map arity gs
arity (Recurse f g)   = 1 + arity f

allSameArity :: [Prf] -> [Err]
allSameArity pgrs = 
  let
    arities = map arity pgrs
    diffarities = foldl (\l a -> if a `elem` l then l else a:l) [] arities
  in
    if length diffarities < 2 then [] else [ECompList arities]

-- Check if a program is valid
check :: Prf -> [Err]
check (P i n) | i > n = [EProj i n]
check (Compose f gs) = check f ++
                       concatMap check gs ++
                       (if arity f == length gs then [] else [EComp (arity f) (length gs)]) ++
                       (allSameArity gs)
check (Recurse f g)  = check f ++
                       check g ++
                       if 2 + arity f == arity g
                        then []
                        else [ERecur (arity f) (arity g)]
check _ = []

exec :: Prf -> [Int] -> Int
exec C0 _ = 0
exec S [n] = n + 1
exec (P i n) args | n == length args = args !! (i - 1)
exec (Compose f gs) args = exec f $ map (flip exec args) gs
exec (Recurse f g)  (n:args) = let folder res i = exec g (i:res:args)
                               in foldl folder (exec f args) [0..(n-1)]
exec prog args = traceShow (prog,args) (-1) -- Invalid arguments for this Program

multiExec :: Prf -> [[Int]] -> [Maybe Int]
multiExec pgm intss =
  let
    ar = arity pgm
    mapper ints | length ints == ar = Just (exec pgm ints)
    mapper ints | otherwise         = Nothing
  in map mapper intss

fromParse :: PrfParse.Prf -> Prf
fromParse f = case f of
  PrfParse.C0 -> C0
  PrfParse.S  -> S
  PrfParse.P a b  -> P a b
  PrfParse.Compose f gs -> Compose (fromParse f) (map fromParse gs)
  PrfParse.Recurse f g  -> Recurse (fromParse f) (fromParse g)
  PrfParse.Fun fname    -> error ("No such function: "++fname)

parse = fromParse . PrfParse.parse

