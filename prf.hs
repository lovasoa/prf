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

dependOnArg :: Int -> Prf -> Bool
dependOnArg _ C0 = False
dependOnArg _ S  = True
dependOnArg k (P i n) = k + 1 == i
dependOnArg k (Compose f gs) = any (dependOnArg k) gs
dependOnArg 0 (Recurse _ (P 2 _)) = False
dependOnArg 0 g = True
dependOnArg k (Recurse f g) = dependOnArg (k-1) f && dependOnArg (k+1) g

data Optimized = OConst Int | OAdd | OSub | OMul | OIf | OAddNullArgs Int Optimized
                  | ORec Optimized Optimized
                  | OComp Optimized [Optimized] | OProj Int Int
                  deriving (Eq, Show)

addNullArg :: Optimized -> Optimized
addNullArg (OProj i n) = OProj (i+1) (n+1)
addNullArg (OAddNullArgs n o) = OAddNullArgs (n+1) o
addNullArg o = OAddNullArgs 1 o

-- optimize composition
mkComp :: Optimized -> [Optimized] -> Optimized
-- f(a(g,h), b(g,h)) = (f(a,b))(g,h)
mkComp (OComp f ab) gh = OComp f $ map (flip mkComp gh) ab
mkComp (OConst c) gs = OConst c
mkComp (OProj i n) gs = gs !! (i-1)
mkComp f gs = OComp f gs

optimize :: Prf -> Optimized
-- Use addition instead of a series of S
optimize (Compose S [f]) =
  case optimize f of
    OConst i -> OConst (i+1)
    OComp OAdd ((OConst c):others) -> OComp OAdd ((OConst (c+1)):others)
    OComp OAdd others -> OComp OAdd ((OConst 1):others)
    other -> OComp OAdd [(OConst 1), other]
-- When composing with a projection, delete the projection
optimize (Compose (P i n) gs) = optimize (gs !! (i-1))
-- Detect constant null function defined as recursive function
optimize (Recurse C0 (P 2 n)) = OConst 0
-- Optimize addition
optimize (Recurse (P 1 1) (Compose S [P 2 3])) = OAdd
optimize (Recurse f (Compose S [P 2 n])) = OComp OAdd [OProj 1 (n-1), addNullArg (optimize f)]
-- Optimize precedent
optimize (Recurse C0 (P 1 2)) = OComp OSub [OConst 1, OProj 1 1]
optimize (Recurse f g) | not $ dependOnArg 1 g = let
  n = 1 + arity f
  gcomp =  (OComp OSub [OConst 1, OProj 1 n]):(OConst 0):[OProj i n | i <- [2..n]]
  gopt = mkComp (optimize g) gcomp
  fopt = addNullArg $ optimize f
  in OComp OIf [OProj 1 n, gopt, fopt]
-- Optimize multiplication
optimize (Recurse f g) = case (optimize f, optimize g) of
  (OConst 0, OComp OAdd [OProj 2 3,OProj 3 3]) -> OMul
  (OProj 1 1, OComp OSub [OConst 1, OProj 2 3]) -> OSub
  (optf, optg) -> ORec optf optg
optimize C0 = OConst 0
optimize S  = OComp OAdd [OConst 1, OProj 1 1]
optimize (P i n) = OProj i n
optimize (Compose f g) = mkComp (optimize f) (map optimize g)

-- Optimize a function and execute it
exec :: Prf -> [Int] -> Int
exec = execopt . optimize

-- Execute optimized function
execopt :: Optimized -> [Int] -> Int
execopt (OConst n) _ = n
execopt OAdd ints = sum ints
execopt OSub [a,b] = if b > a then b-a else 0
execopt OMul ints = product ints
execopt (OProj i n) ints = ints !! (i-1)
execopt (OComp f gs) ints = execopt f $ map (flip execopt ints) gs
execopt (ORec f g) (n:ints) = let
  init = execopt f ints
  folder res i = execopt g (i:res:ints)
  in foldl folder init [0..(n-1)]
execopt (OAddNullArgs n f) ints = execopt f ((replicate n 0)++ints)
execopt OIf [a,b,c] = if a/=0 then b else c
execopt other ints = error ("unimplemented execution of " ++ (show other) ++ (show ints))

-- Execute a function without optimizing it
execprf :: Prf -> [Int] -> Int
execprf C0 _ = 0
execprf S [n] = n `seq` n + 1
execprf (P i n) args | n == length args = args `seq` args !! (i - 1)
execprf (Compose f gs) args = execprf f $! map (flip execprf args) gs
execprf (Recurse f g)  (n:args) = let
                                  folder res i = execprf g (i:res:args)
                                  init = execprf f args
                               in init `seq` foldl folder init [0..(n-1)]
execprf prog args = traceShow (prog,args) (error "Invalid arguments for this Program")

multiExec :: Prf -> [[Int]] -> [Maybe Int]
multiExec pgm intss =
  let
    ar = arity pgm
    optimized = optimize pgm
    mapper ints | length ints == ar = Just (execopt optimized ints)
    mapper ints | otherwise         = Nothing
  in optimized `seq` map mapper intss

fromParse :: PrfParse.Prf -> Prf
fromParse f = case f of
  PrfParse.C0 -> C0
  PrfParse.S  -> S
  PrfParse.P a b  -> P a b
  PrfParse.Compose f gs -> Compose (fromParse f) (map fromParse gs)
  PrfParse.Recurse f g  -> Recurse (fromParse f) (fromParse g)
  PrfParse.Fun fname    -> error ("No such function: "++fname)

parse = fromParse . PrfParse.parse

