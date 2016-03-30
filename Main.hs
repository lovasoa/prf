import Data.Maybe
import Prf
import System.Environment

getInputs :: String -> [[Int]]
getInputs = map (map read . words) . lines

showErrs :: [Err] -> IO Int
showErrs errs = do
  putStrLn "The following errors were found in your program: "
  sequence_ $ map (putStrLn.show) errs
  return 1

mainLoop :: Prf -> Int -> IO Int
mainLoop prog ar = do
  inputs <- getContents
  sequence_ $
    map (putStrLn . (fromMaybe ("The function expects "++(show ar)++" arguments!")).((Just . show) =<<)) $
    multiExec prog $
    getInputs inputs
  return 0

readArgs :: [String] -> String
readArgs [filename] = filename
readArgs _ = error ("Wrong number of arguments!\nUsage:\n\tprf function.prf")

main = do
  args <- getArgs
  progtxt <- readFile (readArgs args)
  let
    prog = parse progtxt
    ar   = arity prog
    errs = check prog
   in
    if length errs > 0
      then showErrs errs
      else mainLoop prog ar
