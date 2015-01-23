import System.Environment
import System.Random
import Data.List  

{-
 - Lets implement the UNIX echo command
 - The program arguments are simply printed to the standard output.
 - If the first argument is -n, this argument is not printed, and no trailing newline is printed
 -}
  
main = do  
   args <- getArgs  
   mapM putStr $ insertSpaces . newline $ args

newline :: [String] -> [String]
newline args = case args of
	"-n":xs -> xs
	_       -> args ++ ["\n"]

insertSpaces :: [String] -> [String]
insertSpaces x = reverse . (drop 1) . reverse . concat . map (\y -> [y, " "]) $ x


{- Write a lottery number picker
 - This function should take a StdGen instance, and produce a list of six unique numbers between 1 and 49, in numerical order
 -}
lottery :: StdGen -> [Int]
lottery gen = sort . take 6 $ randomRs (1,49) gen
