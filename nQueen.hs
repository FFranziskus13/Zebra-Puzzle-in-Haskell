import Backtracking

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Verifier
import Text.Printf


n :: Integer
n = 8

s :: [Integer]
s = [1..n]

queenVariables :: [Variable]
queenVariables = [printf "V%d" i | i <- s]

queenValues :: [Value]
queenValues = s

differentCols :: [Constraint]
differentCols  = [Constraint {
        fstVar = printf "V%d" i,
        sndVar = printf "V%d" j, 
        constrFunc = \(x, y) -> x /= y
    }
    | i <- s, j <- s, i < j]

differentDiags :: [Constraint]
differentDiags = [Constraint {
        fstVar = printf "V%d" i,
        sndVar = printf "V%d" j, 
        constrFunc = \(x, y) -> abs(x - y) /= abs(dij)
    } 
    | i <- s, j <- s, i < j, let dij = j - i]

queenConstraint :: [Constraint]
queenConstraint = differentCols ++ differentDiags

assignment :: Assignment
assignment = Map.fromList []

type Position = (String, Integer)

drawBoard :: [Position] -> String
drawBoard positions =
  let posMap = Map.fromList positions
      board = [ [ if Map.lookup ("V" ++ show col) posMap == Just row then 'O' else '.'
                | col <- [1..n] ]
              | row <- reverse [1..n] ]
  in unlines (map unwordsChars board)

unwordsChars :: [Char] -> String
unwordsChars = unwords . map (:[])

nQueen :: CSP
nQueen = CSP queenVariables queenValues queenConstraint
finalRes = solveCSP nQueen

main :: IO()
main = do
    putStrLn $ (drawBoard (Map.toList finalRes))