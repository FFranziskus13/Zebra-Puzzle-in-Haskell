
module Backtracking where

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Text.Printf
import Verifier

data CSP = CSP
    {
        variables :: [Variable],
        values :: [Value],
        constraints :: [Constraint]
    }

solveCSP :: CSP -> Assignment
solveCSP (CSP vars vals constraints) = let solution = backtrack_search (Map.fromList []) vars vals constraints (length vals - 1) in
    if Maybe.isNothing solution
        then Map.fromList []
        else Maybe.fromJust solution

backtrack_search :: Assignment -> [Variable] -> [Value] -> [Constraint] -> Int -> Maybe.Maybe Assignment
backtrack_search assign vars vals constraints (-1) = Nothing -- all values have been tested for one variable (unsolvable point)
backtrack_search assign vars vals constraints nVal =
    if length vars == Map.size assign
        then Just assign -- Done !
        else
            -- Because vars is ordered therefore var will
            -- always be  the first unassigned variable 
            let var = (getUnassignedVars assign vars) !! 0 in 
            let val = (vals !! nVal) in
            if isConsistent var val assign constraints -- Check if val is a possible value for the variable var
            then let possibleTree = backtrack_search (updateAssignment assign var val) vars vals constraints ((length vals) - 1) in
                if Maybe.isNothing possibleTree -- Check if the recursion tree let to a unsolvable point
                    then backtrack_search assign vars vals constraints (nVal - 1) -- Check next value
                    else possibleTree -- return correct unassigned
            else backtrack_search assign vars vals constraints (nVal - 1) -- Check next value


getUnassignedVars :: Assignment -> [Variable] -> [Variable]
getUnassignedVars assign vars = [v | v <- vars, v `Map.notMember` assign]

updateAssignment :: Assignment -> Variable -> Value -> Assignment
updateAssignment assign var val = Map.fromList ((var, val): (Map.toList assign))
