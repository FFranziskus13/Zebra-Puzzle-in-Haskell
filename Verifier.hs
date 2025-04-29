-- Verifier.hs
-- Verifies variable assignments for a Constraint Satisfaction Problem (CSP)

module Verifier where

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Map (Map)

-- Type synonyms
type Variable = String
type Value = Integer
type Assignment = Map Variable Value

-- A constraint relates two variables using a function
data Constraint = Constraint
    { fstVar :: Variable
    , sndVar :: Variable
    , constrFunc :: (Value, Value) -> Bool
    }

-- | Insert (var, val) into the assignment and check if all constraints are satisfied.
isConsistent :: Variable -> Value -> Assignment -> [Constraint] -> Bool
isConsistent var val assignment constraints =
    let
        extendedAssignment = Map.insert var val assignment
    in
        all (evalConstraint extendedAssignment) constraints

-- | Evaluate one constraint under a given assignment
evalConstraint :: Assignment -> Constraint -> Bool
evalConstraint assign (Constraint var1 var2 func) =
    let
        val1 = Map.lookup var1 assign
        val2 = Map.lookup var2 assign
    in
        if Maybe.isNothing val1 || (Maybe.isNothing val2 && var2 /= "unused") -- check if both are given or the second variable is unused
            then True -- unable to check but not yet violated
            else func (Maybe.fromJust val1, Maybe.fromJust val2) -- check the contraint function
            -- casting back from Just is save here because isNothing has been checked

-- Show instance for Constraint (optional)
instance Show Constraint where
    show (Constraint v1 v2 _) = v1 ++ " <-> " ++ v2
