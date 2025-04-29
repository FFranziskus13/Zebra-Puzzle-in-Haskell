import Backtracking
import Verifier
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.List as List

-- Attribute categories
colors = ["Red", "Green", "Ivory", "Yellow", "Blue"]
nationalities = ["Englishman", "Spaniard", "Ukrainian", "Norwegian", "Japanese"]
pets = ["Dog", "Snails", "Fox", "Horse", "Zebra"]
drinks = ["Coffee", "Tea", "Milk", "OrangeJuice", "Water"]
smokes = ["OldGold", "Kools", "Chesterfields", "LuckyStrike", "Parliaments"]

categories = [ ("Color", colors), ("Nationality", nationalities), ("Drink", drinks), ("Smoke", smokes), ("Pet", pets)]

zebraVariables = colors ++ nationalities ++ pets ++ drinks ++ smokes
zebraValues = [1..5]

eq a b = Constraint a b (\(x, y) -> x == y)
nextTo a b = Constraint a b (\(x, y) -> abs (x - y) == 1)

allDifferent vars = [Constraint i j (\(x, y) -> x /= y) | i <- vars, j <- vars, i < j ]

zebraConstraints = [
    eq "Englishman" "Red",
    eq "Spaniard" "Dog",
    eq "Coffee" "Green",
    eq "Ukrainian" "Tea",
    Constraint "Green" "Ivory" (\(x, y) -> x == y + 1),
    eq "OldGold" "Snails",
    eq "Kools" "Yellow",
    Constraint "Milk" "unused" (\(x, _) -> x == 3),
    Constraint "Norwegian" "unused" (\(x, _) -> x == 1),
    nextTo "Chesterfields" "Fox",
    nextTo "Kools" "Horse",
    eq "LuckyStrike" "OrangeJuice",
    eq "Japanese" "Parliaments",
    nextTo "Norwegian" "Blue"
    ] 
    ++ allDifferent colors
    ++ allDifferent nationalities
    ++ allDifferent pets
    ++ allDifferent drinks
    ++ allDifferent smokes

zebraCSP = CSP zebraVariables zebraValues zebraConstraints 

solution = solveCSP zebraCSP

-- Pretty-print the Zebra Puzzle result as a table
-- For each house, find the attribute name from the category
invert xs = [(v, k) | (k, v) <- xs]
findByHouse catAttrs h = Maybe.fromMaybe "???" $ lookup h (invert catAttrs)

printZebraTable:: Assignment -> IO ()
printZebraTable assignment = do
    let houses = [1..5]
        -- Turn a category and its attribute names into a row of values
        buildRow (label, attrs) =
            let catValues = [ (attr, assignment Map.! attr) | attr <- attrs, attr `Map.member` assignment ]
            in label : [ findByHouse catValues h | h <- houses ]

        table = map buildRow categories
        columnWidths = [ maximum (map length col) | col <- transpose table ]
        padCell i s = s ++ replicate (columnWidths !! i - length s) ' '
        formatRow row = "| " ++ intercalate " | " (zipWith padCell [0..] row) ++ " |"

    -- Header
    putStrLn $ formatRow ("Category" : map (("House " ++) . show) houses)
    putStrLn $ replicate (sum columnWidths + length columnWidths * 3 + 1) '-'

    -- Rows
    mapM_ (putStrLn . formatRow) table

main :: IO ()
main = do
    printZebraTable solution
    putStrLn $ "The person in the House " ++ show (Maybe.fromJust (Map.lookup "Water" solution)) ++ " drinks water."
    putStrLn $ "The person in the House " ++ show (Maybe.fromJust (Map.lookup "Zebra" solution)) ++ " owns the Zebra."
    