-- project: formula-2-bdd
-- author: Martin Risa
-- login: xrisam00
-- NOTE: Many of code come from StackOverflow and github

import System.IO
import System.Environment
import Data.List
import Control.Monad as Monad
import Control.Exception
import System.IO.Error  

type Formula = [[String]]
type TruthTable = [[Bool]]
--BDD is standard binary tree with bool value at the leafs
data BDD a = Leaf Bool 
    | Node a (BDD a) (BDD a)
    deriving (Show, Eq)

-- START DEBUG FUNCTIONS ------------------------------------------
stringHeads :: [String] -> String
stringHeads [] = []
stringHeads (x:xs) = (head x) : stringHeads xs

--METHODS REGARDED -i option

printFLineH :: [String] -> String
printFLineH []      = ")"
printFLineH (x:xs)  = "," ++ x ++ printFLineH xs

printFLine :: [String] -> String
printFLine []       = ""
printFLine (x:xs)   = "(" ++ x ++ (printFLineH xs) ++ "\n"

printFormula :: Formula -> String
printFormula []     = ""
printFormula (x)    = concat( map printFLine x)

--MUTUAL METHODS---------------------------------------------------------------------------------------
getUniqueVariables :: Formula -> [String]
--concats every line of formula and drop negations if present,remove duplicates(nub) from it
getUniqueVariables formula =
    let ss  = concat formula     -- Formula -> [String] concats every line of formula into single list
        sss = map (dropWhile (== '-')) (ss)  -- [String] -> [String] drop negations if present
    in nub sss -- remove duplicates
    
--creates empty BDD from list of variables with leaves equal empty
createEmptyBDD :: [String] -> BDD String
createEmptyBDD (x:[]) = Node x (Leaf False) (Leaf True)
createEmptyBDD (x:xs) = Node x (createEmptyBDD xs) (createEmptyBDD xs)

--METHODS REGARDED -t option   ------------------------------------------------------------------------ 
--chcecks whether (1)e.g. [a,b,-c] with valuation (2)e.g [T,T,F] corresponds to clause from the input(3)
--first we check for all items in (3) whether it contains Bool value and we add negation when needed then
-- then we check  for all items in modified (3) whether it contains the same elements as (1).
-- if so return true, returns false otherwise 
satisfied :: [String] -> [Bool] -> [String] -> Bool
satisfied vars vals clause = all (`elem` values ) clause
    where values = map (\ x -> if fst x == False then "-" ++ snd x else snd x) (zip vals vars)

--returns String representing table
printTable :: TruthTable -> String
--shows fromEnum(True -> 1, False ->0) every element of truth table wich is joined by delimiter space,
--every line of truth table is then joined by delimiter new line 
printTable table = unlines [ unwords [ show (fromEnum x) | x <- xs] | xs <- table]

--creates truth table according to formula
getTruthTable :: Formula -> TruthTable
getTruthTable formula =
    -- replicate (number of vars)-times list [T,F] creating [[F,T],[F,T]...,[F,T]]
    let vars        = getUniqueVariables formula
        truthValues = sequence (replicate (length vars) [True,False])
    -- for every item of created list check whether and value correspond given formula
    in [values ++ [ any (satisfied vars values) formula] | values <- truthValues]

--creates complete truth table String
printTruthTable::Formula -> String
printTruthTable formula = do
    ret <- unwords (getUniqueVariables formula) ++ "\n" ++ (printTable (getTruthTable formula))
    return ret


--METHODS REGARDED -b option------------------------------------------------------------------------------
setLeaf :: BDD a -> [Bool] -> Bool -> BDD a
setLeaf (Node x l r) (True:xs)  newVal  = Node x l (setLeaf r xs newVal)
setLeaf (Node x l r) (False:xs) newVal  = Node x (setLeaf l xs newVal) r
setLeaf (Leaf b)     []         newVal  = Leaf newVal

getLeafValue :: BDD a -> [Bool] -> Bool
getLeafValue (Node x l r) (True:xs)  = getLeafValue r xs
getLeafValue (Node x l r) (False:xs) = getLeafValue l xs
getLeafValue (Leaf b)     []         = b

--set BDD leaves value according to TruthTable
setBDDLeaves :: BDD a -> TruthTable -> BDD a
setBDDLeaves bdd tt = foldl (\x vals -> setLeaf x (init vals) (last vals)) bdd tt

getBDD :: Formula -> BDD String
getBDD formula =
    let vars = getUniqueVariables formula
        emptyBDD = createEmptyBDD vars
        tt = getTruthTable formula
        retBDD = setBDDLeaves emptyBDD tt
    in retBDD

--"super effective function for creating paths in bdd in proper order"
getPaths :: BDD String -> String -> [String] -> [String] 
getPaths (Leaf x) s xs =
    let ret = (s++(if x==False then "0" else "1")):xs 
    in ret
getPaths (Node x left right) s xs = 
    let ret = (getPaths right (s ++ x ++ "=>") xs ) ++ (getPaths left (s ++ x ++ "->") xs )
    in ret

printBDD :: Formula -> [String]
printBDD formula =
    let bdd = getBDD formula    -- bdd is tree of strings
        paths = getPaths bdd "" [] -- paths is e.g. ["a=b->c->",...]
    in paths

--METHODS REGARDED -r option
-- | Reduce Ordered BDD to ROBDD(we can reduce if subtrees are equivalent)
reduceBDD :: (Eq a) => BDD a -> BDD a
reduceBDD (Node x l r)
    | l == r = reduceBDD l
    | l /= r = (Node x (reduceBDD l) (reduceBDD r))
reduceBDD (Leaf b) = (Leaf b)

printRBDD :: Formula -> [String]
printRBDD formula =
    let vars = getUniqueVariables formula -- vars is e.g. ["a","b","c"]
        bdd = getBDD formula    -- bdd is tree of strings
        rbdd = reduceBDD bdd
        paths = getPaths rbdd "" [] -- paths is e.g. ["a=b->c->",...]
    in paths

split :: String -> [String]
split [] = [""]
split (c:cs) | c == ','  = "" : rest
             | otherwise = (c : head rest) : tail rest
    where rest = split cs

getInputFromFile :: FilePath -> IO ([String], Handle)
getInputFromFile f = do
    handle <- openFile f ReadMode
    cont <- hGetContents handle    
    let ret = lines cont
    return (ret,handle)    
    
getInputFromStdIn :: IO [String]
getInputFromStdIn = do
    input <- getContents
    let ret = lines input
    return ret
    
help :: String
help = " ./formula-2-bdd [-i | -t | -r | -b] input\n \
\ \n \
\ -i read input to internal representation and print DNF representation\n \
\ -t print truth table\n \
\ -b print BDD\n \
\ -r print RBDD\n \
\ \n \
\ apply function:\n \
\ ./formula-2-bdd SWITCH input_file\n"

--Command Input Result
processCommand :: String -> [String] -> String
processCommand _ [] = ""
processCommand c ss = do
    let sss = map tail ss               -- [String] -> [String] removing first element
        ssss = map init sss             -- [String] -> [String] removing last element
        formula = map split ssss        -- [String] -> [[String]] by spliting on delimiter ','
    ret <- case c of
                "-i"    -> (printFormula formula)
                "-t"    -> (printTruthTable formula)
                "-b"    -> (unlines (printBDD formula))
                "-r"    -> (unlines (printRBDD formula))
                _       -> error "Unknown operation: " ++ c ++ "\n" ++ help
    return ret

main = do
    args <- getArgs
    case args of
        [command, file] -> do
            (inputs,handle) <- getInputFromFile file            
            let result = processCommand command inputs
            putStr result
            hClose handle
        [command] -> do
            inputs <- getInputFromStdIn
            let result = processCommand command inputs        
            putStr result
        _ -> putStr help
