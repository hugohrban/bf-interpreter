import Control.Monad.Except
import Prelude hiding (Left, Right)
import Data.Functor
import Data.Int (Int8, Int16)
import System.Environment


-- tape is the BF memory (size is 30000, starting at index 0), 
-- left side is reversed to make it easier (and faster) to work with
data Tape = Tape [Int8] Int8 [Int8] deriving (Show)

data Instruction =
    Inc |
    Dec |
    Left |
    Right |
    Print |
    Read |
    Loop [Instruction]
    deriving (Show)

-- the BF program is a list of instructions
type Program = [Instruction]

inc :: Tape -> Tape
inc (Tape l p r) = Tape l (p+1) r

dec :: Tape -> Tape
dec (Tape ls p rs) = Tape ls (p-1) rs

left :: Tape -> Tape
left (Tape (l:ls) p rs) = Tape ls l (p:rs)
left (Tape [] _ _) = error "left end of tape"

right :: Tape -> Tape
right (Tape ls p (r:rs)) = Tape (p:ls) r rs
right (Tape _ _ []) = error "right end of tape"

get :: Tape -> Int8
get (Tape _ p _) = p

printChar :: Tape -> IO Tape
printChar t = do
    putChar $ toEnum $ fromEnum $ get t
    return t

readChar :: Tape -> IO Tape
readChar t = do
    ch <- getChar
    -- if end of file
    --if ch == '\n' then return t else do
    let i = toEnum $ fromEnum ch
    return $ set t i

set :: Tape -> Int8 -> Tape
set (Tape ls _ rs) p = Tape ls p rs


--- print a portion of tape around the pointer
debugPrint :: IO Tape -> Int -> IO ()
debugPrint t i = do
    (Tape l p r) <- t
    putStrLn $ show (take i $ reverse l) ++ " " ++ show p ++ " " ++ show (take i r)


-- get only the valid BF characters . , < > + - [ ]
removeUnwantedChars :: String -> String
removeUnwantedChars = filter (`elem` ".,+-<>[]")


-- check if the brackets are matched
checkMatchingBrackets :: String -> Bool
checkMatchingBrackets = check 0
    where
        check n [] = n == 0
        check n (x:xs)
            | n < 0 = False
            | x == '[' = check (n+1) xs
            | x == ']' = check (n-1) xs
            | otherwise = check n xs


-- parse the first instruction from source code, (if its a '[' it will be a longer list), and also return the restof source code
parseInstruction :: String -> ([Instruction], String)
parseInstruction [] = ([],[])
parseInstruction (i:is)
    | i == '.' = ([Print], is)
    | i == ',' = ([Read], is)
    | i == '+' = ([Inc], is)
    | i == '-' = ([Dec], is)
    | i == '<' = ([Left], is)
    | i == '>' = ([Right], is)
    | i == '[' = ([Loop (parseSourceCode l)], r)
        where (l,r) = getBracketedCode (i:is)


-- get all chars between this and the matching bracket and the rest, assuming first char is bracket, output will be ([.*], .*)
getBracketedCode :: String -> (String, String)
getBracketedCode [] = ([],[])
getBracketedCode (x:xs) = check 1 [x] xs
    where
        check 0 left right = (init $ tail $ reverse left, right)
        check n ls (r:rs)
            | r == '[' = check (n+1) (r:ls) rs
            | r == ']' = check (n-1) (r:ls) rs
            | otherwise = check n (r:ls) rs

-- assuming source code is verified and cleaned up, parse it into a list of instructions
parseSourceCode :: String -> Program
parseSourceCode code = parse code []
    where
        parse [] prog = prog
        parse xs prog = prog ++ parsed ++ parseSourceCode rest
            where (parsed, rest) = parseInstruction xs

getProgram :: String -> Program
getProgram code
    | not $ checkMatchingBrackets code = error "unmatched brackets"
    | otherwise = parseSourceCode $ removeUnwantedChars code

makeTape :: Int -> Tape
makeTape n = Tape [] 0 (replicate (n-1) 0)

-- normal tape size
normtape :: Tape
normtape = makeTape 30000

executeLoop :: Instruction -> Tape -> IO Tape
executeLoop (Loop is) t = do
    if get t == 0                   -- if the pointer is 0, don't execute the loop
        then return t
    else do                    
        t' <- execute is (return t)     -- execute the loop body
        executeLoop (Loop is) t'        -- execute the loop again recursively

    -- t' <- execute is (return t)
    -- if get t' == 0 then return t' else executeLoop (Loop is) t'


-- TODO understand this, fix Read
executeInstruction :: Instruction -> IO Tape -> IO Tape
executeInstruction Inc t = t >>= return . inc
executeInstruction Dec t = t >>= return . dec
executeInstruction Left t = t >>= return . left
executeInstruction Right t = t >>= return . right
executeInstruction Print t = t >>= printChar
executeInstruction Read t = t >>= readChar
executeInstruction (Loop is) t = t >>= executeLoop (Loop is)

-- \t' -> if get t' == 0 then return t' else execute is (return t') >>= \t'' -> executeInstruction (Loop is) (return t'')

-- TODO
execute :: Program -> IO Tape -> IO Tape
-- execute [] t = t
-- execute (i:is) t = execute is (executeInstruction i t)
execute is t = foldl (flip executeInstruction) t is

run :: String -> IO ()
run code = do
    execute (getProgram code) (return normtape)
    return ()

runFile :: String -> IO ()
runFile filename = do
    code <- readFile filename
    execute (getProgram code) (return normtape)
    return ()

main :: IO ()
main = do
    args <- getArgs
    code <- readFile $ head args
    execute (getProgram code) (return normtape)
    return ()