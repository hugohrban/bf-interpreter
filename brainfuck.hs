import Control.Monad.Except
import Data.Functor ((<&>))
import Data.Word (Word8)
import System.Environment (getArgs)
import Prelude hiding (Left, Right)

-- This program is an interpreter for the Brainfuck programming language implemented in Haskell.
-- Language reference: http://brainfuck.org/brainfuck.html

-- the Brainfuck program is a list of instructions
type Program = [Instruction]

-- Brainfuck instructions:
--      '+' -> Increment value at pointer
--      '-' -> Decrement value at pointer
--      '<' -> Move pointer left
--      '>' -> Move pointer right
--      '.' -> Print character at pointer to stdout
--      ',' -> Read one character from stdin and store it at the pointer
--      '[' -> Start of loop - only enter the loop if value at pointer is non-zero. The loop contains a list of instructions
--      ']' -> End of loop - if value at pointer is non-zero repeat the loop
data Instruction
  = Inc
  | Dec
  | Left
  | Right
  | Print
  | Read
  | Loop [Instruction]
  deriving (Show, Eq)

-- tape is the BF memory (size is 30000, starting at index 0),
-- left side is reversed to make it easier (and faster) to work with
-- if user moves pointer outside the tape, error is thrown
-- the elements are of type Word8 which is a 8-bit unsigned integer
data Tape = Tape [Word8] Word8 [Word8] deriving (Show)

-------------------- Instructions --------------------

inc :: Tape -> Tape
inc (Tape l p r) = Tape l (p + 1) r

dec :: Tape -> Tape
dec (Tape ls p rs) = Tape ls (p - 1) rs

left :: Tape -> Tape
left (Tape (l : ls) p rs) = Tape ls l (p : rs)
left (Tape [] _ _) = error "left end of tape"

right :: Tape -> Tape
right (Tape ls p (r : rs)) = Tape (p : ls) r rs
right (Tape _ _ []) = error "right end of tape"

get :: Tape -> Word8
get (Tape _ p _) = p

printChar :: Tape -> IO Tape
printChar t = do
  putChar $ toEnum $ fromEnum $ get t
  return t

set :: Tape -> Word8 -> Tape
set (Tape ls _ rs) p = Tape ls p rs

readChar :: Tape -> IO Tape
readChar t = do
  ch <- getChar
  let i = toEnum $ fromEnum ch
  return $ set t i

executeLoop :: Instruction -> Tape -> IO Tape
executeLoop (Loop is) t = do
  if get t == 0 -- if the pointer is 0, don't execute the loop
    then return t
    else do
      t' <- execute is (return t) -- execute the loop body
      executeLoop (Loop is) t' -- execute the whole loop again recursively

-------------------- Debug --------------------

--- print a portion of tape around the pointer for debugging purposes
debugPrint :: IO Tape -> Int -> IO ()
debugPrint t i = do
  (Tape l p r) <- t
  putStrLn $ show (take i $ reverse l) ++ " " ++ show p ++ " " ++ show (take i r)

-------------------- Parsing --------------------

-- check if the brackets are matched correctly
checkMatchingBrackets :: String -> Bool
checkMatchingBrackets code = check 0 $ filter (`elem` "[]") code
  where
    check n [] = n == 0 -- n is the number uf currently open brackets
    check n (x : xs)
      | n < 0 = False
      | x == '[' = check (n + 1) xs
      | x == ']' = check (n - 1) xs
      | otherwise = check n xs -- this shouldn't happen since we filter

-- parse the first instruction from source code, (if its a '[' parse the inside recursively using parseSourceCode).
-- returns ([parsedInstruction], restOfCode). note: we need to return rest of code because the loop can have arbitrary length
parseInstruction :: String -> ([Instruction], String)
parseInstruction [] = ([], [])
parseInstruction (i : is)
  | i == '.' = ([Print], is)
  | i == ',' = ([Read], is)
  | i == '+' = ([Inc], is)
  | i == '-' = ([Dec], is)
  | i == '<' = ([Left], is)
  | i == '>' = ([Right], is)
  | i == '[' = ([Loop (parseSourceCode l)], r)
  where
    (l, r) = getBracketedCode (i : is)

-- get all chars between this and the matching bracket and the rest, assuming first char is bracket, output will be (codeInBrackets, restOfCode)
getBracketedCode :: String -> (String, String)
getBracketedCode [] = ([], [])
getBracketedCode (x : xs) = check 1 [x] xs
  where
    check 0 left right = (init $ tail $ reverse left, right)
    check n ls (r : rs)
      | r == '[' = check (n + 1) (r : ls) rs
      | r == ']' = check (n - 1) (r : ls) rs
      | otherwise = check n (r : ls) rs

-- parse the source code string into a BF program. throws error if brackets are unmatched
parseSourceCode :: String -> Program
parseSourceCode code
  | not $ checkMatchingBrackets code = error "unmatched brackets"
  | otherwise = parse (filter (`elem` "+-<>[],.") code) []
  where
    parse :: String -> Program -> Program
    parse [] prog = prog
    parse xs prog = prog ++ parsed ++ parseSourceCode rest
      where
        (parsed, rest) = parseInstruction xs

-------------------- Execution --------------------

-- make a tape of size n
makeTape :: Int -> Tape
makeTape n = Tape [] 0 (replicate (n - 1) 0)

-- normal tape size is 30000 as per the BF spec
normTape :: Tape
normTape = makeTape 30000

-- execute a single instruction on a tape and return the new tape
executeInstruction :: IO Tape -> Instruction -> IO Tape
executeInstruction t ins
  | ins == Inc = t <&> inc
  | ins == Dec = t <&> dec
  | ins == Left = t <&> left
  | ins == Right = t <&> right
  | ins == Print = t >>= printChar
  | ins == Read = t >>= readChar
  | otherwise = t >>= executeLoop ins

-- execute a program on a tape and return the final tape
execute :: Program -> IO Tape -> IO Tape
execute is t = foldl executeInstruction t is

-- run a BF program from a string of source code
run :: String -> IO ()
run code = do
  execute (parseSourceCode code) (return normTape)
  return ()

-- run a BF program from a file
runFile :: String -> IO ()
runFile filename = do
  code <- readFile filename
  execute (parseSourceCode code) (return normTape)
  return ()

main :: IO ()
main = do
  args <- getArgs
  code <- readFile $ head args
  let tape = if length args > 1 then return $ makeTape $ read $ args !! 1 else return normTape
  execute (parseSourceCode code) tape
  return ()
