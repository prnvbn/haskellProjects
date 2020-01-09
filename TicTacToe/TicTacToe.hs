module TicTacToe where

import Data.Ix
import Data.Char
import Data.Maybe
import Data.List
import Text.Read

-------------------------------------------------------------------
data Player = O | X
            deriving (Eq, Show)

data Cell = Empty | Taken Player
          deriving (Eq, Show)

type Board = ([Cell], Int)

type Position = (Int, Int)

-------------------------------------------------------------------

--
-- Some useful functions from, or based on, the unassessed problem sheets...
--

-- Preserves Just x iff x satisfies the given predicate. In all other cases
-- (including Nothing) it returns Nothing.
filterMaybe :: (a -> Bool) -> Maybe a -> Maybe a
filterMaybe p m@(Just x)
  | p x = m
filterMaybe p _
  = Nothing

-- Replace nth element of a list with a given item.
replace :: Int -> a -> [a] -> [a]
replace 0 p (c : cs)
  = p : cs
replace _ p []
  = []
replace n p (c : cs)
  = c : replace (n - 1) p cs

-- Returns the rows of a given board.
rows :: Board -> [[Cell]]
rows (cs , n)
  = rows' cs
  where
    rows' []
      = []
    rows' cs
      = r : rows' rs
      where
        (r, rs) = splitAt n cs

-- Returns the columns of a given board.
cols :: Board -> [[Cell]]
cols
  = transpose . rows

-- Returns the diagonals of a given board.
diags :: Board -> [[Cell]]
diags (cs, n)
  = map (map (cs !!)) [[k * (n + 1) | k <- [0 .. n - 1]],
                      [k * (n - 1) | k <- [1 .. n]]]

-------------------------------------------------------------------

gameOver :: Board -> Bool
gameOver board
  = or (map isWin [bRows, bCols, bDiags])
  where
  bRows  = rows board
  bCols  = cols board
  bDiags = diags board
  isWin []
    = False
  isWin cells@(c:cs)
    = isSingle(nub c) || isWin cs
    -- Checks if a given list is an occupied singleton or not
  isSingle (Taken _:[])
    = True
  isSingle _
    = False

-------------------------------------------------------------------

--
-- Moves must be of the form "row col" where row and col are integers
-- separated by whitespace. Bounds checking happens in tryMove, not here.
--
parsePosition :: String -> Maybe Position
parsePosition ""
  = Nothing
parsePosition string
  | notIntList /= "" = Nothing
  | ws == []         = Just (fst', snd')
  | otherwise        = Nothing
  where
    splitString@(w1:(w2:ws)) = words string
    fst'        = fromJust (readMaybe w1 :: Maybe Int)
    snd'        = fromJust (readMaybe w2 :: Maybe Int)
    notIntList  = filter (\x -> isNumber x == False && x/=' ' && x/='-') string

tryMove :: Player -> Position -> Board -> Maybe Board
tryMove mark (i,j) board@(cells,size)
  | (inRange (0,size) i && inRange (0,size) j) == False = Nothing
  | isOccupied(cells!!pos)                              = Nothing
  | otherwise                                           = Just newBoard
  where
    newBoard = (replace pos (Taken mark) cells, size)
    pos      = (i*size + j)
    isOccupied (Taken _)
      = True
    isOccupied _
      = False
-------------------------------------------------------------------
-- I/O Functions

prettyPrint :: Board -> IO ()
prettyPrint board@(cells,size)
  = mapM_ putStrLn bRows
  where
    (c:cs) = cells
    bRows  = makeCharList (rows board)
    makeCharList []
      = []
    makeCharList (l : ls)
      = intersperse '|' (charList l) : makeCharList ls
    -- Converts a row into a printable character list
    charList  []
      = []
    charList  bRows@((Taken c) :cs)
      | c == X = 'X' : charList cs
      | c == O = 'O' : charList cs
    charList  bRows@(Empty :cs)
      = '-' : charList cs
-- Repeats a function until it works
doParseAction :: (String -> Maybe a) -> IO a
doParseAction function
  = do
        input  <- getLine
        maybe (putStrLn errMesage >> doParseAction function) return (function input)
    where
      errMesage = "Invalid Input! Try Again!"

-- The following reflect the suggested structure, but you can manage the game
-- in any way you see fit.

-- Repeatedly read a target board position and invoke tryMove until
-- the move is successful (Just ...).
takeTurn :: Board -> Player -> IO Board
takeTurn board player
  = do
      putStr ("Player " ++ show player ++  " make your move :")
      pos <- doParseAction parsePosition
      maybe (putStrLn errMesage >> takeTurn board player) return (tryMove player pos board)
    where
      errMesage = "!!!Oops your move failed!!!"
-- Manage a game by repeatedly: 1. prin" :"" :"ting the current board, 2. using
-- takeTurn to return a modified board, 3. checking if the game is over,
-- printing the board and a suitable congratulatory message to the winner
-- if so.
playGame :: Board -> Player -> IO ()
playGame board player
  = do
     if (gameOver board)
        then
          putStrLn (" Player " ++ show (changePlayer player) ++ " won!!")
      else
        do
          newBoard <- (takeTurn board player)
          prettyPrint newBoard
          playGame newBoard (changePlayer player)
          where
            changePlayer X = O
            changePlayer O = X

-- Print a welcome message, read the board dimension, invoke playGame and
-- exit with a suitable message.
main :: IO ()
main
  = do
    putStrLn "Welcome! Let's play haskell TicTacToe !!"
    putStrLn "Enter the dimensions of the board : "
    dimension <- doParseAction readMaybe :: IO Int
    playGame (take (dimension^2) (repeat Empty),dimension) X


-------------------------------------------------------------------

testBoard1, testBoard2, testBoard3 :: Board

testBoard1
  = ([Taken O,Taken X,Empty,Taken O,
      Taken O,Empty,Taken X,Taken X,
      Taken O,Empty,Empty,Taken X,
      Taken O,Taken X,Empty,Empty],
      4)

testBoard2
  = ([Taken X,Empty,
      Empty,Empty],
      2)

testBoard3
  = ([Taken O,Taken X,Empty,Taken O,Taken X,
      Taken O,Empty,Taken X,Taken X,Empty,
      Empty,Empty,Taken X,Taken O,Taken O,
      Taken O,Taken X,Empty,Empty,Taken X,
      Taken X,Empty,Taken O,Empty,Empty],
      5)
