module Game2048 where

import Test.QuickCheck
import Data.Char
import Data.List
import Data.Maybe
import System.Random
import Control.Monad

-- | 4x4 board for 2048 puzzle
newtype Board = Board { rows :: [[Int]]}
 deriving (Show,Eq)

-- | example 2048 puzzle board
example :: Board
example = Board [[2 ,16,32,64]
                ,[2 ,0 ,8 ,0 ]
                ,[4 ,4 ,0 ,0 ]
                ,[0 ,0 ,8 ,0 ]
                ]

instance Arbitrary Board where
  arbitrary = do
    rows <- vectorOf 4 (vectorOf 4 cell)
    return (Board rows)

cell :: Gen (Int)
cell = frequency [(4,return 0),   (4,return 2),
                  (4,return 4),   (1,return 8),
                  (1,return 16),  (1,return 32),
                  (1,return 64),  (1,return 128),
                  (1,return 256), (1,return 512),
                  (1,return 1024),(1,return 2048)
                  ]

-- | prop for isBoard
prop_Board :: Board -> Bool
prop_Board = isBoard

-- | check that board has 4 x 4 dimension
isBoard :: Board -> Bool
isBoard (Board rows) = isSize4 rows && isAllRow4 rows
  where isAllRow4 = all isSize4
        isSize4 x = size x == 4

size :: [a] -> Int
size = foldr (\ x -> (+) 1) 0

-- | list of 2 power n
twopow :: [Int]
twopow = [0] ++ map (\n-> 2^n) [1..]

--  | alternative generator of cell
cell2 :: Gen (Int)
cell2 = elements $ take 12 twopow

-- | get cell at position (x,y) of the board
getCell :: Board -> Pos -> Int
getCell board (x,y) = (rows board)!!x!!y

-- | Board with all 0
allBlankBoard :: Board
allBlankBoard = Board (replicate 4 $ replicate 4 0)

-- | board at the start of the game (allBlankBoard with 2 random cells)
startBoard :: RandomGen g => g -> (Board, g)
startBoard g = rUpdate b g'
  where (b, g') = rUpdate allBlankBoard g

-- | update board at position (i,j) with a given value
update :: Board -> Pos -> Int -> Board
update (Board b) (i,j) x = Board $ update' b i updatedRow
  where updatedRow = update' (b!!i) j x
        update' (x:xs) i (y) | i==0 = y:xs
                             | i>0  = x: (update' xs (i-1) y)

-- | check that update is correct
-- |  1) position (x,y) must be updated to x
-- |  2) other positions must be the same
prop_Update :: Board -> (Int, Int) -> Int -> Bool
prop_Update b1 pos@(i,j) x | pos `elem` poss = (getCell b2 pos == x) && b1'==b2'
                           | otherwise = True
  where b2 = update b1 pos x           --updated board
        poss' = delete pos poss        --all positions without the updated one
        b1' = map (getCell b1) poss'
        b2' = map (getCell b2) poss'

-- | postion of cell
type Pos = (Int, Int)

-- | all possible positions
poss :: [Pos]
poss = [(x,y)| x<-[0..3], y<-[0..3]]

-- | return the positions of zeroes of the board
zeroes :: Board -> [Pos]
zeroes (Board b) = catMaybes [ if 0==(b!!i)!!j
                                  then (Just (i,j))
                                  else Nothing
                               | i<-[0..3], j<-[0..3]]

-- | return the positions of non-zeroe numbers of the board
nonzeroes :: Board -> [Pos]
nonzeroes (Board b) = catMaybes [ if 0/=(b!!i)!!j
                                     then (Just (i,j))
                                     else Nothing
                                  | i<-[0..3], j<-[0..3]]

-- | check zeroes and nonzeroes function
-- |  1) union of zeroes and nonzeroes positions must equal to all possible positions
-- |  2) all return positions from zeroes function are all zero
-- |  3) all return positions from nonzeroes function are all non-zero numbers
prop_ZeroesAndNonzeroes :: Board -> Bool
prop_ZeroesAndNonzeroes board = (sort ((zeroes board) ++ (nonzeroes board)) == poss)
                                && prop_zero board
                                && prop_nonzero board

-- | help function of prop_ZeroesAndNonzeroes
prop_zero :: Board -> Bool
prop_zero board = all (==0) $ map (getCell board) poss
  where poss = zeroes board

-- | help function of prop_ZeroesAndNonzeroes
prop_nonzero :: Board -> Bool
prop_nonzero board = all (/=0) $ map (getCell board) poss
  where poss = nonzeroes board

-- | random generate a cell and update it to random position of the board
-- | called after player swiped
rUpdate :: RandomGen b => Board -> b -> (Board, b)
rUpdate board g = ((update board (fromJust pos) (num)),g2)
  where (pos,g1) = rPos board g
        (num,g2) = rNum g1

-- | random either 2 or 4
rNum :: (Num a, RandomGen b) => b -> (a, b)
rNum g = ([2,4]!!x,g')
  where (x,g') = randomR (0,1) g

-- | random a position of some zero-cell
rPos :: RandomGen b => Board -> b -> (Maybe Pos, b)
rPos board g | n/=0 = (Just ((zeroes board)!!x),g')
             | otherwise = (Nothing,g')
  where n = length $ zeroes board
        (x,g') = randomR (0,n-1) g

-- | Direction consists of Up, Down, Left, Right
data Direction = U | D | L | R
  deriving (Show,Eq)

-- | swipe to a direction to update board and the score
swipe :: RandomGen g => Direction -> Board -> Int -> g -> (Board, Int, g)
swipe dir b score g | b/=b' = (b'', score+score', g')
                     | otherwise = (b,score,g')
  where (b',score') = swipe' dir b
        (b'',g') = rUpdate b' g

-- | help function of swipe
swipe' :: Direction -> Board -> (Board, Int)
swipe' dir (Board b) | dir == L = (Board $ map fst $ map combineRow bL,
                                    sum $ map snd $ map combineRow bL)
                      | dir == R = (Board $ map reverse $ map fst $ map combineRow bR,
                                    sum $ map snd $ map combineRow bR)
                      | dir == U = (Board $ transpose $ map fst $ map combineRow bU,
                                    sum $ map snd $ map combineRow bU)
                      | dir == D = (Board $ transpose $ map reverse $
                                    map fst $ map combineRow bD,
                                    sum $ map snd $ map combineRow bD)
  where bL = zip b [0,0,0,0]
        bR = zip (map reverse b) [0,0,0,0]
        bU = zip (transpose b) [0,0,0,0]
        bD = zip (map reverse (transpose b)) [0,0,0,0]

-- | help function of swipe'
combineRow :: (Eq b, Num b) => ([b], b) -> ([b], b)
combineRow (xs, score) = (padRow xs',score')
  where (xs',score') = combineRow' ((filter (/=0) xs),score)
        padRow xs = take 4 (xs++[0,0,0,0])
        combineRow' (x:y:xs, score) | x==y = ([x+y] ++ (fst $ combineRow' (xs, (x+y)+score)),
                                              snd $ combineRow' (xs, (x+y)+score))
                                    | otherwise = (x:(fst $ combineRow' (y:xs,score)),
                                                   (snd $ combineRow' (y:xs,score)))
        combineRow' (x, score) = (x, score)

-- | check if the game is over (no moves are possible)
isOver :: Board -> Bool
isOver b | u==d && u==l && u==r = True
         | otherwise = False
  where u = fst $ swipe' U b
        d = fst $ swipe' D b
        l = fst $ swipe' L b
        r = fst $ swipe' R b

-- | convert board to printable string
printBoard :: Board -> String
printBoard (Board []  )    = "+----+----+----+----+\n"
printBoard (Board (cs:rs)) = "+----+----+----+----+\n"++pstr++(printBoard (Board rs))
  where pstr = "|"++(foldr (++) "" $ map addSpace cs)++"\n"

-- | add spaces to the number and convert to string (used in printBoard)
addSpace :: (Eq a, Num a, Show a) => a -> String
addSpace c = take numspace "    " ++ (show' c)++"|"
  where numspace = 4 - (length $ show c)
        show' c | c==0 = " "
                | otherwise = show c


----------------------------------------------------------------------
-- | main function, call this function to play the game
main :: IO()
main = do
  g <- newStdGen
  runGame g

runGame :: RandomGen g => g -> IO ()
runGame g = do
    putStrLn ":: Welcome to 2048 puzzle!"
    putStrLn ":: Type w, a, s, d to control movement direction"
    putStrLn ":: Type quit to exit"
    putStrLn "\n=== GAME START ===\n"
    gameLoop (b, 0, g')
  where (b,g') = startBoard g

gameLoop :: RandomGen g => (Board, Int, g) -> IO ()
gameLoop (board, score, g) = do
    if isOver board
      then putStrLn $ ":: your final score is " ++ (show score) ++"!\n=== GAME OVER === "
      else do putStrLn $ "Score: " ++ (show score)
              putStr   $ printBoard board
              putStr   "direction [w/a/s/d]: "
              dir <- getLine
              putStrLn ""
              case (take 1 dir) of
                "w" -> gameLoop $ swipe U board score g
                "a" -> gameLoop $ swipe L board score g
                "s" -> gameLoop $ swipe D board score g
                "d" -> gameLoop $ swipe R board score g
                "q" -> putStrLn $ ":: your final score is " ++ (show score) ++"!\n=== QUIT GAME === "
                _   -> do putStrLn "\ntry again!!"
                          gameLoop (board, score, g)
