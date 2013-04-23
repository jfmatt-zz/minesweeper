module Main(main) where

import System.Random
import Data.Functor		((<$>))
import Control.Applicative 	((<*>))
import Data.List 		(foldl')
--
-- TILE
--

data TileValue = Bomb | Touching Int
                        deriving (Eq)
data Tile = Tile {
  		value :: TileValue,
                marked :: Bool,
                hidden :: Bool
                } deriving (Eq)

instance Show Tile where
  show Tile { marked = True } = " X "
  show Tile { hidden = True } = " _ "
  show Tile { value = Bomb } = " * "
  show Tile { value = (Touching x) } = " " ++ show x ++ " "

newTile :: Tile
newTile = Tile { value=Touching 0, marked=False, hidden=True }

newBomb :: Tile
newBomb = Tile { value=Bomb, marked=False, hidden=True }

addTile :: Tile -> Int -> Tile
addTile (Tile Bomb         m h) _ 	= Tile Bomb 		m h
addTile (Tile (Touching x) m h) n 	= Tile (Touching (x+n)) m h

-- Used for setting up the numbers at the beginning of the game
-- Given a neighbor (Maybe Tile, since it comes from boardGet), is it a valid tile and a bomb?
isBomb :: Maybe Tile -> Bool
isBomb (Just t) = value t == Bomb
isBomb _ 	= False

-- Used for flooding the 0 tiles when one is hit
-- Given a neighbor, is it a 0 that's already been revealed?
isZero :: Maybe Tile -> Bool
isZero (Just t) = (value t == Touching 0) && not (hidden t)
isZero _	= False

--
-- BOARD
--

data Board = Board {
  		tiles :: [Tile],
                height :: Int,
                width :: Int
                } deriving (Eq)

instance Show Board where
  show b = showHeader (width b) ++ showTiles (tiles b) 0 where
    align3 n
      | n < 10   = ' ' : show n ++ " "
      | n >= 100 = error "Number is too wide to fit here..."
      | n >= 10  = ' ' : show n
    showHeader n = "    " ++ foldl' (\a b -> a ++ align3 b) "" [0..n-1] ++ "\n    " ++ foldl' (\a b -> a ++ " v ") "" [0..n-1] ++ "\n"
    
    showTiles [] _ = ""
    showTiles ts n = foldl' (++) (align3 n ++ ">") (map show thisRow) ++ "\n" ++ showTiles otherRows (n+1) where
      (thisRow, otherRows) = splitAt (width b) ts

boardGet2 :: Board -> Int -> Int -> Maybe Tile
boardGet2 b y x 
  | y >= 0 && x >= 0 && y < height b && x < width b  = Just $ tiles b !! yxToN y x b
  | otherwise					     = Nothing

numTiles :: Board -> Int
numTiles b = width b * (height b)

yxToN :: Int -> Int -> Board -> Int
yxToN y x b = (y * width b) + x

--
-- Construction
--

blankBoard :: Int -> Int -> Board
blankBoard w h = Board {
  			tiles=[ newTile | i <- [1..w], j <- [1..h]],
                        width=w,
                        height=h
                               }

placeBombs :: Board -> Int -> StdGen -> Board
placeBombs b 0 _ = b
placeBombs b n g 
  | n > numTiles b = placeBombs b (numTiles b) g
  | otherwise      = let
                      h = height b
                      w = width b

                      -- Reservoir polling
                      place t      _   0 = t
                      place (t:ts) rng n
                         | rngValue <= fromIntegral n / fromIntegral (length (t:ts)) 	= newBomb : place ts rng' (n - 1)
                         | otherwise							= t : place ts rng' n
                         where (rngValue, rng') = randomR (0, 1) rng :: (Double, StdGen)
                         
                      -- DOOO EEEEEEIT
                      t' = place (tiles b) g n
                     in
                      Board t' h w

neighborMoves :: [(Int, Int)]
neighborMoves = (,) <$> [-1, 0, 1] <*> [-1, 0, 1]

setTouching :: Board -> Board
setTouching b = let
                 w = width b
                 h = height b
                 nextCoord x y
                   | x == w - 1 = (0, y + 1)
                   | otherwise  = (x + 1, y)
                 getNeighbor x y deltas = boardGet2 b (y + fst deltas) (x + snd deltas)
                 
                 -- Add to this tile the # of neighbors who are bombs
                 process []     _ _ = []
                 process (t:ts) x y = addTile t (length . filter isBomb $ map (getNeighbor x y) neighborMoves) : process ts x' y' where
                   (x', y') = nextCoord x y
                 
                 t' = process (tiles b) 0 0
                in
                 Board t' h w

buildBoard :: Int -> Int -> Int -> StdGen -> Board
buildBoard w h b rng = setTouching $ placeBombs (blankBoard w h) b rng

--
-- ACTIONS
--

markTile :: Tile -> Tile
markTile (Tile v m h) = Tile v (not m) h

revealTile :: Tile -> Tile
revealTile (Tile v m _) = Tile v m False

liftBoardN :: (Tile -> Tile) -> Board -> Int -> Board
liftBoardN f (Board t h w) n = Board (l' f t n) h w where
  l' _ []     _ = []
  l' f (t:ts) 0 = f t:ts
  l' f (t:ts) n = t:l' f ts (n-1)

makeMove :: String -> Board -> Int -> Int -> Board
makeMove s b y x = liftBoardN f b (yxToN y x b) where
  f = case s of
    "m" -> markTile
    "d" -> revealTile
    _   -> id

floodZeroHelper :: Board -> Board -> Board
floodZeroHelper oldb b
  -- Recurse until nothing changes
  | b == oldb = b
  | otherwise = floodZeroHelper b (Board t' h w) where
    h = height b
    w = width b	
    nextCoord x y
      | x == w - 1 = (0, y + 1)
      | otherwise  = (x + 1, y)
    getNeighbor x y deltas = boardGet2 b (y + fst deltas) (x + snd deltas)
        
    -- Unhide tile if # of neighbors who are revealed 0s is >0
    process []                               _ _                           = []
    process (Tile (Touching n) mkd hdn:ts) x y
      | (length . filter isZero $ map (getNeighbor x y) neighborMoves) > 0 = Tile (Touching n) mkd False : process ts x' y' where
          (x', y') = nextCoord x y
    process (t:ts)                           x y                           = t                           : process ts x' y' where
          (x', y') = nextCoord x y
    
    t' = process (tiles b) 0 0

floodZeros :: Board -> Board
floodZeros = floodZeroHelper (blankBoard 0 0)

--
-- GAME LOOP
--

data GameState = Win | Loss | Continue | Unknown
                              deriving (Eq, Show)

getGameState :: Board -> GameState
getGameState = analyse . tiles where
  check Tile { value=Bomb, hidden=False, marked=False } = Loss
  check Tile { value=Touching n, marked=True } 		= Continue
  check Tile { value=Bomb, hidden=True, marked=False } 	= Continue
  check _ 						= Unknown
  
  analyse ts =
    case (foldl' f Unknown $ map check ts) of 
     Unknown -> Win
     a       -> a
    where
     f _ Loss            = Loss
     f Loss _            = Loss
     f a Win             = a
     f a Unknown         = a
     f Continue Continue = Continue
     f a b               = f b a
                    
runGame :: Board -> IO ()
runGame b =
  case (getGameState b) of
    Loss -> do
    	print  b
    	print "You lose."
    Win -> do
    	print b
        print "You win!"
    Continue -> do
	print b
        print "Next turn: Mark/unmark (m) or dig (d)?"
        m <- getLine
        print "Which row?"
        y <- getLine
        print "Which column?"
        x <- getLine
	let b' = floodZeros $ makeMove m b (read y) (read x) 
  	runGame b'

main :: IO ()
main = do
  -- Setup
  print "Height of board?"
  height <- getLine
  print "Width of board?"
  width <- getLine
  print "Number of bombs?"
  bombs <- getLine
  rng <- newStdGen
  
  -- Run
  runGame $ buildBoard (read width) (read height) (read bombs) rng