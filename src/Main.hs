import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Color
import Data.Maybe

import System.Environment (getArgs)

{- 
I have choosen to make the I/O normal basic ASCII because otherwise some tests did't work on windows.
The coordinate system is centered top left with the y-axis going down.
The cord of a sausage is the cord of the part closest to (0,0)
-}

-- | different types of fields
data FieldType = Water | Grass | Grill deriving (Show, Eq)

type Coordinate = (Int,Int)

-- | the Chef in the game
data Player = Player Direction Coordinate

-- | left and right state of the sausage 0,1, and > 2 are raw, cooked, burned respectively
type State = (Int, Int)

-- | sausage in the game with cord isHorizontal isUp (UpState, DownState)
data Sausage = Sausage Coordinate Bool Bool (State, State) deriving (Show, Eq)

-- | a field in the game with a type and a coordinate
data Field = Field FieldType Coordinate deriving (Show)

-- | the board with all components on it the 2D array of fields has height as rows and width as columns 
-- last bool says if a player can still win (aka is there not already a sausage in the water)
data Board = Board [[Field]] [Sausage] Player Bool

-- | vector to move to a certain direction
type Direction = (Int,Int)

sausageLength :: Int
sausageLength = 2

----- Directions -----

west :: Direction
west = (-1, 0)

east :: Direction
east = (1, 0)

north :: Direction
north = (0, -1)

south :: Direction
south = (0, 1)

-- | all directions
directions :: [Direction]
directions = [north, east, south, west]

----- Mechanics -----

-- | get the next direction with Bool isClockwise
nextDirection :: Bool -> Direction -> Direction
nextDirection b d | b = directions !! ((getIndex d + 1) `mod` length directions)
                  | otherwise = directions !! ((getIndex d - 1) `mod` length directions) 
    where getIndex :: Direction -> Int
          getIndex d = head [i | i <- [0..length directions], directions !! i == d]

oppositeDirection :: Direction -> Direction
oppositeDirection d = nextDirection True (nextDirection True d)
 
-- | get all the cords of a sausage (2 in our case)
getCords :: Sausage -> [Coordinate]
getCords (Sausage (x,y) h u (_, _)) | h = [(x+i,y) | i <- [0..sausageLength-1]]
                                    | otherwise = [(x,y+i) | i <- [0..sausageLength-1]]

-- | gives the sausage on a certain cord, if there is one
getSausage :: Board -> Coordinate -> Maybe Sausage
getSausage (Board _ s _ _) c = listToMaybe [i |i <- s, c `elem` getCords i]

-- | moves the given sausages to the given direction
moveSausages :: Board -> [Sausage] -> [Sausage] -> Direction -> [Sausage]
moveSausages b all sub d = map (\x -> if x `elem` sub then grillSausage b (moveSausage d x) else x) all
    where moveSausage :: Direction -> Sausage -> Sausage
          moveSausage d@(dx, dy) s@(Sausage (x, y) h u (su, sd)) | (dy /= 0 && h) || (dx /= 0 && not h) = Sausage (x+dx, y+dy) h (not u) (su, sd)
                                                                 | otherwise = Sausage (x+dx, y+dy) h u (su, sd)

-- | gives a list of sausages that need to be pushed (input: sausagesNeedPush sausagesInProcess moveDirection board)
getPushedSausages :: [Sausage] -> [Sausage] -> Direction -> Board -> [Sausage]
getPushedSausages result [] _ _ = result
getPushedSausages result (s:sx) d b | s `elem` sx = getPushedSausages result sx d b
                                    | otherwise = getPushedSausages (s:result) (sx ++ getNextSausages d b s) d b

-- | get the sausages that lay on the cord when you move the given sausage to direction
getNextSausages :: Direction -> Board -> Sausage -> [Sausage]
getNextSausages d b s = [fromJust $ getSausage b i | i <- getNextPushCoords s d, isJust $ getSausage b i]
    where getNextPushCoords :: Sausage -> Direction -> [Coordinate]
          getNextPushCoords (Sausage (x,y) h u (_, _)) (dx, dy) | h && dx == 0 = [(x+dx+i, y+dy) | i <- [0..sausageLength-1]]
                                                                | h && dx == 1 = [(x+dx+sausageLength-1, y+dy)]
                                                                | h && dx == -1 = [(x+dx, y+dy)]
                                                                | not h && dy == 1 = [(x+dx, y+dy+sausageLength-1)]
                                                                | not h && dy == -1 = [(x+dx, y+dy)]
                                                                | otherwise = [(x+dx, y+dy+i) | i <- [0..sausageLength-1]]

-- | pushes a sausage if it lays on cord
push :: Coordinate -> Board -> Direction -> [Sausage]
push cord b@(Board _ alls _ _) d | isJust $ getSausage b cord = moveSausages b alls (getPushedSausages [] [fromJust $ getSausage b cord] d b) d
                                 | otherwise = alls

-- | moves a player and pushes the cord
move :: Board -> Bool -> Board
move b@(Board fields sausages p@(Player d@(dx, dy) (x, y)) win) bool | bool && isGrass (x+dx,y+dy) b = Board fields (push (x+2*dx, y+2*dy) b d) (movePlayer bool p) win
                                                                     | not bool && isGrass (x-dx,y-dy) b = Board fields (push (x-dx, y-dy) b (oppositeDirection d)) (movePlayer bool p) win
                                                                     | otherwise = b
    where movePlayer :: Bool -> Player -> Player
          movePlayer b (Player dir@(dx, dy) (x, y)) | b = Player dir (x+dx, y+dy)
                                                    | otherwise = Player dir (x-dx, y-dy)

-- | rotates a player and pushes the sausages that may be in the way
rotateP :: Board -> Bool -> Board
rotateP b bool = rotatePlayer (rotatePush bool b) bool
    where rotatePlayer :: Board -> Bool -> Board
          rotatePlayer (Board f s (Player dir cord) win) bool = Board f s (Player (nextDirection bool dir) cord) win

-- | gets the coordinates that may be in the way to rotate and pushes sausages on them in the given correct direction.
rotatePush :: Bool -> Board -> Board
rotatePush bool b@(Board _ _ pl@(Player d@(dx, dy) (x,y)) _)
    | bool = moveRotate bool b (nx, ny) ((x+nx, y+ny), (x+dx+nx,y+ny+dy))
    | otherwise = moveRotate bool b (px, py) ((x+px, y+py), (x+dx+px,y+dy+py)) 
        where (nx, ny) = nextDirection True d
              (px, py) = nextDirection False d
              moveRotate :: Bool -> Board -> Direction -> (Coordinate, Coordinate) -> Board
              moveRotate bool b@(Board f _ pl win) d (c1, c2) = Board f (push c1 (Board f (push c2 b d) pl win) (nextDirection bool d)) pl win

-- | checks if a sausage is completely in water or out of the map.
sausageInWater :: Board -> Sausage -> Bool
sausageInWater b s = length [c | c <- getCords s, not (inBoard b c) || getFieldType c b == Water] == sausageLength
    where inBoard :: Board -> Coordinate -> Bool
          inBoard b (x,y) = x >= 0 && y >= 0 && x < width b && y < height b

-- | removes all sausages that are completely in the water.          
checkInWater :: Board -> Board
checkInWater b@(Board f saus pl win) = Board f newSaus pl (win && length saus == length newSaus)
    where newSaus = [s | s <- saus, not (sausageInWater b s)]

-- | grills a sausage if it is on a grill
grillSausage :: Board -> Sausage -> Sausage
grillSausage b s@(Sausage cord h u (s1, s2)) | u = Sausage cord h u (s1, grill s2 s b)
                                             | otherwise = Sausage cord h u (grill s1 s b, s2)

-- | checks if the fieldtype of the field on the cord is grass
isGrass :: Coordinate -> Board -> Bool
isGrass c b = getFieldType c b == Grass

-- | grills a sausage on the state facing down
grill :: State -> Sausage -> Board -> State
grill (s1, s2) s b = (grillPart s1 (isGrill (head cords) b), grillPart s2 (isGrill (last cords) b))
    where cords = getCords s
          isGrill :: Coordinate -> Board -> Bool
          isGrill c b = getFieldType c b == Grill
          grillPart :: Int -> Bool -> Int
          grillPart i b | b = i + 1
                        | otherwise = i

-- | gives the field on cord
getField :: Coordinate -> Board -> Field
getField (x, y) (Board fields _ _ _) = fields !! y !! x

-- | gives the fieldtype of a field on a cord
getFieldType :: Coordinate -> Board -> FieldType
getFieldType c b = ft
    where (Field ft _) = getField c b

-- | parses the string representation of a direcion into a direction
parseDir :: String -> Direction
parseDir str | str == "N" = north
             | str == "E" = east        
             | str == "S" = south
             | str == "W" = west

-- | gives an empty board with a standard player
emptyBoard :: Board
emptyBoard = Board [[]] [] (Player north (0,0)) True

-- | parses the string representation of a board into a board
parseBoard :: String -> Board
parseBoard s = parse (0,0) s emptyBoard

-- | parses the next charcter of the string and makes the objects when needed.
parse :: Coordinate -> String -> Board -> Board
parse _ [] b@(Board fields saus pl win) = Board (init fields) saus pl win
parse c@(x, y) (s:sx) b@(Board fields saus pl win)
    | s == '~' = parse (x, y) sx (Board (addField fields (Field Water c)) saus pl win)
    | s == '#' = parse (x, y) sx (Board (addField fields (Field Grill c)) saus pl win)
    | s == '+' = parse (x, y) sx (Board (addField fields (Field Grass c)) saus pl win)
    | s == 'w' = parse (x+1, y) sx (Board fields (saus ++ [Sausage c True True ((0,0),(0,0))]) pl win)
    | s == 'n' = parse (x+1, y) sx (Board fields (saus ++ [Sausage c False True ((0,0),(0,0))]) pl win)
    | s == '\n' = parse (0, y+1) sx (Board (fields ++ [[]]) saus pl win)
    | s == 'N' = parse (x+1, y) sx (Board fields saus (Player north c) win)
    | s == 'E' = parse (x+1, y) sx (Board fields saus (Player east c) win)
    | s == 'S' = parse (x+1, y) sx (Board fields saus (Player south c) win)
    | s == 'W' = parse (x+1, y) sx (Board fields saus (Player west c) win)
    | otherwise = parse (x+1, y) sx b

-- | adds a field to the last row of fields
addField :: [[Field]] -> Field -> [[Field]]
addField alls f = init alls ++ [last alls ++ [f]]

-- | prints the string represantation of a board
printBoard :: Board -> String
printBoard b | isWin b = "Opgelost!\n"
             | otherwise = printB (0,0) b

-- | prints the board by printing everything cord by cord
printB :: Coordinate -> Board -> String
printB _ (Board [] _ _ _) = ""
printB cord@(x, y) b@(Board fs _ _ _)
   | x == length (head fs) && y == length fs - 1 = "\n"
   | x == length (head fs) = "\n" ++ printB (0,y+1) b
   | otherwise = getChars cord b ++ printB (x+1, y) b

-- | gives the characters that need to be printed on a certain cord
getChars :: Coordinate -> Board -> String
getChars c b = fieldToString (getField c b) ++ occupantToString c b

-- | gives the string representation of the occupant of a board
occupantToString :: Coordinate -> Board -> String
occupantToString c b | sausageToString c (getSausage b c) /= "" = sausageToString c (getSausage b c)
                     | playerToString c (getPlayer b c) /= "" = playerToString c (getPlayer b c)
                     | otherwise = " "

-- | gives the string representation of a field
fieldToString :: Field -> String
fieldToString (Field ft _) | ft == Water = "~"
                           | ft == Grass = "+"
                           | otherwise = "#"

-- | gives the string representation of a sausage part
sausageToString :: Coordinate -> Maybe Sausage -> String
sausageToString _ Nothing = ""
sausageToString cord sa@(Just (Sausage c h u ((s11,s12),(s21,s22)))) | cord == c && u = stateToString s11 h True
                                                                     | cord == c && not u = stateToString s21 h True
                                                                     | cord /= c && u = stateToString s12 h False
                                                                     | otherwise = stateToString s22 h False

-- | gives the string representation of a part of the state (input: statePart isHorizontal isBegin)
-- where the begin is the cord closest to (0, 0)    
stateToString :: Int -> Bool -> Bool -> String
stateToString state True True | state == 0 = "w"
                              | state == 1 = "4"
                              | otherwise = "d"
stateToString state True False | state == 0 = "e"
                               | state == 1 = "2"
                               | otherwise = "b"
stateToString state False True | state == 0 = "n"
                               | state == 1 = "1"
                               | otherwise = "a"
stateToString state False False | state == 0 = "s"
                                | state == 1 = "3"
                                | otherwise = "c"

-- | gives the string representation of a player
playerToString :: Coordinate -> Maybe Player -> String
playerToString _ Nothing = ""
playerToString cord (Just (Player dir c)) | cord == c = directionToString dir
                                          | otherwise = "x"

-- | gives a player on a certain cord
getPlayer :: Board -> Coordinate -> Maybe Player
getPlayer (Board _ _ pl@(Player (dx,dy) cord@(x,y)) _) c | cord == c || c == (x+dx, y+dy) = Just pl
                                                         | otherwise = Nothing

-- | gives the string representation of a direction
directionToString :: Direction -> String
directionToString dir | dir == north = "N"
                      | dir == south = "S"
                      | dir == east  = "E"
                      | dir == west  = "W"

-- | rotates to a given direction counter-clockwise or moves to that direction if it already faces that way or the opposite way
walk :: Direction -> Board -> Board
walk d b@(Board _ _ (Player dir _) _) | d == dir = checkInWater $ move b True
                                      | d == oppositeDirection dir = checkInWater $ move b False
                                      | otherwise = checkInWater $ walkRotate d (rotateP b False)
    where walkRotate :: Direction -> Board -> Board
          walkRotate d b@(Board _ _ (Player dir _) _) | d == dir = b
                                                      | otherwise = walkRotate d (rotateP b False)
                            
----- Graphics -----

-- | gives the width of the given board
width :: Board -> Int
width (Board fields _ _ _) = length $ head fields

-- | gives the height of the given board
height :: Board -> Int
height (Board fields _ _ _) = length fields

-- | gives the scale
boardScale  :: Int
boardScale = 100

-- | gives the picture of a sausage (input: isHorizontal (leftColor, rightColor))
sausagePicture :: Bool -> (Color, Color) -> Picture
sausagePicture h (c1, c2) | h = pictures (translate (fromIntegral $ boardScale `div` 8) 0 (rect c1):[translate (fromIntegral $ boardScale * 7 `div` 8) 0 (rect c2)])
                          | otherwise = pictures (translate 0 (fromIntegral $ (- boardScale) `div` 8) (rect c1):[translate 0 (fromIntegral $ (- boardScale) * 7 `div` 8) (rect c2)])

-- | gives the picture of a player facing the given direction
playerPicture :: Direction -> Picture
playerPicture dir = 
    Color yellow (rotate (directionToDegrees dir) 
    (pictures ([circleSolid (fromIntegral $ boardScale `div` 2)] ++
    [translate 0.0 (fromIntegral $ boardScale `div` 8) (rect yellow)] ++
    [translate 0.0 (fromIntegral $ boardScale * 3 `div` 4) (rect yellow)])))
        where directionToDegrees :: Direction -> Float
              directionToDegrees dir | dir == north = 0
                                     | dir == south = 180
                                     | dir == east = 90
                                     | dir == west = 270

-- | gives a rectangles with size 3/4 of the scale
rect :: Color -> Picture
rect cl = Color cl (rectangleSolid (fromIntegral $ boardScale * 3 `div` 4) (fromIntegral $ boardScale * 3 `div` 4))

-- | gives a picture of a field in a certain color
fieldSquare :: Color -> Picture
fieldSquare cl = Color cl (rectangleSolid (fromIntegral boardScale) (fromIntegral boardScale))

-- | translates picture to the coordinate it needs to be
coordToPicture :: Picture -> Board -> Coordinate -> Picture
coordToPicture p board (a,b) = 
    translate 
        ((fromIntegral a - fromIntegral (width board) * 0.5 + 0.5) * fromIntegral boardScale)
        ((fromIntegral b - fromIntegral (height board) * 0.5 + 0.5) * fromIntegral (- boardScale))
        p

-- | gives the game's window
window :: Board -> Display
window b = InWindow "UGent Functioneel Programmeren Opdracht 3" 
                   (width b * boardScale, boardScale * height b)
                   (0,0)

-- | renders all of the pictures into one big final picture
renderPictures :: Board -> Picture
renderPictures b@(Board fields sausages pl _) = pictures (fieldsToPictures b fields ++ sausagesToPictures b sausages ++ [playerToPicture b pl])

-- | gives the correct picture of a player
playerToPicture :: Board -> Player -> Picture
playerToPicture b (Player dir cord) = coordToPicture (playerPicture dir) b cord

-- | gives the correct pictures of the sausages
sausagesToPictures :: Board -> [Sausage] -> [Picture]
sausagesToPictures b saus = [sausageToPicture b s | s <- saus]
    where sausageToPicture :: Board -> Sausage -> Picture
          sausageToPicture b (Sausage cord h u (s1, s2)) | u = coordToPicture (sausagePicture h (stateToColors s1)) b cord
                                                         | otherwise = coordToPicture (sausagePicture h (stateToColors s2)) b cord

brown :: Color
brown = makeColorI 160 82 45 255

-- | gives the corresponding colors of a state
stateToColors :: State -> (Color, Color)
stateToColors (s1, s2) = (stToColor s1, stToColor s2)
    where stToColor :: Int -> Color
          stToColor s | s == 0 = rose
                      | s == 1 = brown
                      | s >= 2 = black

-- | gives the correct pictures of the fields
fieldsToPictures :: Board -> [[Field]] -> [Picture]
fieldsToPictures b fields = [pictures [fieldToPicture b field | field <- fieldRow] | fieldRow <- fields]
    where fieldToPicture :: Board -> Field -> Picture
          fieldToPicture b (Field ft cord) = coordToPicture (fieldSquare $ fieldTypeToColor ft b) b cord

-- | gives the corresponding color of a fieldtype, if the player won we indicate this by making the grass magenta.
fieldTypeToColor :: FieldType -> Board -> Color
fieldTypeToColor ft b | ft == Water = blue
                      | ft == Grill = orange
                      | ft == Grass && isWin b = magenta
                      | otherwise = green -- grass

-- | executes every tick of the game
tick :: Float -> Board -> Board
tick _ b = b

-- | check if key is down
isKey k1 (EventKey (SpecialKey k2)    Down  _ _ )  = k1 == k2
isKey _  _  = False

-- | processes the input
processInput :: Event -> Board -> Board
processInput e b@(Board _ _ (Player (dx, dy) (x,y)) _)
            | isKey KeyUp e = checkInWater $ move b True
            | isKey KeyDown e = checkInWater $ move b False
            | isKey KeyLeft e = checkInWater $ rotateP b False
            | isKey KeyRight e = checkInWater $ rotateP b True
            | otherwise = b

-- | checks if the player won
isWin :: Board -> Bool
isWin (Board _ saus _ win) = win && null [i | i <- saus, not (checkBaked i)] && not (null saus)
    where checkBaked :: Sausage -> Bool
          checkBaked (Sausage _ _ _ ((1,1),(1,1))) = True
          checkBaked s = False 

main :: IO ()
main = do (boardFile:steps) <- getArgs
          boardString       <- readFile boardFile
          if null steps
            then play (window (parseBoard boardString)) blue 4 (parseBoard boardString) renderPictures processInput tick
            else putStr . unlines . map printBoard $ scanl (flip walk) (parseBoard boardString) (map parseDir steps)
