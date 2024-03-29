-- Chess pieces drawing module
-- 


module ChessPieces (Picture,
                    display,
                    height, width,
                    flipH, flipV, rotateL, rotateR,
                    invert,
                    beside, above, superimpose,
                    repeatH, repeatV,
                    whiteSquare, greySquare, blackSquare, clearSquare,
                    bishop, king, knight, pawn, queen, rook
                   ) where

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLUT
import Data.IORef
import Data.List
import Control.Monad( liftM, liftM2 )
import Test.QuickCheck( Arbitrary (arbitrary), elements )


-- Colors

gray :: GL.GLfloat -> GL.Color3 GL.GLfloat
gray a = GL.Color3 a a a

charToRGB :: Char -> GL.Color3 GL.GLfloat
charToRGB '_' = gray 1    -- white (inverts to gray)
charToRGB '=' = gray 0.5  -- gray  (inverts to white)
charToRGB '.' = gray 1    -- white (inverts to black)
charToRGB '@' = gray 0    -- black (inverts to white)
charToRGB '#' = gray 0.2  -- dark grey (inverts to itself)
charToRGB _ = gray 0

invColor :: Char -> Char
invColor '_' = '='
invColor '=' = '_'
invColor '.' = '@'
invColor '@' = '.'
invColor  c  =  c

overlay :: Char -> Char -> Char
overlay ' ' c = c
overlay  c  _ = c


-- Coordinates

type Point = (Int,Int)


-- Pictures

data Picture = Pic [String] deriving (Eq)
data Pixel   = Px Char

instance Show Picture where
    show (Pic p) = unlines p

    -- QuickCheck generator;
    --  picture size is restricted to 150 characters
instance Arbitrary Picture where
    arbitrary = liftM2 f arbitrary arbitrary
        where
          f n xs = Pic $ g (1 + mod n 14) $ map h $ take 150 xs
          g n xs | length xs < n = []
                 | otherwise     = take n xs : g n (drop n xs)
          h (Px c) = c

instance Arbitrary Pixel where
    arbitrary = liftM Px $ elements " .#"


liftP :: ([String] -> [String]) -> Picture -> Picture
liftP f (Pic p) = Pic (f p)

liftP2 :: ([String] -> [String] -> [String]) -> Picture -> Picture -> Picture
liftP2 f (Pic p) (Pic q) = Pic (f p q)

height, width :: Picture -> Int
height (Pic p) = length p
width  (Pic p) | p == []   = 0
               | otherwise = maximum $ map length p

size :: Picture -> Point
size p = (width p,height p)


-- Main drawing function

display :: Picture -> IO ()
display pic = do
  let p = size pic
  wsize <- newIORef p
  initialDisplayMode $= [DoubleBuffered]
  initialWindowSize $= pointToSize p
  getArgsAndInitialize
  w <- createWindow "Chess"
  displayCallback $= (readIORef wsize >>= (\p -> draw p pic))
  reshapeCallback $= Just (\x -> (viewport $= (Position 0 0, x) >> writeIORef wsize (sizeToPoint x)))
  --actionOnWindowClose $= ContinueExectuion
  draw p pic
  mainLoop

draw :: Point -> Picture -> IO ()
draw p pic = do clear [ColorBuffer]
                loadIdentity
                GL.color $ GL.Color3 0.6 0.6 (1.0::GL.GLfloat)
                GL.renderPrimitive GL.Polygon $ mapM_ GL.vertex 
                      [GL.Vertex3 (-1) (-1) 0, GL.Vertex3 1 (-1) 0, GL.Vertex3 1 1 0, GL.Vertex3 (-1) 1 (0.0::GL.GLfloat)]
                sequence_ (map (drawLine p) (scan pic))
                swapBuffers

drawLine :: Point -> (Char,Point,Point) -> IO ()
drawLine p (c,p1,p2) = do GL.color (charToRGB c) 
                          GL.renderPrimitive GL.Lines (mapM_ (pointToVertex p) [p1,p2])

pointToSize :: Point -> Size
pointToSize (x,y) = Size (fromIntegral x) (fromIntegral y)

sizeToPoint :: Size -> Point
sizeToPoint (Size x y) = (fromIntegral x,fromIntegral y)

pointToVertex :: Point -> Point -> IO ()
pointToVertex (xsize,ysize) (x,y) = GL.vertex $ GL.Vertex3
       ((2 * (fromIntegral x) / fromIntegral xsize ) - 1.0::GL.GLfloat) 
       ((-2 * (fromIntegral y) / fromIntegral ysize ) + 1.0::GL.GLfloat)
       0

-- Interpreting pictures --------------------------

scan :: Picture -> [(Char,Point,Point)]
scan (Pic xss) = concat $ zipWith g [1..] (map (h . scanLine) xss)
    where
      f y (c,x0,x1) = (c,(x0,y),(x1,y))
      g y xs = map (f y) xs
      h = filter (\(c,_,_) -> c /= ' ')

scanLine :: String -> [(Char,Int,Int)]
scanLine [] = []
scanLine (c:cs) = s c 0 0 cs
    where
      s c i j [] = [(c,i,j+1)]
      s c i j (d:ds) | c == d    = s c i (j+1) ds
                     | otherwise = (c,i,j+1) : (s d (j+1) (j+1) ds)

---------------------------------------------------

fillOut :: Int -> Picture -> Picture
fillOut n pic@(Pic p) = Pic $ map (f (width pic)) (p ++ replicate (n - height pic) [])
    where
      f m []     = replicate m ' '
      f m (x:xs) = x : f (m-1) xs

specialZip :: (a -> a -> a) -> [a] -> [a] -> [a]
specialZip _ [] ys = ys
specialZip _ xs [] = xs
specialZip f (x:xs) (y:ys) = f x y : specialZip f xs ys

---------------------------------------------------

flipV :: Picture -> Picture
flipV = liftP $ map reverse

flipH :: Picture -> Picture
flipH =  liftP $ reverse 

flipDiag :: Picture -> Picture
flipDiag =  liftP $ transpose

rotateL :: Picture -> Picture
rotateL =  flipDiag . flipV

rotateR :: Picture -> Picture
rotateR =  flipDiag . flipH

invert :: Picture -> Picture
invert =  liftP $ map (map invColor)

superimpose :: Picture -> Picture -> Picture
superimpose p1 p2 = fillOut 0 $ liftP2 (specialZip $ specialZip overlay) p1 p2

beside :: Picture -> Picture -> Picture
beside p1 p2 = liftP2 (zipWith (++)) (fillOut (height p2) p1) (fillOut (height p1) p2)

above :: Picture -> Picture -> Picture
above p1 p2 = fillOut 0 $ liftP2 (++) p1 p2
  
repeatH :: Int -> Picture -> Picture
repeatH n p | n <= 1    = p
            | otherwise = beside p (repeatH (n-1) p)

repeatV :: Int -> Picture -> Picture
repeatV n p | n <= 1    = p
            | otherwise = above p (repeatV (n-1) p)


-- The pieces themselves

whiteSquare, greySquare, blackSquare, clearSquare :: Picture
whiteSquare = Pic $ replicate 58 (replicate 58 '_')
greySquare  = Pic $ replicate 58 (replicate 58 '=')
blackSquare = greySquare
clearSquare = Pic $ replicate 58 (replicate 58 ' ')
bishop  = Pic $ bishopRaster
king    = Pic $ kingRaster
knight  = Pic $ knightRaster
pawn    = Pic $ pawnRaster
queen   = Pic $ queenRaster
rook    = Pic $ rookRaster

bishopRaster = (
 ["                                                          ",
  "                                                          ",
  "                                                          ",
  "                            ##                            ",
  "                          ######                          ",
  "                         ###..###                         ",
  "                         ##....##                         ",
  "                         ##....##                         ",
  "                         ###..###                         ",
  "                          ######                          ",
  "                           ####                           ",
  "                         ########                         ",
  "                        ###....###                        ",
  "                      ####......####                      ",
  "                    ####..........####                    ",
  "                   ###..............###                   ",
  "                  ###................###                  ",
  "                 ###..................###                 ",
  "                ###.........##.........###                ",
  "                ##..........##..........##                ",
  "               ###..........##..........###               ",
  "               ##...........##...........##               ",
  "               ##.......##########.......##               ",
  "               ##.......##########.......##               ",
  "               ##...........##...........##               ",
  "               ##...........##...........##               ",
  "               ##...........##...........##               ",
  "               ###..........##..........###               ",
  "                ##..........##..........##                ",
  "                ###....................###                ",
  "                 ##....................###                ",
  "                 ###..................###                 ",
  "                  ###................###                  ",
  "                   ####################                   ",
  "                   ####################                   ",
  "                   ##................##                   ",
  "                  ###................###                  ",
  "                  ##..................##                  ",
  "                  ######################                  ",
  "                 ########################                 ",
  "                 ###..................###                 ",
  "                 #####..............#####                 ",
  "                 ########################                 ",
  "                      ##############                      ",
  "                          ######                          ",
  "                        ####..####                        ",
  "        ##################......##################        ",
  "      ##################..........##################      ",
  "    ####..........................................####    ",
  "    ###.....................##.....................###    ",
  "     ##...................######...................##     ",
  "     ###.########.......####  ####.......########.###     ",
  "      ####################      ####################      ",
  "       ##        #######          #######        ##       ",
  "                                                          ",
  "                                                          ",
  "                                                          ",
  "                                                          "]
               )

kingRaster = (
 ["                                                          ",
  "                                                          ",
  "                                                          ",
  "                                                          ",
  "                           ####                           ",
  "                           #..#                           ",
  "                           #..#                           ",
  "                         ###..###                         ",
  "                         #......#                         ",
  "                         ###..###                         ",
  "                           #..#                           ",
  "                           #..#                           ",
  "                           #..#                           ",
  "                           #..#                           ",
  "                           #..#                           ",
  "                          ######                          ",
  "                         ###..###                         ",
  "                         ##....##                         ",
  "                        ###....###                        ",
  "           #######      ##......##      #######           ",
  "         ###########    ##......##    ###########         ",
  "       ####.......####  ##......##  ####.......####       ",
  "      ###...........######......######...........###      ",
  "     ###..............####......####..............###     ",
  "     ##................####....####................##     ",
  "    ###.................###....###.................###    ",
  "    ##...................###..###...................##    ",
  "    ##...................###..###...................##    ",
  "    ##....................######....................##    ",
  "    ##....................######....................##    ",
  "    ##.....................####.....................##    ",
  "    ###....................####....................###    ",
  "     ##.....................##....................###     ",
  "     ###....................##....................##      ",
  "      ###...................##...................###      ",
  "       ###..................##..................###       ",
  "        ###...........##############...........###        ",
  "         ###.....########################.....###         ",
  "          ############..............############          ",
  "           ######........................######           ",
  "            ##..............................##            ",
  "            ##..............................##            ",
  "            ##........##############........##            ",
  "            ##...########################...##            ",
  "            ##########..............##########            ",
  "            #####........................#####            ",
  "            ##.........############.........##            ",
  "            ##....######################....##            ",
  "            ##.########............########.##            ",
  "            ######......................######            ",
  "            ######......................######            ",
  "               ########............########               ",
  "                  ######################                  ",
  "                       ############                       ",
  "                                                          ",
  "                                                          ",
  "                                                          ",
  "                                                          "]
             )

knightRaster = (
 ["                                                          ",
  "                                                          ",
  "                                                          ",
  "                                                          ",
  "                                                          ",
  "               #         ##                               ",
  "              ###       ####                              ",
  "              #####     ####                              ",
  "               #####   ######                             ",
  "               ##.### ###..##                             ",
  "               ##..#####...######                         ",
  "               ##...####...#########                      ",
  "               ##..#####...##...######                    ",
  "              ###.###.......#.....######                  ",
  "             ###..#.................#####                 ",
  "            ###.......................####                ",
  "            ##.........................####               ",
  "           ###..####....................####              ",
  "           ##..####......................####             ",
  "           ##..####......................#####            ",
  "           ##..###........................####            ",
  "          ###..#..............##...........####           ",
  "          ##..................##...........####           ",
  "          ##..................##............####          ",
  "         ###..................##............####          ",
  "         ##...................##.............####         ",
  "        ###..................###.............####         ",
  "       ###..................###..............####         ",
  "      ###...................###...............####        ",
  "      ##...................####...............####        ",
  "     ###.................######...............####        ",
  "     ##................####  ##................####       ",
  "    ###.##...........####    ##................####       ",
  "    ##.###.........####     ###................####       ",
  "    ##.###........###       ##.................####       ",
  "    ##.##........###       ###.................#####      ",
  "    ##......##..###        ##...................####      ",
  "    ##.....###.###        ###...................####      ",
  "    ###...#######        ###....................####      ",
  "     ########.##        ###.....................####      ",
  "       #########       ###......................####      ",
  "            ###       ###.......................#####     ",
  "                     ###........................#####     ",
  "                    ###..........................####     ",
  "                    ##...........................####     ",
  "                   ###...........................####     ",
  "                  ###............................####     ",
  "                  ##.............................####     ",
  "                 ###.............................####     ",
  "                 ##..............................####     ",
  "                 ##..............................####     ",
  "                 ####################################     ",
  "                 ####################################     ",
  "                                                          ",
  "                                                          ",
  "                                                          ",
  "                                                          ",
  "                                                          "]
               )

pawnRaster = (
 ["                                                          ",
  "                                                          ",
  "                                                          ",
  "                                                          ",
  "                                                          ",
  "                                                          ",
  "                                                          ",
  "                                                          ",
  "                           ####                           ",
  "                         ########                         ",
  "                        ###....###                        ",
  "                       ###......###                       ",
  "                       ##........##                       ",
  "                       ##........##                       ",
  "                       ##........##                       ",
  "                       ##........##                       ",
  "                       ###......###                       ",
  "                        ###....###                        ",
  "                      #####....#####                      ",
  "                     ###..........###                     ",
  "                    ###............###                    ",
  "                    ##..............##                    ",
  "                   ###..............###                   ",
  "                   ##................##                   ",
  "                   ##................##                   ",
  "                   ##................##                   ",
  "                   ##................##                   ",
  "                   ##................##                   ",
  "                   ##................##                   ",
  "                   ###..............###                   ",
  "                    ##..............##                    ",
  "                    ###............###                    ",
  "                     ####........####                     ",
  "                     ####........####                     ",
  "                   ####............####                   ",
  "                  ###................###                  ",
  "                 ###..................###                 ",
  "                ###....................###                ",
  "               ###......................###               ",
  "               ##........................##               ",
  "              ###........................###              ",
  "              ##..........................##              ",
  "             ###..........................###             ",
  "             ##............................##             ",
  "             ##............................##             ",
  "            ###............................###            ",
  "            ##..............................##            ",
  "            ##..............................##            ",
  "            ##..............................##            ",
  "            ##..............................##            ",
  "            ##################################            ",
  "            ##################################            ",
  "                                                          ",
  "                                                          ",
  "                                                          ",
  "                                                          ",
  "                                                          ",
  "                                                          "]
             )

queenRaster = (
 ["                                                          ",
  "                                                          ",
  "                                                          ",
  "                                                          ",
  "                            ##                            ",
  "                          ######                          ",
  "              #####      ###..###      #####              ",
  "             #######     ##....##     #######             ",
  "             ##...##     ##....##     ##...##             ",
  "             ##...##     ########     ##...##             ",
  "    ##       ##...##      ######      ##...##       ##    ",
  "  ######     #######        ##        #######     ######  ",
  " ###..###     #####        ####        #####     ###..### ",
  " ##....##       ###        ####        ###       ##....## ",
  " ###...##       ###        ####        ###       ##...### ",
  "  #######       ###        ####        ###       #######  ",
  "   #####        ####      ######      ####        #####   ",
  "     ###        ####      ##..##      ####        ###     ",
  "     ####       ####      ##..##      ####       ####     ",
  "     ####       #####     ##..##     #####       ####     ",
  "     #####      ##.##     ##..##     ##.##      #####     ",
  "      #####     ##.##    ###..###    ##.##     #####      ",
  "      ##.##    ###.###   ##....##   ###.###    ##.##      ",
  "      ##.###   ##...##   ##....##   ##...##   ###.##      ",
  "      ##..##   ##...###  ##....##  ###...##   ##..##      ",
  "      ##..###  ##....## ###....### ##....##  ###..##      ",
  "      ###..##  ##....## ##......## ##....##  ##..###      ",
  "       ##..### ##....#####......#####....## ###..##       ",
  "       ##...#####.....####......####.....#####...##       ",
  "       ##....####.....####......####.....####....##       ",
  "       ##....####..#..###..####..###..#..####....##       ",
  "       ###....###.######################.###....###       ",
  "        ##.################....################.##        ",
  "        ##########......................##########        ",
  "        ###....................................###        ",
  "         ##....................................##         ",
  "         ###..................................###         ",
  "          ###.......##################.......###          ",
  "           ####################################           ",
  "            ########..................########            ",
  "            ###............................###            ",
  "             ##............................##             ",
  "             ##......################......##             ",
  "             ################################             ",
  "             ########................########             ",
  "             ##............................##             ",
  "            ###.....##################.....###            ",
  "            ##################################            ",
  "           #########..................#########           ",
  "           ##................................##           ",
  "           ##................................##           ",
  "           #########..................#########           ",
  "             ################################             ",
  "                    ##################                    ",
  "                                                          ",
  "                                                          ",
  "                                                          ",
  "                                                          "]
              )

rookRaster = (
 ["                                                          ",
  "                                                          ",
  "                                                          ",
  "                                                          ",
  "                                                          ",
  "                                                          ",
  "                                                          ",
  "           #########     ########     #########           ",
  "           #########     ########     #########           ",
  "           ##.....##     ##....##     ##.....##           ",
  "           ##.....##     ##....##     ##.....##           ",
  "           ##.....#########....#########.....##           ",
  "           ##.....#########....#########.....##           ",
  "           ##................................##           ",
  "           ####################################           ",
  "           ####################################           ",
  "            ###............................###            ",
  "             ####........................####             ",
  "               ############################               ",
  "                ##########################                ",
  "                ##......................##                ",
  "                ##......................##                ",
  "                ##......................##                ",
  "                ##......................##                ",
  "                ##......................##                ",
  "                ##......................##                ",
  "                ##......................##                ",
  "                ##......................##                ",
  "                ##......................##                ",
  "                ##......................##                ",
  "                ##......................##                ",
  "                ##......................##                ",
  "                ##......................##                ",
  "                ##......................##                ",
  "                ##......................##                ",
  "                ##......................##                ",
  "                ##......................##                ",
  "                ##......................##                ",
  "                ##########################                ",
  "               ############################               ",
  "              ###........................###              ",
  "            ##################################            ",
  "            ##################################            ",
  "            ##..............................##            ",
  "            ##..............................##            ",
  "            ##..............................##            ",
  "            ##..............................##            ",
  "        ##########################################        ",
  "        ##########################################        ",
  "        ##......................................##        ",
  "        ##########################################        ",
  "        ##########################################        ",
  "                                                          ",
  "                                                          ",
  "                                                          ",
  "                                                          ",
  "                                                          ",
  "                                                          "]
             )
