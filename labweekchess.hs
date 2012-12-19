-- Informatics 1 - Functional Programming 
-- Lab week tutorial part II
-- Name: Stefan Sabev
-- Nr. : 1024819

import ChessPieces
import Test.QuickCheck



-- Exercise 9:


intermed1 :: Picture
intermed1 = beside knight (invert knight)
intermed2 :: Picture
intermed2 = beside (invert knight) knight
intermed3 :: Picture 
intermed3 = beside (flipV(invert knight))(flipV knight)

pic1 :: Picture
pic1 = above intermed1 intermed2

pic2 :: Picture
pic2 = above intermed1 intermed3


-- Exercise 10:
-- a)

block1 :: Picture
block1 = beside whiteSquare blackSquare

emptyRow :: Picture
emptyRow = repeatH 4 block1

-- b)

otherEmptyRow :: Picture
otherEmptyRow = flipV emptyRow

-- c)

blockOfRows :: Picture
blockOfRows = above emptyRow otherEmptyRow

middleBoard :: Picture
middleBoard = repeatV 2 blockOfRows

-- d)

whiteRookKnight1 :: Picture
whiteRookKnight1 = beside (superimpose rook blackSquare)(superimpose knight whiteSquare)

whiteBishopQueen :: Picture
whiteBishopQueen = beside (superimpose bishop blackSquare)(superimpose queen whiteSquare)

whiteLeftPart :: Picture
whiteLeftPart = beside whiteRookKnight1 whiteBishopQueen

whiteKingBishop :: Picture
whiteKingBishop = beside (superimpose king blackSquare)(superimpose bishop whiteSquare)

whiteRookKnight2 :: Picture
whiteRookKnight2 = beside (superimpose knight blackSquare)(superimpose rook whiteSquare)

whiteRightPart :: Picture
whiteRightPart = beside whiteKingBishop whiteRookKnight2

whiteRow :: Picture
whiteRow = beside whiteLeftPart whiteRightPart

blackRow :: Picture
blackRow = invert whiteRow

-- e)

blackPawnBlock :: Picture
blackPawnBlock = beside (superimpose (invert pawn) blackSquare)(superimpose (invert pawn) whiteSquare)

blackPawnRow :: Picture
blackPawnRow = repeatH 4 blackPawnBlock

blacks :: Picture
blacks = above blackRow blackPawnRow

whitePawnBlock :: Picture
whitePawnBlock = beside (superimpose pawn whiteSquare)(superimpose pawn blackSquare)

whitePawnRow :: Picture
whitePawnRow = repeatH 4 whitePawnBlock

whites :: Picture 
whites = above whitePawnRow whiteRow

bigBlock1 :: Picture
bigBlock1 = above blacks middleBoard


populatedBoard :: Picture
populatedBoard = above bigBlock1 whites



-- Functions --

twoBeside :: Picture -> Picture
twoBeside x = beside x (invert x)


-- Exercise 11:

twoAbove :: Picture -> Picture
twoAbove x = above x (invert x)

fourPictures :: Picture -> Picture
fourPictures x = beside (above x (invert x))(above (invert x) x)



-- Optional material -------------------------

-- Exercise 12:

-- The properties p, q & r are three pictures used to determine whether the above function is associative.
-- i.e. whether p above the merged q and r is the same as the merged p and q above r
prop_assoc_above :: Picture -> Picture -> Picture -> Bool
prop_assoc_above p q r = p `above` (q `above` r) == (p `above` q) `above` r


prop_2x_invert :: Picture -> Bool
prop_2x_invert p = invert (invert p) == p

prop_assoc_beside :: Picture -> Picture -> Picture -> Bool
prop_assoc_beside p q r = p `beside` (q `beside` r) == (p `beside` q) `beside` r

prop_assoc_superimpose :: Picture -> Picture -> Picture -> Bool
prop_assoc_superimpose p q r = p `superimpose` (q `superimpose` r) == (p `superimpose` q) `superimpose` r

prop_2x_flipv :: Picture -> Bool
prop_2x_flipv p = flipV (flipV p) == p

prop_4x_rotateR :: Picture -> Bool
prop_4x_rotateR p = rotateR (rotateR (rotateR (rotateR p))) == p