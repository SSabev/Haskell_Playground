-- Informatics 1 - Functional Programming 
-- Tutorial 5
--
-- Due: the tutorial of week 7 (5/6 November)


import Control.Monad( liftM, liftM2 )
import Data.List( nub )
import Test.QuickCheck( quickCheck, 
                        Arbitrary( arbitrary ),
                        oneof, elements, sized  )


-- The datatype 'Prop'

type Name = String
data Prop = Var Name
          | F
          | T
          | Not Prop
          | Prop :|: Prop
          | Prop :&: Prop
          | Prop :->: Prop
          | Prop :<->: Prop
          deriving (Eq, Ord)

type Names = [Name]
type Env = [(Name, Bool)]


-- Functions for handling Props

-- turns a Prop into a string approximating mathematical notation
showProp :: Prop -> String
showProp (Var x)        =  x
showProp (F)            =  "F"
showProp (T)            =  "T"
showProp (Not p)        =  "(~" ++ showProp p ++ ")"
showProp (p :|: q)      =  "(" ++ showProp p ++ "|" ++ showProp q ++ ")"
showProp (p :&: q)      =  "(" ++ showProp p ++ "&" ++ showProp q ++ ")"
showProp (p :->: q)     =  "(" ++ showProp p ++ "->" ++ showProp q ++ ")"
showProp (p :<->: q)    =  "(" ++ showProp p ++ "<->" ++ showProp q ++ ")"

-- evaluates a proposition in a given environment
eval :: Env -> Prop -> Bool
eval e (Var x)        =  lookUp x e
eval e (F)            =  False
eval e (T)            =  True
eval e (Not p)        =  not (eval e p)
eval e (p :|: q)      =  eval e p || eval e q
eval e (p :&: q)      =  eval e p && eval e q
eval e (p :->: q)     =  eval e p <= eval e q
eval e (p :<->: q)    =  eval e p == eval e q

-- retrieves the names of variables from a proposition - 
--  NOTE: variable names in the result must be unique
names :: Prop -> Names
names (Var x)        =  [x]
names (F)            =  []
names (T)            =  []
names (Not p)        =  names p
names (p :|: q)      =  nub (names p ++ names q)
names (p :&: q)      =  nub (names p ++ names q)
names (p :->: q)     =  nub (names p ++ names q)
names (p :<->: q)    =  nub (names p ++ names q)

-- creates all possible truth assignments for a set of variables
envs :: Names -> [Env]
envs []      =  [[]]
envs (x:xs)  =  [ (x,False):e | e <- envs xs ] ++
                [ (x,True ):e | e <- envs xs ]

-- checks whether a proposition is satisfiable
satisfiable :: Prop -> Bool
satisfiable p  =  or [ eval e p | e <- envs (names p) ]


-- Exercises ------------------------------------------------------------

-- 1.
p1 = ((Var "P" :|: Var "Q") :&: (Var "P" :&: Var "Q"))
p2 = ((Var "P" :|: Var "Q") :&: (Not (Var "P") :&: Not (Var "Q"))) 
p3 = ( (Var "P" :&: (Var "Q" :|: Var "R")) :&: ( (Not (Var "P") :|: Not (Var "Q")) :&: (Not (Var "P") :|: Not (Var "R")) ) )


-- 2. 
tautology :: Prop -> Bool
tautology x | satisfiable x = and [eval y x| y<-envs (names x)]
            | otherwise    = False

prop_taut1 :: Prop -> Bool
prop_taut1 x = tautology x || satisfiable (Not x)

prop_taut2 :: Prop -> Bool
prop_taut2 x = (satisfiable x == False) || (tautology (Not x) == False)


-- 3.
p4 = ((Var "P" :->: Var "Q") :&: (Var "P" :<->: Var "Q"))
p5 = ((Var "P" :->: Var "Q") :&: (Var "P" :&: Not (Var "Q")))
p6 = ((Var "P" :<->: Var "Q") :&: ((Var "P" :&: Not (Var "Q")) :|: (Not (Var "P") :&: Var "Q")))


-- 4.

equivalent :: Prop -> Prop -> Bool
equivalent x y = and [eval e x == eval e y | e <- envs(nub (names x ++ names y))]

equivalent' :: Prop -> Prop -> Bool
equivalent' x y = tautology (x:<->:y)

prop_equivalent :: Prop -> Prop -> Bool
prop_equivalent x y = equivalent x y == equivalent' x y 


-- 5.
subformulas :: Prop -> [Prop]
subformulas  x = nub (helperStuff x)

helperStuff :: Prop -> [Prop]
helperStuff (Not x) = (Not x) : helperStuff x
helperStuff (x:|:y) = (x:|:y) : helperStuff x ++ helperStuff y
helperStuff (x:&:y) = (x:&:y) : helperStuff x ++ helperStuff y
helperStuff (x:->:y) = (x:->:y) : helperStuff x ++ helperStuff y
helperStuff (x:<->:y) = (x:<->:y) : helperStuff x ++ helperStuff y
helperStuff x = [x]

-- Optional Material

-- 6.
-- check for negation normal form
isNNF :: Prop -> Bool
isNNF (p:&:q) = isNNF p && isNNF q
isNNF (p:|:q) = isNNF p && isNNF q
isNNF (Not (Var _)) = True
isNNF (Var _) = True
isNNF T = True
isNNF F = False


-- 7.
-- convert to negation normal form
toNNF :: Prop -> Prop
toNNF (p:|:q) = toNNF p :|: toNNF q
toNNF (p:&:q) = toNNF p :&: toNNF q
toNNF (Not p) = toNNF' p
toNNF (p:->:q) = toNNF' p :|: toNNF q
toNNF (p:<->:q) = toNNF((p:->q) :&: (q:->:p))   
toNNF T = T
toNNF F = F

toNNF' :: Prop -> Prop
toNNF' (p:|:q) = toNNF' p :|: toNNF' q
toNNF' (p:&:q) = toNNF' p :&: toNNF' q
toNNF' (p:->:q) = toNNF p :|: toNNF' q
toNNF' T = F
toNNF' F = T

-- check if result of toNNF is in neg. normal form
prop_NNF1 :: Prop -> Bool
prop_NNF1 p  =  isNNF (toNNF p)

-- check if result of toNNF is equivalent to its input
prop_NNF2 :: Prop -> Bool
prop_NNF2 p  =  equivalent p (toNNF p)


-- 8.
-- check whether a formula is in conj. normal form
isCNF :: Prop -> Bool
isCNF = undefined


-- 10.
-- transform a list of lists into a (CNF) formula
listsToCNF :: [[Prop]] -> Prop
listsToCNF = undefined


-- 11.
-- transform a CNF formula into a list of lists
listsFromCNF :: Prop -> [[Prop]]
listsFromCNF = undefined


-- 12.
-- transform an arbitrary formula into a list of lists
toCNFList :: Prop -> [[Prop]]
toCNFList = undefined



-- convert to conjunctive normal form
toCNF :: Prop -> Prop
toCNF p  =  listsToCNF (toCNFList p)

-- check if result of toCNF is equivalent to its input
prop_CNF :: Prop -> Bool
prop_CNF p  =  equivalent p (toCNF p)




-- For QuickCheck --------------------------------------------------------

instance Show Prop where
    show  =  showProp

instance Arbitrary Prop where
    arbitrary  =  sized prop
        where
          prop n | n <= 0     =  atom
                 | otherwise  =  oneof [ atom
                                       , liftM Not subform
                                       , liftM2 (:|:) subform subform
                                       , liftM2 (:&:) subform subform
                                       , liftM2 (:->:) subform subform
                                       , liftM2 (:<->:) subform' subform'
                                       ]
                 where
                   atom = oneof [liftM Var (elements ["P", "Q", "R", "S"]),
                                   elements [F,T]]
                   subform  =  prop (n `div` 2)
                   subform' =  prop (n `div` 4)


-- For Drawing Tables ----------------------------------------------------

-- centre a string in a field of a given width
centre :: Int -> String -> String
centre w s  =  replicate h ' ' ++ s ++ replicate (w-n-h) ' '
            where
            n = length s
            h = (w - n) `div` 2

-- make a string of dashes as long as the given string
dash :: String -> String
dash s  =  replicate (length s) '-'

-- convert boolean to T or F
fort :: Bool -> String
fort False  =  "F"
fort True   =  "T"

-- print a table with columns neatly centred
-- assumes that strings in first row are longer than any others
showTable :: [[String]] -> IO ()
showTable tab  =  putStrLn (
  unlines [ unwords (zipWith centre widths row) | row <- tab ] )
    where
      widths  = map length (head tab)

table p = tables [p]

tables :: [Prop] -> IO ()
tables ps  =
  let xs = nub (concatMap names ps) in
    showTable (
      [ xs            ++ ["|"] ++ [showProp p | p <- ps]           ] ++
      [ dashvars xs   ++ ["|"] ++ [dash (showProp p) | p <- ps ]   ] ++
      [ evalvars e xs ++ ["|"] ++ [fort (eval e p) | p <- ps ] | e <- envs xs]
    )
    where  dashvars xs        =  [ dash x | x <- xs ]
           evalvars e xs      =  [ fort (eval e (Var x)) | x <- xs ]

-- print a truth table, including columns for subformulas
fullTable :: Prop -> IO ()
fullTable = tables . filter nontrivial . subformulas
    where nontrivial :: Prop -> Bool
          nontrivial (Var _) = False
          nontrivial T       = False
          nontrivial F       = False
          nontrivial _       = True


-- Auxiliary functions

lookUp :: Eq a => a -> [(a,b)] -> b
lookUp z xys  =  the [ y | (x,y) <- xys, x == z ]
    where the [x]  =  x
          the _    =  error "eval: lookUp: variable missing or not unique"