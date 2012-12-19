import Test.QuickCheck
import List

-- Bool

eqBool :: Bool -> Bool -> Bool
eqBool False False  =  True
eqBool False True   =  False
eqBool True  False  =  False
eqBool True  True   =  True

showBool :: Bool -> String
showBool False  =  "False"
showBool True   =  "True"

-- Season 

data Season = Winter | Spring | Summer | Fall
  deriving (Eq)

next :: Season -> Season
next Winter  =  Spring
next Spring  =  Summer
next Summer  =  Fall
next Fall    =  Winter

eqSeason :: Season -> Season -> Bool
eqSeason Winter Winter  =  True
eqSeason Spring Spring  =  True
eqSeason Summer Summer  =  True
eqSeason Fall   Fall    =  True
eqSeason x      y       =  False

showSeason :: Season -> String
showSeason Winter  =  "Winter"
showSeason Spring  =  "Spring"
showSeason Summer  =  "Summer"
showSeason Fall    =  "Fall"

toInt :: Season -> Int
toInt Winter  =  0
toInt Spring  =  1
toInt Summer  =  2
toInt Fall    =  3

fromInt :: Int -> Season
fromInt 0  =  Winter
fromInt 1  =  Spring
fromInt 2  =  Summer
fromInt 3  =  Fall

next' :: Season -> Season
next' x  =  fromInt ((toInt x + 1) `mod` 4)

prop_next :: Season -> Bool
prop_next x  =  next x == next' x

eqSeason' :: Season -> Season -> Bool
eqSeason' x y  =  (toInt x == toInt y)

prop_eqSeason :: Season -> Season -> Bool
prop_eqSeason x y  =  eqSeason x y == eqSeason' x y

-- Shape

type  Radius  =  Float
type  Width   =  Float
type  Height  =  Float

data  Shape  =  Circle Radius
             |  Rect Width Height
  deriving (Eq,Show)

area :: Shape -> Float
area (Circle r)  =  pi * r^2
area (Rect w h)  =  w * h

eqShape :: Shape -> Shape -> Bool
eqShape (Circle r) (Circle r')   =  (r == r')
eqShape (Rect w h) (Rect w' h')  =  (w == w') && (h == h')
eqShape x          y             =  False

showShape :: Shape -> String
showShape (Circle r)  =  "Circle " ++ showF r
showShape (Rect w h)  =  "Rect " ++ showF w ++ " " ++ showF h

showF :: Float -> String
showF x | x >= 0     =  show x
        | otherwise  =  "(" ++ show x ++ ")"

prop_eqShape :: Shape -> Shape -> Bool
prop_eqShape x y  =  eqShape x y == (x == y)

prop_showShape :: Shape -> Bool
prop_showShape x  =  showShape x == show x

data  List a  =  Nil
              |  Cons a (List a)

append :: List a -> List a -> List a
append Nil ys          =  ys
append (Cons x xs) ys  =  Cons x (append xs ys)

fromList :: List a -> [a]
fromList Nil           =  []
fromList (Cons x xs)   =  x : fromList xs

toList :: [a] -> List a
toList []      =  Nil
toList (x:xs)  =  Cons x (toList xs)

prop_list :: [Int] -> Bool
prop_list xs  =  fromList (toList xs) == xs

prop_append :: [Int] -> [Int] -> Bool
prop_append xs ys  =
  fromList (append (toList xs) (toList ys)) == xs ++ ys

-- Expressions

data Exp  =  Lit Int
          |  Add Exp Exp
          |  Mul Exp Exp

evalExp :: Exp -> Int
evalExp (Lit n)    =  n
evalExp (Add e f)  =  evalExp e + evalExp f
evalExp (Mul e f)  =  evalExp e * evalExp f

showExp :: Exp -> String
showExp (Lit n)    =  show n
showExp (Add e f)  =  par (showExp e ++ "+" ++ showExp f)
showExp (Mul e f)  =  par (showExp e ++ "*" ++ showExp f)

par :: String -> String
par s  =  "(" ++ s ++ ")"

e0, e1 :: Exp
e0 = Add (Lit 2) (Mul (Lit 3) (Lit 3))
e1 = Mul (Add (Lit 2) (Lit 3)) (Lit 3)

prop_Exp :: Bool
prop_Exp =
      showExp e0 == "(2+(3*3))"
  &&  evalExp e0 == 11
  &&  showExp e1 == "((2+3)*3)"
  &&  evalExp e1 == 15

-- Propositions

type Name = String
data Prp = Var Name
          | F
          | T
          | Not Prp
          | Prp :|: Prp
          | Prp :&: Prp
          deriving (Eq, Ord, Show)

type Names = [Name]
type Env = [(Name,Bool)]

showPrp :: Prp -> String
showPrp (Var x)   =  x
showPrp (F)       =  "F"
showPrp (T)       =  "T"
showPrp (Not p)   =  par ("~" ++ showPrp p)
showPrp (p :|: q) =  par (showPrp p ++ "|" ++ showPrp q)
showPrp (p :&: q) =  par (showPrp p ++ "&" ++ showPrp q)

names :: Prp -> Names
names (Var x)    =  [x]
names (F)        =  []
names (T)        =  []
names (Not p)    =  names p
names (p :|: q)  =  nub (names p ++ names q)
names (p :&: q)  =  nub (names p ++ names q)

eval :: Env -> Prp -> Bool
eval e (Var x)        =  lookUp e x
eval e (F)            =  False
eval e (T)            =  True
eval e (Not p)        =  not (eval e p)
eval e (p :|: q)      =  eval e p || eval e q
eval e (p :&: q)      =  eval e p && eval e q

lookUp :: Eq a => [(a,b)] -> a -> b
lookUp xys x  =  the [ y | (x',y) <- xys, x == x' ]
  where
  the [x]  =  x

p0 :: Prp
p0 =  (Var "a" :&: Var "b") :|:
      (Not (Var "a") :&: Not (Var "b"))

env0 :: Env
env0 =  [("a",False), ("b",False)]

prop_Prp :: Bool
prop_Prp =
  showPrp p0  ==  "((a&b)|((~a)&(~b)))" &&
  names p0  ==  ["a","b"]  &&
  eval env0 p0  ==  True  &&
  lookUp env0 "a"  ==  False

envs :: Names -> [Env]
envs []      =  [[]]
envs (x:xs)  =  [ (x,b):e | b <- bs, e <- envs xs ]
  where
  bs = [False,True]

prop_envs :: Bool
prop_envs =
      envs []  ==
        [[]]
  &&  envs ["b"]  ==
        [[("b",False)],
         [("b",True )]]
  &&  envs ["a","b"]  ==
        [[("a",False),("b",False)],
         [("a",False),("b",True )],
         [("a",True ),("b",False)],
         [("a",True ),("b",True )]] 

satisfiable :: Prp -> Bool
satisfiable p  =  or [ eval e p | e <- envs (names p) ]

prop_satisfiable :: Bool
prop_satisfiable  =
  satisfiable p0 == True  

-- All sublists of a list

subs :: [a] -> [[a]]
subs []      =  [[]]
subs (x:xs)  =  subs xs ++ [ x:ys | ys <- subs xs ]

prop_subs :: Bool
prop_subs =
      subs []  ==
        [[] :: [String]]
  &&  subs ["b"]  ==
        [[], ["b"]]
  &&  subs ["a","b"] ==
        [[], ["b"], ["a"], ["a","b"]]

-- Micro-Haskell

data  Univ  =  UBool Bool
	    |  UInt Int
            |  UList [Univ]
	    |  UFun (Univ -> Univ)

data  Hask  =  HTrue
	    |  HFalse
            |  HIf Hask Hask Hask
            |  HLit Int
            |  HEq Hask Hask
            |  HAdd Hask Hask
	    |  HVar Name
	    |  HLam Name Hask
	    |  HApp Hask Hask

type  HEnv  =  [(Name, Univ)]

showUniv :: Univ -> String
showUniv (UBool b)   =  show b
showUniv (UInt i)    =  show i
showUniv (UList us)  =
  "[" ++ concat (intersperse "," (map showUniv us)) ++ "]"

eqUniv :: Univ -> Univ -> Bool
eqUniv (UBool b) (UBool c)    =  b == c
eqUniv (UInt i) (UInt j)      =  i == j
eqUniv (UList us) (UList vs)  =  and [ eqUniv u v | (u,v) <- zip us vs ]
eqUniv u v                    =  False

hEval :: Hask -> HEnv -> Univ
hEval HTrue r         =  UBool True
hEval HFalse r        =  UBool False
hEval (HIf c d e) r   =
  hif (hEval c r) (hEval d r) (hEval e r)
  where  hif (UBool b) v w  =  if b then v else w
hEval (HLit i) r      =  UInt i
hEval (HEq d e) r     =  heq (hEval d r) (hEval e r)
  where  heq (UInt i) (UInt j) = UBool (i == j)
hEval (HAdd d e) r    =  hadd (hEval d r) (hEval e r)
  where  hadd (UInt i) (UInt j) = UInt (i + j)
hEval (HVar x) r      =  lookUp r x
hEval (HLam x e) r    =  UFun (\v -> hEval e ((x,v):r))
hEval (HApp d e) r    =  happ (hEval d r) (hEval e r)
  where  happ (UFun f) v  =  f v

h0 =
 (HApp
   (HApp
     (HLam "x" (HLam "y" (HAdd (HVar "x") (HVar "y"))))
     (HLit 3))
   (HLit 4))

prop_h0 = eqUniv (hEval h0 []) (UInt 7)
