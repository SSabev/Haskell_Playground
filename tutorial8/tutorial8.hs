-- Informatics 1 - Functional Programming 
-- Tutorial 8
--
-- Week 10 - due: 26/27 Nov.

import Data.List
import Test.QuickCheck



-- Type declarations

type FSM q = ([q], Alphabet, q, [q], [Transition q])
type Alphabet = [Char]
type Transition q = (q, Char, q)



-- Example machines

m1 :: FSM Int
m1 = ([0,1,2,3,4],
      ['a','b'],
      0,
      [4],
      [(0,'a',1), (0,'b',1), (0,'a',2), (0,'b',2),
       (1,'b',4), (2,'a',3), (2,'b',3), (3,'b',4),
       (4,'a',4), (4,'b',4)])

m2 :: FSM Char
m2 = (['A','B','C','D'],
      ['0','1'],
      'B',
      ['A','B','C'],
      [('A', '0', 'D'), ('A', '1', 'B'),
       ('B', '0', 'A'), ('B', '1', 'C'),
       ('C', '0', 'B'), ('C', '1', 'D'),
       ('D', '0', 'D'), ('D', '1', 'D')])

dm1 :: FSM [Int] 
dm1 =  ([[],[0],[1,2],[3],[3,4],[4]],
        ['a','b'],
        [0],
        [[3,4],[4]],
        [([],   'a',[]),
         ([],   'b',[]),
         ([0],  'a',[1,2]),
         ([0],  'b',[1,2]),
         ([1,2],'a',[3]),
         ([1,2],'b',[3,4]),
         ([3],  'a',[]),
         ([3],  'b',[4]),
         ([3,4],'a',[4]),
         ([3,4],'b',[4]),
         ([4],  'a',[4]),
         ([4],  'b',[4])])



-- 1.
states :: FSM q -> [q]
alph   :: FSM q -> Alphabet
start  :: FSM q -> q
final  :: FSM q -> [q]
trans  :: FSM q -> [Transition q]


states (u, a, s, f, t) = u
alph   (u, a, s, f, t) = a
start  (u, a, s, f, t) = s
final  (u, a, s, f, t) = f
trans  (u, a, s, f, t) = t


-- 2.
delta :: (Eq q) => FSM q -> q -> Char -> [q]
delta (_,_,_,_,t) a c = [final | (start, b, final) <- t , start == a, b == c] 


-- 3.
accepts :: (Eq q) => FSM q -> String -> Bool
accepts m [] = q `elem` final m
accepts m (x:xs) = or [accepts m' l xs | l<-delta m q x, let m'=(states,start m, final m, trans m)]


-- 4.
canonical :: (Ord q) => [q] -> [q]
canonical = sort.nub


-- 5.
ddelta :: (Ord q) => FSM q -> [q] -> Char -> [q]
ddelta m xs c = (canonical.concat.map (delta' m c)) xs 
                where delta' m c p = delta m p c


-- 6.
next :: (Ord q) => FSM q -> [[q]] -> [[q]]
next = undefined


-- 7.
reachable :: (Ord q) => FSM q -> [[q]] -> [[q]]
reachable = undefined


-- 8.
dfinal :: (Ord q) => FSM q -> [[q]] -> [[q]]
dfinal = undefined


-- 9.
dtrans :: (Ord q) => FSM q -> [[q]] -> [Transition [q]]
dtrans = undefined


-- 10.
deterministic :: (Ord q) => FSM q -> FSM [q]
deterministic = undefined


-- 11.
prop_deterministic :: (Ord q) => FSM q -> [Int] -> Bool
prop_deterministic = undefined

makeInput :: Alphabet -> [Int] -> String
makeInput [] _     =  error "makeInput: empty alphabet"
makeInput alph xs  =  map f xs
    where f x  =  alph !! (x `mod` length alph)


-- OPTIONAL MATERIAL ------------------------------------

data Step q = State q 
            | Symbol Char
type Trace q = [Step q]

instance (Show q) => Show (Step q) where
  show (State q)  = show q
  show (Symbol x) = [x]


-- 12.

traces :: Eq q => FSM q -> String -> [Trace q]
traces = undefined