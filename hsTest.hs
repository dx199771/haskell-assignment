module EightOff where

  import System.Random
  import Data.List
{- 8-Off Solitaire Part 1
  NAMING CONVENTIONS
  f,c,r Foundations,Colomns,Reserves
  p,s Pip,Suit

-}

  -- data structures defination
  type Card= (Pip,Suit)
  
  type Deck= [Card]
  
  type EOBoard = (Foundations ,Columns ,Reserves)
  
  type Foundations  = [Card]
  
  type Columns  = [[Card]]

  type Reserves  = [Card]

  data Pip = Empty|Ace|Two|Three|Four|Five|Six|Seven|Eight|Nine|Ten|Jack|Queen|King
         deriving (Eq,Ord,Show,Enum)
  allP = enumFrom Ace

  data Suit = Spade|Heart|Club|Diamond
        deriving (Eq,Ord,Show,Enum)
  allS = enumFrom Spade
  
  --pack
  --no arguements, returns a list of all the cards
  --all the cards mean a list of 52 cards
  pack :: Deck
  pack = [(p,s)|p<-allP,s<-allS] --list comprehensions

  --sCard
  --takes a Card, returns the successor card
  --
  sCard :: Card-> Card
  sCard (p,s)
    |otherwise = (succ p,s)

  --pCard
  --takes a Card, returns the predecessor card
  --
  pCard :: Card  -> Card 
  pCard (p,s)
    |otherwise = (pred p,s)
  
  --isAce
  --takes a Card, returns true if the given card is Ace
  --
  isAce :: Card  -> Bool
  isAce (p,s)
    |p==Ace = True
    |otherwise = False

  --isKing
  --takes a Card, returns true if the given card is King
  --
  isKing :: Card  -> Bool
  isKing (p,s)
    |p==King = True
    |otherwise = False

  --shuffle
  --take an int, return a shuffled deck
  --int as a seed
  shuffle ::Int->Deck
  shuffle s= map fst (mergesort (\(_,n1)(_,n2)->n1<n2)(zip pack(take 52 (randoms (mkStdGen s)::[Int]))))

  eODeal :: Deck ->EOBoard 
  eODeal a = ([(Empty,Spade),(Empty,Heart),(Empty,Club),(Empty,Diamond)],
    [(take 6 a),
    (take 6 (drop 6 a)),
    (take 6 (drop 12 a)),
    (take 6 (drop 18 a)),
    (take 6 (drop 24 a)),
    (take 6 (drop 30 a)),
    (take 6 (drop 36 a)),
    (take 6 (drop 42 a))],
    take 4 (drop 48 a))

  toFoundations::EOBoard -> EOBoard
  toFoundations b
    |checkReserve b/=b
    =toFoundations(checkReserve b)
    |otherwise = b

  checkReserve::EOBoard->EOBoard
  checkReserve (f,c,r)
    |otherwise=
    (map (\n -> (updateFoundation n f)) allf++allc,
    map (\x -> filter(\n -> (elem n common) == False) x ) c,
    (filter(\n -> (elem n allf) == False) r))
    where allf = map sCard f
          allc = map head c
          common = filter(\n -> (elem n allf)) allc
          common2 = filter(\n -> (elem n allf)) r 

  updateFoundation::Card->Foundations->Foundations
  updateFoundation [] _ = []
  updateFoundation _ [] = []
  updateFoundation c f@(fh:ft)
    |bgpCard c == fh = c:ft
    |otherwise = fh:updateFoundation c ft

  --merge sort code from MOLE given by Phil
  --merge
  
  merge :: Ord a=> (a->a -> Bool)->[a]->[a] -> [a]
  
  merge _ [] lis2 = lis2
  
  merge _ lis1 [] = lis1
  merge compfn lis1 lis2 
    | compfn h1 h2 = (h1:merge compfn t1 lis2)
    | otherwise = (h2:merge compfn lis1 t2)
    where 
      (h1:t1)=lis1
      (h2:t2)=lis2
 
  -------------------------------------------------
  --mergesort
  mergesort :: Ord a=> (a->a -> Bool)->[a] -> [a]
 
  mergesort _ [] = [] --check this once only
  mergesort compfn dlis = 
        mergesortA compfn (map (\ e -> [e]) dlis) -- give aux fn list of lists length 1
  -------------------------------------------------
  --mergsortA
  mergesortA :: Ord a=> (a->a -> Bool)->[[a]] -> [a]

  mergesortA _ [lis] = lis -- one list only, it's the answer
  -- general case - merge list pairs & repeat
  mergesortA compfn mlis= mergesortA compfn (mergesortpass compfn mlis)
  ---------------------------------------------------------------------
  --mergesortpass
  -- merge pairs of lists 
  mergesortpass :: Ord a=> (a->a -> Bool)->[[a]] -> [[a]]

  mergesortpass _ [] = [] 
  mergesortpass _ [l]= [l] -- one element only, return list unchanged

  -- general case - merge first two lists, cons to remainder

  mergesortpass compfn (lis1:(lis2:rest)) =(merge compfn lis1 lis2): mergesortpass compfn rest
  
