module EightOff where

  import System.Random
  import Data.List
{- 8-Off Solitaire Part 1
  NAMING CONVENTIONS
  f,c,r Foundations,Colomns,Reserves


-}

  -- data structures defination
  type Card= (Pip,Suit)
  
  type Deck= [Card]
  
  type EOBoard = (Foundations ,Columns ,Reserves)
  
  type Foundations  = [Card]
  
  type Columns  = [[Card]]

  type Reserves  = [Card]

  data Pip = Ace|Two|Three|Four|Five|Six|Seven|Eight|Nine|Ten|Jack|Queen|King
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
  eODeal a = ([],[(take 6 a),
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
    |checkReserve(checkColomns(checkA b))/=b
    =toFoundations(checkReserve(checkColomns(checkA b)))
    |otherwise = b

  checkA::EOBoard->EOBoard
  checkA (f,c@(hc:tc),(r@(hr:tr)))
    |null(filter isAce r++filter isAce allh)= (f,c,r)
    |otherwise = (f++filter isAce r++filter isAce allh,
                 removeA c,
                 filter (\n->(isAce n==False)) r)
                 where  allh = map head c

  removeA::Columns->Columns
  removeA []=[]
  removeA (col@(hcol:tcol))
    |isAce (head hcol)= tail hcol:removeA tcol
    |otherwise = (hcol:removeA tcol)
  
  checkReserve::EOBoard->EOBoard
  checkReserve (f,c@(hc:tc),r@(hr:tr))
    |null tr = (f,c,r)
    |elem hr allf = checkReserve(insertCard hr f,c,tr)
    |elem hr allf == False = addN hr (checkReserve(f,c,tr))
    |otherwise = (f,c,r)
    where  allf = map sCard f

  addN :: Card->EOBoard->EOBoard
  addN card (f,c,r)=(f,c,card:r)
  checkColomns::EOBoard->EOBoard
  checkColomns (f,c@(hc:tc),r)
    |null tc = (f,c,r)
    |elem (head hc) allf = checkColomns(insertCard (head hc) f,(tail hc):tc,r)
    |elem (head hc) allf == False = addC hc (checkColomns(f,tc,r))
    |otherwise=(f,c,r)
    where  allf = map sCard f
  addC :: [Card]->EOBoard->EOBoard
  addC card (f,c,r) = (f,card:c,r)
  insertCard::Card->Foundations->Foundations
  insertCard c f1@(hf:tf)
    |pCard c ==hf = c:tf
    |otherwise = hf: insertCard c tf



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
  