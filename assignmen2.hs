
module EightOff where

  import System.Random
  import Data.List
{- 8-Off Solitaire Part 1
  NAMING CONVENTIONS
  f,c,r : Foundations,Colomns,Reserves
  b : eOBoard
  p,s : Pip,Suit
-}

  -- data structures defination
  type Card= (Pip,Suit)
  
  type Deck= [Card]
  
  type EOBoard = (Foundations ,Columns ,Reserves)
  
  type Foundations  = [Card] --just need to know head of foundation,do not need [[card]]
  
  type Columns  = [[Card]]

  type Reserves  = [Card]

  data Pip = Ace|Two|Three|Four|Five|Six|Seven|Eight|Nine|Ten|Jack|Queen|King
         deriving (Eq,Ord,Show,Enum)
  allP = [(Ace)..(King)] --all data in pip

  data Suit = Spade|Heart|Club|Diamond
        deriving (Eq,Ord,Show,Enum)
  allS = [(Spade)..(Diamond)] --all data in suit
  
  --pack
  --no arguements, returns a list of all the cards
  --all the cards mean a list of 52 cards
  pack :: Deck
  pack = [(p,s)|p<-allP,s<-allS] --list comprehensions

  --sCard
  --takes a Card, returns the successor card
  --return error if given card is King
  sCard :: Card-> Card
  sCard (p,s)
    |p==King = error "King has no successor"
    |otherwise = (succ p,s)

  --pCard
  --takes a Card, returns the predecessor card
  --return error if given card is Ace
  pCard :: Card  -> Card 
  pCard (p,s)
    |p==Ace = error "Ace has no predecessor"
    |otherwise = (pred p,s)
  
  --isAce
  --takes a Card, returns true if the given card is Ace
  isAce :: Card  -> Bool
  isAce (p,s)
    |p==Ace = True
    |otherwise = False

  --isKing
  --takes a Card, returns true if the given card is King
  isKing :: Card  -> Bool
  isKing (p,s)
    |p==King = True
    |otherwise = False

  --shuffle
  --take an int, return a shuffled deck
  --int as a seed (manully input)
  shuffle :: Int -> Deck
  shuffle seed = 
    let
      rlis = take 52 (randoms (mkStdGen seed)::[Int]) --zip with pack
      zlis = zip pack rlis --producing card pairs
      slis = mergesort (\(_,n1)(_,n2)->n1<n2) zlis --sort list
      cypher = map fst slis --extract card in random order
    in
      cypher --return shuffled cards

  --eODeal
  --take an deck of card, return it as a Board
  eODeal :: Deck ->EOBoard 
  eODeal a = ([],[(take 6 a), --take 6 cards from 52 cards
    (take 6 (drop 6 a)),      --drop 6 cards which just took, take 6 again in 48 cards
    (take 6 (drop 12 a)),     --repeat
    (take 6 (drop 18 a)),
    (take 6 (drop 24 a)),
    (take 6 (drop 30 a)),
    (take 6 (drop 36 a)),
    (take 6 (drop 42 a))],
    take 4 (drop 48 a))

  -------------------------------------------------
  --toFoundations (checkReserveAce)
  --take a Board and return a Board which obtained by making all the possible moves to the Foundations
  --only run once at the start of game as there can't be any aces in reserve after it
  toFoundations::EOBoard -> EOBoard
  toFoundations b@(f,c,r)
    |null (filter isAce r) = toFoundationsA b --no aces in reserve
    |otherwise = toFoundationsA(filter isAce r,c,filter (not.isAce) r) --remove aces in reserve, add to foundation
  
  --toFoundationsA
  --take a Board without Ace in resreves and return a Board which obtained by making all the possible moves to the Foundations
  toFoundationsA::EOBoard->EOBoard
  toFoundationsA b
    |checkColRes(checkColomnsAce b)/=b  --chcek until no moveable cards
    = toFoundations(checkColRes(checkColomnsAce b))
    |otherwise = b --otherwise, no possible move, return board

  --checkColomnsAce
  --take a board, return a board which put all aces in colomns to foundations
  checkColomnsAce::EOBoard -> EOBoard
  checkColomnsAce b@(f,c,r)
    |null colomnsAce = b --no moveable aces 
    |otherwise = (colomnsAce++f, --add moveable aces 
                 map tail (filter (isAce.head) nonEmpty) ++ filter (not.isAce.head) nonEmpty , --return tail of list wihch start of Ace and list which not start of Ace
                 r)
    where colomnsAce = filter isAce (map head nonEmpty) --all head aces in colomns
          nonEmpty = (filter (not.null) c) --filter may return empty list, this is remove empty lists
  
  --checkColRes
  --take a board and return a board which has moved all possible cards to foundations
  --by taking all moveable cards in reserve and colomns, take out them and add them in foundations
  checkColRes::EOBoard->EOBoard
  checkColRes b@(f,c,r) = 
    let
      f1 = map sCard (filter(\n -> elem n (map pCard (moveableCol++moveableRes))) f) ++ filter(\n -> notElem  n (map pCard (moveableCol++moveableRes))) f --updated foundation
      c1 = map (\n ->filter (\m -> notElem  m moveableCol) n) nonEmpty --takes out moveable cards (filter out cards can make possible move)
      r1 = filter (\n -> notElem  n moveableRes) r --takes out moveable cards
    in
      (f1,c1,r1)
    where succFoundations = map sCard (filter (not.isKing) f) --return all successors of Foundations EXCEPT KING
          --compare each card in reserves and head colomns. take cards which are successor of foundations
          moveableCol = [n|n<- map head nonEmpty,elem n succFoundations] --moveable cards in colomns
          moveableRes = [n|n<- r, elem n succFoundations]                --moveable cards in reserve
          nonEmpty = (filter (not.null) c) --filter may return empty list, this is remove empty lists

  -------------------------------------------------
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
  