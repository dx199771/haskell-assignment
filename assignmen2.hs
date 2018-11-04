module EightOff where

    import System.Random
    import Data.List

    type Card= (Pip,Suit)
    type Deck= [Card]
    type EOBoard = (Foundations ,Columns ,Reserve)
    type Foundations  = [Card]
    type Columns  = [[Card]]
    type Reserve  = [Card]
    data Pip = Ace|Two|Three|Four|Five|Six|Seven|Eight|Nine|Ten|Jack|Queen|King
               deriving (Eq,Show,Enum)
    allP = enumFrom Ace

    data Suit = Spade|Heart|Club|Diamond
                deriving (Eq,Show,Enum)
    allS = enumFrom Spade

    pack :: Deck
    pack = [(p,s)|p<-allP,s<-allS]

    sCard :: Card-> Card
    sCard (p,s)
        |otherwise = (succ p,s)

    pCard :: Card  -> Card 
    pCard (p,s)
        |otherwise = (pred p,s)
    
    isAce :: Card  -> Bool
    isAce (p,s)
        |p==Ace = True
        |otherwise = False

    isKing :: Card  -> Bool
    isKing (p,s)
        |p==King = True
        |otherwise = False

    merge :: (a->a->Bool)->[a]->[a]->[a]
    merge compfn [] lis2 =lis2
    merge compfn lis1 [] = lis1
    merge compfn (h1:t1) (h2:t2)
        |compfn h1 h2 = (h1:merge compfn t1 (h2:t2))
        |otherwise = (h2:merge compfn (h1:t1) t2)

    mergesort :: (a->a->Bool)->[a]->[a]
    mergesort compfn [] = []
    mergesort compfn dlis = (mergesortA compfn (map(\e -> [e]) dlis))

    mergesortA _ [lis] = lis
    mergesortA compfn mlis = mergesortA compfn (mergesortpass compfn mlis)

    mergesortpass :: (a->a->Bool)->[[a]]->[[a]]
    mergesortpass _ [] = []
    mergesortpass _ [l] =[l]
    mergesortpass compfn(lis1:(lis2:rest)) = (merge compfn lis1 lis2): mergesortpass compfn rest

    shuffle ::Deck 
    shuffle = map fst (mergesort (\(_,n1)(_,n2)->n1<n2)(zip pack(take 52 (randoms (mkStdGen 42)::[Int]))))

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

    {--toFoundations::EOBoard -> EOBoard
    toFoundations (f,c,r)
         |null (reserveItemA r) = (putReserveAToFoundations (reserveItemA c) f),c,r
         |otherwise = (putReserveAToFoundations (reserveItemA r) f)
        --where ((p,s):rpl)=r

    putReserveAToFoundations::[Card]->Foundations->Foundations
    putReserveAToFoundations c@(h:t) f
        |length c ==1 = h:f
        |otherwise = h:putReserveAToFoundations t f

    --return all Ace in Reserve
    reserveItemA::[Card]->[Card]
    reserveItemA r
        |null r = []
        |p==Ace = [(p,s)]++reserveItemA rpl
        |otherwise = reserveItemA rpl
        where ((p,s):rpl)=r
--}
    toFoundations::EOBoard -> EOBoard
    toFoundations b
        |checkReserveA b==b= checkColomnsA b
        |checkColomnsA b==b= checkReserve b
		|checkReserve b==b=
    checkReserveA::EOBoard->EOBoard
    checkReserveA (f,c,(r@(hr:tr)))
        |null(filter (\n->(isAce n)) r)= (f,c,r)
        |otherwise = (f++(filter (\n->(isAce n)) r),c,(filter (\n->(isAce n==False)) r))

    checkColomnsA::EOBoard ->EOBoard
    checkColomnsA (f,(c@(hc:tc)),r)
        |null (filter (\n->(isAce n)) hc) = (f,c,r)
        |otherwise = ((f++(filter (\n->(isAce n)) allh)),removeA c,r)
        where  allh =map head c
    removeA::Columns->Columns
    removeA []=[]
    removeA (col@(hcol:tcol))
        |isAce (head hcol)= tail hcol:removeA tcol
        |otherwise = (hcol:removeA tcol)