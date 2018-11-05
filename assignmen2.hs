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
        |checkReserve(checkColomns(checkReserveA(checkColomnsA b)))/=b
        =checkReserve(checkColomns(checkReserveA(checkColomnsA b)))
        |otherwise = b

    checkReserveA::EOBoard->EOBoard
    checkReserveA (f,c,(r@(hr:tr)))
        |null(filter (\n->(isAce n)) r)= (f,c,r)
        |otherwise = (f++(filter (\n->(isAce n)) r),c,(filter (\n->(isAce n==False)) r))

    checkColomnsA::EOBoard ->EOBoard
    checkColomnsA (f,(c@(hc:tc)),r)
        |null (filter (\n->(isAce n)) allh) = (f,c,r)
        |otherwise = ((f++(filter (\n->(isAce n)) allh)),removeA c,r)
        where  allh = map head c
    removeA::Columns->Columns
    removeA []=[]
    removeA (col@(hcol:tcol))
        |isAce (head hcol)= tail hcol:removeA tcol
        |otherwise = (hcol:removeA tcol)
    
    checkReserve::EOBoard->EOBoard
    checkReserve (f,c,r@(hr:tr))
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
