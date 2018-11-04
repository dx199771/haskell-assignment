module Bags where
    --polymorphic type for a Bag
    type Quantitiy = Int
    type Bag a= [(a, Quantitiy)]

    --listToBag
    listToBag::Eq a=>[a]->Bag a
    listToBag []=[]
    listToBag (h:t)
      |otherwise= bagInsert h (listToBag t) --insert each item using bagInsert

    --bagEqual  
    bagEqual::Eq a =>Bag a->Bag a->Bool
    bagEqual b1 b2 
      |length b1 /= length b2 = False --if length of each Bag not equal then false
      |otherwise = bagEqualA b1 b2 

    bagEqualA::Eq a =>Bag a->Bag a->Bool 
    bagEqualA []_ = True --compare until the last item 
    bagEqualA (h:t) b2
      |elem h b2= bagEqualA t b2 --if bag2 contain first item in bag1 then do the rest list
      |otherwise = False

    --bagInsert
    bagInsert::Eq a =>a->Bag a ->Bag a
    bagInsert a b       
      |null b = [(a,1)] --insert to an empty list
      |a==q = (a,v+1):rpl --found the item, quantity plus one
      |otherwise = ((q,v):bagInsert a rpl) --find the rest list
      where ((q,v):rpl)=b

    --bagSum
    bagSum::Eq a =>Bag a ->Bag a ->Bag a
    bagSum b1 b2
      |null b1 = b2
      --insert first item in bag1 to bag2 and remove it from bag2 if exist(replace)
      |otherwise = tupleInsert(q,v) b2: bagSum rpl (tupleRemove q b2)
      where ((q,v):rpl)=b1
            ((q1,v1):rpl2)=b2

    tupleInsert::Eq a =>(a, Quantitiy)->Bag a->(a,Quantitiy)
    tupleInsert (h1,t1) b1
      |null b1 = (h1,t1) 
      |h1==q = (h1,v+t1) --found the item,sum quantity
      |h1/=q = tupleInsert(h1,t1) rpl
      where ((q,v):rpl)=b1
 
    tupleRemove::Eq a =>a->Bag a->Bag a
    tupleRemove a b
      |null b = []
      |a==q = rpl --found the item, answer is the rest
      |otherwise = (q,v):tupleRemove a rpl
      where ((q,v):rpl)=b

    --bagIntersection 
    bagIntersection ::Eq a =>Bag a->Bag a->Bag a
    bagIntersection b1 b2
      |null b1 = []
      |null b2 = []
      --if bag2 contains the first item in bag1
      |otherwise = bagContain (q,v) b2 ++bagIntersection rpl b2 
      where ((q,v):rpl)=b1
  
    bagContain :: Eq a =>(a, Quantitiy)->Bag a->Bag a
    bagContain (h1,t1) b2
      |null b2 = [] --not contain, return empty list
      |h1==q = [(h1,compareItem t1 v)] --fount the item,compare the quantity
      |h1/=q = bagContain (h1,t1) rpl
      where ((q,v):rpl)=b2
 
    compareItem::Quantitiy->Quantitiy->Quantitiy
    compareItem i1 i2
      |i1<i2=i1 --return smaller quantity
      |otherwise = i2
