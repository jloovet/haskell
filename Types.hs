

data MyBoolean = Yes | No 
     deriving (Show, Eq)

data MaybeInt = XEmpty | Filled Int
     deriving (Show, Eq)

data MyMaybe a = MyEmpty | MyFilled a
     deriving (Show, Eq) 


--struct
data User = User String String Int 

--egen lista
--i ghci: lst = Pointing 1 (Pointing 2 (Pointing 3 Empty) ) 
data MyList a = Empty | Pointing a (MyList a)
     deriving (Eq)
     
instance Show a => Show (MyList a) where 
	 show lst  = case lst of
	      Empty -> ""
	      Pointing x Empty -> show x
	      Pointing x y -> show x ++ "," ++ show y

 
myHead :: MyList a -> Maybe a 
myHead x = case x of
       Pointing x _ -> Just x
       Empty -> Nothing 
myTail :: MyList a -> Maybe (MyList a)
myTail x = case x of
       Pointing _ x -> Just x
       Empty -> Nothing 
myLast :: MyList a -> Maybe a
myLast x = case x of
       Pointing a Empty -> Just a
       Pointing a b -> myLast b
       Empty -> Nothing
--i ghci: myMap (\a -> a+10) lst  
myMap :: (t -> a) -> MyList t -> MyList a
myMap f lst = case lst of
      Empty -> Empty
      Pointing x y -> Pointing (f x) (myMap f y)

myFilter :: (t -> Bool) -> MyList t -> MyList t
myFilter f lst = case lst of
      Empty -> Empty
      Pointing x y -> if f x 
      	       then Pointing x (myFilter f y)
      	       else (myFilter f y) 

--create list doing eg myConvert [1,2,3]
myConvert :: [a] -> MyList a
myConvert lst = case lst of
	  [] -> Empty
      	  x:y -> Pointing x (myConvert y)
