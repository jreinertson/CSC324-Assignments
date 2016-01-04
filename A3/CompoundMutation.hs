{- Assignment 3 - Memory and Mutation

This file contains the code responsible for working with association lists,
which you will use as the data structure for storing "mutable" data.
-}

-- **YOU MUST ADD ALL FUNCTIONS AND TYPES TO THIS LIST AS YOU CREATE THEM!!**
module CompoundMutation (
    Mutable, get, set, def,
	Value(..),
    Memory, Pointer(..), (>>>), (>~>), StateOp(..), runOp, returnVal, alloc, free
    )
    where

import AList (AList, lookupA, insertA, updateA)

-- A type representing the possible values stored in memory.
data Value = IntVal Integer |
             BoolVal Bool
             deriving Show
-- A type representing a container for stored "mutable" values.
type Memory = AList Integer Value

-- A type representing a pointer to a location in memory.
data Pointer a = P Integer | PP Integer Integer --Added a second constructor to pointer

data StateOp a = StateOp (Memory -> (a, Memory))

-- A type representing a person with two attributes:
-- age and whether they are a student or not.
data Person = Person Integer Bool deriving Show

runOp :: StateOp a -> Memory -> (a, Memory)
runOp (StateOp op) mem = op mem

returnVal :: a -> StateOp a
returnVal a = StateOp (\m -> (a, m))

-- Type class representing a type which can be stored in "Memory".
class Mutable a where
    -- Look up a value in memory referred to by a pointer.
	get :: Pointer a -> StateOp a
    -- Change a value in memory referred to by a pointer.
    -- Return the new memory after the update.
        set :: Pointer a -> a -> StateOp (Pointer a)
    -- Create a new memory location storing a value, returning a new pointer
    -- and the new memory with the new value.
    -- Raise an error if the input Integer is already storing a value.
	def :: Integer -> a -> StateOp (Pointer a)

--THESE FUNCTIONS ARE MODIFIED VERSIONS OF lookupA/insertA/updateA THAT HAVE ERROR CHECKING THAT THE ORIGINALS WERE NOT SPECIFIED TO HAVE
lookupMem :: Eq a => AList a b -> a -> b
lookupMem [] key = error "Key does not exist"
lookupMem alist key = let (a, b) = head alist	
						in if (a == key) then b else lookupMem (tail alist) key	
insertMem :: Eq a => AList a b -> (a, b) -> AList a b
insertMem [] (key, val) = [(key, val)]
insertMem alist (key, val) = let (a, b) = head alist
							 in if (a==key) then error "Key already Exists" else (a,b):(insertMem (tail alist) (key,val))
updateMem :: Eq a => AList a b -> (a, b) -> AList a b
updateMem [] (key, val) = error "key does not exist"
updateMem [(a,b)] (key, val) = if (a==key) then [(a,val)] else [(a,b)]
updateMem alist (key, val) = let (a, b) = head alist
							in if (a == key) then (a, val):(tail alist) else (a,b):(updateMem (tail alist) (key, val))

--THESE FUNCTIONS ARE THE ORIGINAL get/set/def BEFORE STACKOPS WERE IMPLEMENTED, THEY'RE STILL USED BUT ARE NOW WRAPPED IN get/set/def
getInt mem p = let P x = p 
               in let v = lookupMem mem x 
                  in case v of BoolVal b -> error "value is bool, expected integer"
                               IntVal b -> b
getBool mem p = let P x = p
                in let v = lookupMem mem x 
                   in case v of IntVal b -> error "value is integer, expected bool"
                                BoolVal b -> b
setInt mem p a = let P x = p
                 in let v = lookupMem mem x 
                    in case v of BoolVal b -> error "value is bool, expected integer"
                                 IntVal b -> updateMem mem (x,a)
setBool mem p a = let P x = p
                  in let v = lookupMem mem x 
                     in case v of IntVal b -> error "value is integer, expected bool"
                                  BoolVal b -> updateMem mem (x,a)
defInt mem i a = (P i, insertMem mem (i, IntVal a)) 
defBool mem i a = (P i, insertMem mem (i, BoolVal a))
--END OF ORIGINAL get/set/def

getNum p = let P i = p  --Extracts an integer value from a pointer
	       in i

(>>>) :: StateOp a -> StateOp b -> StateOp b
op1 >>> op2  = StateOp (\m ->
				let (_, m1) = runOp op1 m
				in runOp op2 m1)
(>~>) :: StateOp a -> (a -> StateOp b) -> StateOp b
f >~> g = StateOp (\mem ->
				let (op, mem1) = runOp f mem
				in let newStateOp = g op
				      in runOp newStateOp mem1)
instance Mutable Integer where
		get p = StateOp (\m -> ((getInt m p),  m))
		set p a = StateOp (\mem -> (p, (setInt mem p (IntVal a))))
		def i a = StateOp (\m ->  defInt m i a)
	
instance Mutable Bool where
		get p = StateOp (\m -> ((getBool m p),  m))
		set p a = StateOp (\mem -> (p, (setBool mem p (BoolVal a))))
		def i a = StateOp (\m ->  defBool m i a)
		
newAddr [] = 0
newAddr [(i, _)] = if (i == 0) then 1 else (i+1)*i --helper function that sums up all used memory addresses, to create a new, unused address
newAddr (x:xs) = (newAddr [x]) + (newAddr xs)		
		
alloc :: Mutable a => a -> StateOp (Pointer a)
alloc a = StateOp(\m -> runOp (def (newAddr m) a) m) --Runs def with a freshly generated address 	
	
free :: Mutable a => Pointer a -> StateOp ()
free p = StateOp (\m -> ((), freeHelper p m)) --Removes any data that matches the given pointer

freeHelper p [(i, v)] = let P m = p --helper that goes through the list, removing any element that has a value matching the pointer
                        in if (m == i) then [] else [(i,v)]
freeHelper p mem = (freeHelper p [(head mem)]) ++ freeHelper p (tail mem)

instance Mutable Person where
		get p = let PP a s = p
			    in get (P a) >~> \age ->
                   get (P s) >~> \student->
                   returnVal (Person age student)				   
		set pp new = let PP a s = pp
		             in let Person age student = new
					    in set (P a) age >>>
						   set (P s) student >>>
						   returnVal(pp)
		def i p = let Person a b = p
				  in def i a >~> \p1 -> --assumes the given i is an empty address
				     alloc b >~> \p2 -> --then allocs the isStudent since i+1 may not be free
					 returnVal (PP (getNum p1) (getNum p2))

--THESE ARE THE @@, age, AND isStudent METHODS FOR PART 5
(@@) :: Pointer Person -> ( Pointer Person ->  Pointer a) -> Pointer a --Applies Age or isStudent to extract a pointer from PersonPointer
p @@ x = x p

isStudent ::  Pointer Person -> Pointer Bool --Extracts isStudent pointer from a personpointer
isStudent p = let PP _ b =  p 
			  in P b

age ::  Pointer Person -> Pointer Integer --Extracts age pointer from a personpointer
age p = let PP i _ = p 
	    in P i
--------------PART 5 methods ENDS HERE


personTest :: Person -> Integer -> StateOp (Integer, Bool, Person)
personTest person x =
    -- not using alloc, but we could
    def 1 person >~> \personPointer ->
    get (personPointer @@ age) >~> \oldAge ->
    set (personPointer @@ age) x >>>
    get (personPointer @@ isStudent) >~> \stu ->
    get (personPointer @@ age) >~> \newAge ->
    set personPointer (Person (2 * newAge) (not stu)) >>>
    get personPointer >~> \newPerson ->
    get (personPointer @@ isStudent) >~> \newStu ->
    returnVal (oldAge, newStu, newPerson)

	