-- ASSIGNMENT 2 COMP 3007 FALL 2015
--
-- DUE: 23:55 Wed Sep 16 2015
--
-- WHAT TO SUBMIT. Submit a single Haskell file (with an extension
-- ".hs").  If a question asks for text, put it in a Haskell comment.
-- Your file needs to be loadable as a Haskell file.

-- REQUIRED BACKGROUND. 

-- COLLABORATION POLICY. Collaborating on assignments is STRICTLY
-- DISALLOWED. You must complete the work by yourself. If you need
-- help, please see a TA or your instructor. Posting assignment
-- solutions on discussion boards before the due date and time is also
-- prohibited.
--
-- READINGS. Lecture notes from Lecture 2 Sep 9; Learn You a Haskell
-- chapter 6 except for the subsection "Only folds and horses".
--
-- This assignment is some small exercises on programming with lists
-- and higher-order functions.  It will also introduce pattern
-- matching.
--
-- Pattern matching is an intuitive way of avoiding use of
-- if-then-else when making decisions based on the structure of a
-- value.  Here's an example from class.

incrementElements l =
  if l==[]
  then []
  else head l + 1 : incrementElements (tail l)

-- Here we're doing a case analysis on whether l is [] or is a "cons",
-- i.e. of the form e:e'.  Instead of a single definition with an
-- if-then-else, we can give a two-part definition, one part for each
-- case.

incrementElements2 [] = []
incrementElements2 (x : l) = x + 1 : incrementElements2 l

-- Pattern matching works in order of the parts of the definition.
-- Consider the following.

f (x, "bar") = 1
f ("foo", y) = 2
f (x, y) = 3

-- All three patterns match ("foo", "bar"), but f ("foo", "bar") = 1
-- because the first matching pattern is used.  The third line is a
-- "catch-all" that applies whenever the previous two lines don't.
--
-- Patterns can also be nested.  E.g.

g [] = []
g ( (x,y) : l ) = (y,x) : g l

-- You should use pattern matching in most of the problems below.


--
-- THE ASSIGNMENT
--
-- Complete the definitions below.  For each function, the type is
-- already provided.  Do not change it.  There are 10 questions, each
-- 10 points. Functions are currently defined using "stub" (see
-- below) so that the file loads in ghci, e.g. Question 1 has
--   duple = stub
-- You don't have to define it this way, e.g. your definition can use
-- parameters and have multiple parts, e.g.
--   duple 0 x = ...
--   duple n x = ...


stub :: a
stub = error "Not implemented!"

-- QUESTION 1
--
-- duple n x: create a list of n copies of x
-- E.g. duple 3 1 = [1,1,1]

duple :: Int -> a -> [a]
duple n x = replicate n x
	

-- QUESTION 2
--
-- invert l: reverse all the two-element lists in l, leaving other
-- lists in l unchanged.
-- Example: invert [[1,2],[3,4,5],[6,7]] = [[2,1],[3,4,5],[7,6]]

invert :: (Eq a) =>[[a]] -> [[a]]
invert l = if l==[]
		then []
		else if length(head l)==2 
			then reverse(head l) : invert (tail l)
		    else head l : invert (tail l)

-- QUESTION 3
--
-- triplize l: make 2 additional copies of each list element.
-- Example: triplize [1,2,3] = [1,1,1,2,2,2,3,3,3]
triplize :: (Eq a) =>[a] -> [a]
triplize l = if l==[]
		then []
		else duple 3 (head l)  ++ triplize(tail l)

-- QUESTION 4
--
-- listSet l n x: replace the n-th member of l by x; assume n >= 1.
-- Example: listSet ["a", "b", "c", "d"] 2 "e" = ["a","e","c","d"]
listSet :: [a] -> Integer -> a -> [a]
listSet l n x = if n<1
		then l
		else if n==1
			then x:tail l
			else (head l) : (listSet (tail l) (n-1) x)




-- QUESTION 5
--
-- product xs ys: all pairs consisting of an element of xs followed by
-- an element of ys.
-- Example: prod [1,2,1] [3,4] = [(1,3),(1,4),(2,3),(1,3),(2,4),(1,4)]
-- The order of the pairs in the list doesn't matter.  You might want
-- to use ++, which appends two lists, e.g. [1,2]++[3,4] = [1,2,3,4]
prod :: [a] -> [b] -> [(a,b)]
prod xs ys = [(a,b)|a<-xs,b<-ys]

-- QUESTION 6
--
-- down xs: make a singleton list of each member of xs
-- Example: [1,2,3] -> [[1],[2],[3]]
down :: [a] -> [[a]]
down xs =[[x]|x<-xs]

-- QUESTION 7
--
-- union ls: the concatenation of all the lists in l
-- Example: union [[1,2,3], [4,5], [6]] = [1,2,3,4,5,6]

union [] = []
union ls = if ls==[]
	then []
	else head ls ++ union (tail ls)

-- QUESTION 8
--
-- identity x = x.  Your definition must have the form identity = e
-- where e mentions *only* ap and proj and parentheses.
ap f g x = f x (g x)
proj x y = x
identity :: a -> a
identity x = ap (proj) (proj) x;

-- QUESTIONS 9 & 10
--
-- Below are the definitions of map, filter and all, three functions
-- that are widely used in functional programming.  Also below is a
-- special case of the "elem" function, and some other useful
-- functions.
--
-- map :: (a -> b) -> [a] -> [b]
-- map _ [] = []
-- map f (x:xs) = f x : map f xs
--
-- filter :: (a -> Bool) -> [a] -> [a]
-- filter pred []    = []
-- filter pred (x:xs) =
--   if pred x
--     then x : filter pred xs
--     else filter pred xs
--
-- all :: (a -> Bool) -> [a] -> Bool
-- all _ [] =  True
-- all p (x:xs) =  p x && all p xs
--
-- flip :: (a -> b -> c) -> b -> a -> c
-- flip f x y =  f y x
--
charIn ::  Char -> String -> Bool
charIn _ []       = False
charIn x (y:ys)   = x==y || charIn x ys
--
-- You are to write two functions.  Neither can directly use
-- recursion, i.e. the right-hand-sides of your definitions can only
-- use previously defined functions.  You should instead make use of
-- the above functions.  Also, don't use other string or list
-- functions from the Haskell standard library.  The point of this
-- exercise is to learn about using the above functions.
--
-- One more note.  Any infix function, like +, can be written as a
-- prefix function using parens; any prefix function can be used infix
-- using backticks.
-- Example: (+) x y = x + y, and x `elem` y = elem x y

-- isWord s: True if and only if s all the characters in s are
-- lower-case letters
-- Example: isWord "foo" = True; isWord "fo3o" = False.
isWord :: String -> Bool
isWord ys = all (flip (charIn) (['a'..'z'])) ys

-- prefixWords str strs: add str as a prefix to each word in strs,
-- deleting non-words.
-- Example:
-- prefixWords "word:" ["aa", "a3", "foo"]
-- = ["word:aa", "word:foo"]
prefixWords :: String -> [String] -> [String]
prefixWords str strs=  filter(isWord) (map(str++) strs)


