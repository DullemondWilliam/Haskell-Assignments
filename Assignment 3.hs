-- ASSIGNMENT 3 COMP 3007 FALL 2015
--
-- DUE: 23:55 Wed Sep 23 2015
--
-- WHAT TO SUBMIT. Submit this Haskell file with the questions
-- completed.

-- COLLABORATION POLICY. Collaborating on assignments is STRICTLY
-- DISALLOWED. You must complete the work by yourself. If you need
-- help, please see a TA or your instructor. Posting assignment
-- solutions on discussion boards before the due date and time is also
-- prohibited.
--
-- READINGS. Learn You a Haskell chapters 4 and 6 (including the folds
-- section).
--
-- HASKELL STANDARD LIBRARY.  This is huge.  We will only need a
-- fraction of it in the course.  Mainly, we will just use the
-- Prelude: http://bit.ly/1Lz6f2o.  You should look the Prelude over.
--
-- WHAT YOU CAN USE.  In this assignment, you can use anything from
-- the lectures, the given Haskell file, and anything in the Prelude.
-- Nothing more though.

--
-- The assignment is build a small bit of inductive-definition
-- machinery.
--
-- Two examples should adequately illustrate the main idea.
--
-- A string of balanced parens has a corresponding right paren for
-- every left paren.  E.g. "()(()())" is balanced, whereas "()(()" is
-- not.  The set BP of balenced-paren strings can be inductively
-- generated as follows.
--   - () is in BP
--   - if s and t are in BP, so is the concatenation st of s and t
--   - if s is in BP then so is (s)
--
-- A set A of strings representing simple arithmetic expressions can
-- also be defined inductively.
--   - x, y and z are in A
--   - if e is in A then so is (e)
--   - if e and e' are in A, then so are e1+e2 and e1*e2
-- (Sorry about the object-meta level confusion: x, y and z are the
-- string "x" "y" and "z"; e and e' range over all strings.)
--
-- We can formalize definitions like this in Haskell.  Each of the
-- clauses in the above two definitions is really just a simple
-- template where slots get filled in with values already generated.
-- Write {} for the slots.
--
-- The templates for BP:
--   - ()
--   - {}{}
--   - ({})
--
-- For A:
--   - three templates: x y z
--   - ({})
--   - {}+{} {}*{}
--
-- So, we can derive that (x+y)*z is in A:
--   - the strings x, y and z are immediately in A since their
--   templates have no slots needing filling-in
--   - putting x in for the first {} in {}+{}, and y in for the
--   second {}, we've derived that x+y is in A
--   - the template ({}) and the fact that we know x+y is in A gives
--   (x+y) in A
--   - finally, the {}*{} template gives (x+y)*z in A.
--
-- The code below formalizes the notions of templates and derivations.
-- Your job is to complete the missing definitions.  There are 4
-- questions, each worth 25 points.


----------------------------------------------------------------------


-- Yes, it says above to stick to the Prelude, but this one function
-- (intercalate) is a favorite of the instructor.
import Data.List (intercalate)

data Slot = Slot
          deriving (Show, Eq)

data Template =
  Template [Either String Slot]
  deriving Eq

-- explicitly define show for templates so that they print like in the
-- discussion above
instance Show Template where
  show (Template xs) =   concat $ map f xs
    where f (Left x) = x
          f (Right x) = "{}"

-- An inductive definition is just a list of templates.
data Def =
  Def [Template]
  deriving Eq

-- A definition is displayed as a sequence of rules separated by "//".
instance Show Def where
  show (Def rs) = "Templates: " ++ (intercalate " // " $ map show rs)

-- QUESTION 1.
-- The number of slots in a template.
-- Example: templateArity ({})foo{}{} = 3
templateArity :: Template -> Int
templateArity (Template xs) = sum $ map (\x -> either (\_ -> 0) (\_ -> 1) x) xs


--
-- Balenced paren example
--

-- templates from the discussion above coded in Haskell
bpBase = Template [Left "()"]
bpStep1 = Template [Left "(", Right Slot, Left ")"]
bpStep2 = Template [Right Slot, Right Slot]
bp = Def [bpBase, bpStep1, bpStep2]

-- show bp =
-- "Templates: () // ({}) // {}{}"



--
-- Arithmetic expressions example
--

arithX = Template [Left "x"]
arithY = Template [Left "y"]
arithZ = Template [Left "z"]
arithPlus = Template [Right Slot, Left "+", Right Slot]
arithTimes = Template [Right Slot, Left "*", Right Slot]
arithParens = Template [Left "(", Right Slot, Left ")"]
arith = Def [arithX, arithY, arithZ, arithPlus, arithTimes, arithParens]

-- show arith =
-- "Templates: x // y // z // {}+{} // {}*{} // ({})"



subst :: [String] -> [Either String Slot] -> [Either String Slot]
subst [] xs = xs
subst (str : strs) (Right _ : xs) = Left str : subst strs xs
subst (str : strs) (x : xs) = x : subst (str : strs) xs


-- Question 2
-- Replace the slots with the given strings.
-- Example: templateInstance {}foo{}{} ["1", "2", "3"] = 1foo23
-- Note: you can use the show function to turn objects into strings.
templateInstance :: Template -> [String] ->  String
templateInstance (Template xs) strs =  show(Template (subst(strs) (xs)))


-- A derivation is a tree of templates.
-- A derivation (Derivation t ds) is *valid* if the derivations in ds
-- are valid and if the arity of t is the same as the length of ds.
-- The *result* of a derivation (Derivation t ds) is the instance of
-- the template t obtained by replacing the slots in t with the
-- results of the derivations in ds.
data Derivation = Derivation Template [Derivation]
                deriving Show


-- Question 3
-- Analogous to the foldr function defined in class for binary trees.
-- Example: derivationFoldR (\t vs -> maximum $ (templateArity t : vs))
-- is a function that computes the maximum template arity in a
-- derivation.
--derivationFoldR :: (Template -> [a] -> a) -> Derivation -> a
derivationFoldR f (Derivation r l) =  if (length l == 0)
                                        then 0
                                        else
                                          if(f(r)>maximum (map (derivationFoldR f) l))
                                            then f(r)
                                            else maximum(map (derivationFoldR f) l)

-- Question 4
-- derivationResult d returns Nothing if d is not valid; otherwise it
-- returns Just s where s is the *result* of d (see the comment before
-- the definition of Derivation
--derivationResult :: Derivation -> Maybe String
derivationResult (Derivation r l) =  if (length l == 0)
                                      then show r
                                      else (show(map derivationResult l)) ++(show(show r))





                       -- else if(f( r ) == f(map (derivationResult ) l))
                         -- then templateInstance( r ) ( l )
       --                   else  -- can add argument if you like
 -- stub


bp1 = Derivation bpBase []          -- ()
bp2 = Derivation bpStep1 [bp1]      -- (())
bp3 = Derivation bpStep2 [bp1, bp2] -- ()(())
bp4 = Derivation bpStep1 [bp3]      -- (()(()))
bp5 = Derivation bpStep1 [bp3, bp3] -- invalid
bp6 = Derivation bpStep2 [bp4]      -- invalid

ax = Derivation arithX []            -- x
ay = Derivation arithY []            -- y
az = Derivation arithZ []            -- z
a2 = Derivation arithPlus [ax, ay]   -- x+y
a3 = Derivation arithParens [a2]     -- (x+y)
a4 = Derivation arithPlus [ay, az]   -- y+z
a5 = Derivation arithParens [a4]     -- (y+z)
a6 = Derivation arithTimes [a3, a5]  -- (x+y)*(y+z)
a7 = Derivation arithParens [a3, a5] -- invalid
a8 = Derivation arithTimes [a7, a7]  -- invalid

stub :: a
stub = error "Not implemented"
-- The templates for BP:
--   - ()
--   - {}{}
--   - ({})
--
-- For A:
--   - three templates: x y z
--   - ({})
--   - {}+{} {}*{}