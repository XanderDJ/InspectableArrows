{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}

module Game.MTL where

{-

    Bouw verder op example 3 waarbij if else word toegevoegd (voor interface case of te bereiken)

-}

import Control.Arrow
import Data.Functor.Const
import qualified Data.Set as S
import Lens.Micro
import PaSe

class Arrow a => Split a where
  split :: Bool -> (b -> c) -> (b -> c) -> a (b -> c) (b -> c)

class Indexable a where
  index :: a -> Int

class Arrow a => CaseIndex a where
  caseIndex :: Indexable i => i -> [a b c] -> a b c

instance CaseIndex (->) where
  caseIndex caseEvent caseBranches = caseBranch
    where
      indexOf = index caseEvent
      -- Not enough branches supplied could be captured in a Maybe Type. For now it throws an error
      selectCaseBranch :: Arrow a => Int -> [a b c] -> a b c
      selectCaseBranch x [] = error $ "Branch " ++ show x ++ " wasn't found when using a case statement"
      selectCaseBranch 0 (caseB : caseBs) = caseB
      selectCaseBranch x (cb : cbs) = selectCaseBranch (x - 1) cbs
      caseBranch = selectCaseBranch indexOf caseBranches

instance CaseIndex (ConstA (S.Set Texture)) where
  caseIndex _ branches = ConstA textures
    where
      sets = map getConstA branches
      textures = foldl1 (<>) sets

-- PROBLEM textures might be in b aswell. In an arrow function we don't have access to b. we can instantiate a b to a const variable but we don't have access to that variable
-- I don't know how to start this
data PatternList p a b c
  = PL
      [(p, a b c)] -- List containing a pattern p and a branch a b c, when p is given branch a b c should be executed
      (a b c) -- a default action a b c that only gets executed if p wasn't found in the list

class Arrow a => CaseList a where
  caseList :: (Show p, Eq p) => p -> PatternList p a b c -> a b c

instance CaseList (->) where
  caseList p (PL patternList defaultAction) = selectPattern p patternList
    where
      selectPattern p [] = defaultAction
      selectPattern p ((ptn, branch) : ps) = if p == ptn then branch else selectPattern p ps

instance CaseList (ConstA (S.Set Texture)) where
  caseList _ (PL patternList defaultAction) = ConstA textures
    where
      textureSets = map (getConstA . snd) patternList
      textures = foldl1 (<>) textureSets <> getConstA defaultAction

{-

  Case handled as an Arrow looks like this
  a PatternType (a b c)
  given a patternType it gives back an arrow a b c where a is the same type

  The problem is that Somehow we need to do an if check on the pattern because we can't use pattern matching
  This way of formulating the problem also doesn't give access to the textures in a static manner
-}

instance Monoid a => ArrowChoice (ConstA a) where
  left (ConstA a) = ConstA a

class ArrowChoice a => IfA a where
  ifA :: a b (Either c d) -> a c e -> a d e -> a b e

instance IfA (->) where
  ifA a b1 b2 = a >>> b1 ||| b2 -- a (Either b c) d

instance Monoid a => IfA (ConstA a) where
  ifA (ConstA tb) (ConstA t1) (ConstA t2) = ConstA $ tb <> t1 <> t2

ifThenElseA :: ArrowChoice a => a b (Either c d) -> a c e -> a d e -> a b e
ifThenElseA a b1 b2 = a >>> b1 ||| b2

notA :: Arrow a => a (Either c d) (Either d c)
notA = arr swap
  where
    swap (Right x) = Left x
    swap (Left x) = Right x

data List8 a = L8 a a a a a a a a deriving (Show, Eq)

class Arrow a => Case8 a where
  case8 :: Indexable i => i -> List8 (a b c) -> a b c

instance Semigroup a => Semigroup (ConstA a b c) where
  (<>) (ConstA a1) (ConstA a2) = ConstA $ a1 <> a2

instance Case8 (->) where
  case8 indexable (L8 b1 b2 b3 b4 b5 b6 b7 b8) =
    case index indexable of
      0 -> b1
      1 -> b2
      2 -> b3
      3 -> b4
      4 -> b5
      5 -> b6
      6 -> b7
      7 -> b8

instance Case8 (ConstA (S.Set Texture)) where
  case8 _ (L8 b1 b2 b3 b4 b5 b6 b7 b8) = b1 <> b2 <> b3 <> b4 <> b5 <> b6 <> b7 <> b8

{-

  Make a Datatype that contains seven branches so that we never get the error from the first try

  Try to implement a case statement via if branches, i.e. a function p -> Either p p

-}

{-
  PROBLEMS:
  --> way too nested, doesn't look clean
  too many lines that need to be written, because of all the pattern matching. Don't see a way to clean that up

  What do i want to improve

  I want to be able to program a chain where you can represent the tree better without all the nesting

                                              EXAMPLE
                                                Branch
                                              /     \
                                            a b c   Branch
                                                  /       \
                                                a b c     Branch
                                                        /      \
                                                      a b c   Branch
                                                            /     \
                                                          a b c   a b c

  This left sided tree needs to be able to be written in terms of a link operator like >>> instead of the ifA operator

  What do we have
  input type b that can be whatever you need to pattern match and to decide the left or right type
  a b (Either c d) dependant on the input b we can either have a Left c type or a Right d type.
  a d e. One specific branch that works on a d type to provide an e type, this needs to be computed when the first arrow gives a Right c type from b
  when a Left c is given we want to follow the chain to the next function that can provide the b type so we know c == b

                                  Right      Left   TOTAL ARROW
 something :: a b (Either b c) -> a c d -> a b d -> a b d

 match :: a b (Either c d) -> a d e -> a b (Either c e) -- Initial function, given an arrow that takes an input b and categorizes it,
 if it's right then chain it with a d e if it's left then push it through

 (<->) :: a b (Either c d) -> a (Either c d) (Either c e) -> a b (Either c e)

 branch :: (b -> Either c d) -> a d e -> a (Either b e) (Either b e)

 given input Either b c, make an arrow that goes to Either b d

 defaultAction :: a b (Either c d) -> a c d -> a b d

-}

-- Ben heel blij met deze oplossing , maar er kan misschien nog één optimalisatie gebeuren. Namelijk het vroeg stoppen

class ArrowChoice a => Case a where
  match :: a b (Either b c) -> a c d -> a b (Either b d)
  (<-->) :: a b (Either b d) -> (a b (Either b c), a c d) -> a b (Either b d)
  (<-|->) :: a b (Either b c) -> a b c -> a b c

instance Case (->) where
  match a1 a2 = a1 >>> right a2
  (<-->) a1 (a2, a3) = a1 >>> (a2 >>> right a3) ||| \c -> Right c
  (<-|->) a1 a2 = a1 >>> a2 ||| arr id

instance Monoid a => Case (ConstA a) where
  match (ConstA a1) (ConstA a2) = ConstA $ a1 <> a2
  (<-->) (ConstA a1) (ConstA a2, ConstA a3) = ConstA $ a1 <> a2 <> a3
  (<-|->) (ConstA a1) (ConstA a2) = ConstA $ a1 <> a2


-- Potentieel signaleren met threads???


class Arrow a => InitialiseTexture a where
  addTexture :: Texture -> a (Texture -> b) b
  (>=>) :: a b (c -> d) -> a () c -> a b d
  initialiseConstructor :: b -> a () b
  (>*>) :: a b (c-> d) -> c -> a b d

instance InitialiseTexture (->) where
  addTexture t f = f t
  (>=>) a1 a2 = a1 >>> arr (,()) >>> second a2 >>> arr (\(f, obj) -> f obj)
  initialiseConstructor constructor = arr (const constructor)
  (>*>) a1 c = a1 >>> (\f -> f c)

instance InitialiseTexture (ConstA (S.Set Texture)) where
  addTexture t = ConstA (S.singleton t)
  (>=>) (ConstA a1) (ConstA a2) = ConstA $ a1 <> a2
  initialiseConstructor _ = ConstA S.empty
  (>*>) (ConstA a1) _ = ConstA a1

newtype Object a = Object { getObject :: a} deriving (Show, Eq)

instance Functor Object where
  fmap f (Object a) = Object $ f a

instance Applicative Object where
  pure a = Object a
  (Object f) <*> (Object a) = Object $ f a

instance Semigroup a => Semigroup (Object a) where
  (Object a1) <> (Object a2) = Object $ a1 <> a2

instance Monoid a => Monoid (Object a) where
  mempty = Object mempty 

class Applicative a => InitialiseObject a where
  initialiseTexture :: a (Texture -> b) -> Texture -> a b

instance InitialiseObject Object where
  initialiseTexture (Object f) t = Object $ f t

instance InitialiseObject (Const (S.Set Texture)) where
  initialiseTexture (Const ts) t = Const $ S.insert t ts


class InitialiseObject a => InitialiseList a where
  addObject :: a [b] -> a b -> a [b]
  addTexturedObject :: a [b] -> (Texture, Texture -> b) -> a [b]
  initialList :: a [b]

instance InitialiseList Object where
  addObject (Object bs) (Object b) = Object $ b : bs
  addTexturedObject (Object bs) (t, f) = Object $ f t : bs
  initialList = pure []

instance InitialiseList (Const (S.Set Texture )) where
  addObject (Const ts1) (Const ts2) = Const $ ts1 <> ts2
  addTexturedObject (Const ts1) (t, _) = Const $ S.insert t ts1
  initialList = Const S.empty


class Arrow a => TexturedArrows a where
  textureA :: Texture -> (Texture -> b -> c) -> a b c
  texturesA :: [Texture] -> ([Texture] -> b -> c) -> a b c


instance TexturedArrows (->) where
  textureA t tf a = tf t a
  texturesA ts tf a = tf ts a

type ConstATexture = ConstA (S.Set Texture)

instance TexturedArrows ConstATexture where
  textureA t _ = ConstA $ S.singleton t
  texturesA ts _ = ConstA $ S.fromList ts