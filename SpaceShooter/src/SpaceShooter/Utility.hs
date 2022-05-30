module SpaceShooter.Utility where

import Control.Arrow
import Lens.Micro
import SpaceShooter.Objects
import System.Random

randInt :: Arrow a => (Int, Int) -> a World (World, Int)
randInt range =
  arr
    ( \game ->
        let stdGen' = game ^. stdGen
            (i, gen') = randomR range stdGen'
         in (game & stdGen .~ gen', i)
    )

replace :: Eq a => a -> a -> [a] -> [a]
replace old new [] = []
replace old new (s:ss) = if old == s then new : ss else s : replace old new ss

removeFirst :: Eq a => a -> [a] -> [a]
removeFirst _ [] = []
removeFirst a (a':as) = if a == a' then as else a' : removeFirst a as

removeAll :: Eq a => a -> [a] -> [a]
removeAll _ [] = []
removeAll a (a':as) = if a == a' then removeAll a as else a' : removeAll a as