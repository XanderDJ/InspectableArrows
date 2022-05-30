module SpaceShooter.SpaceShips where

import PaSe
import SpaceShooter.Data
import Game.MTL
import Control.Arrow

matchFighter' :: (Game, Int) -> Either (Game, Int) (Game, (Health, Power, ShootStyle))
matchFighter' (game, 1) = Right (game, (10, 10, Single))
matchFighter' other = Left other

matchFighter :: Arrow a => a (Game, Int) (Either (Game, Int) (Game, (Health, Power, ShootStyle)))
matchFighter = arr matchFighter'

matchCruiser' :: (Game, Int) -> Either (Game, Int) (Game, (Health, Power, ShootStyle))
matchCruiser' (game, 2) = Right (game, (50, 20, Spread))
matchCruiser' other = Left other

matchCruiser :: Arrow a => a (Game, Int) (Either (Game, Int) (Game, (Health, Power, ShootStyle)))
matchCruiser = arr matchCruiser'

matchDestroyer' :: (Game, Int) -> Either (Game, Int) (Game, (Health, Power, ShootStyle))
matchDestroyer' (game, 3) = Right (game, (30, 30, Multi))
matchDestroyer' other = Left other

matchDestroyer :: Arrow a => a (Game, Int) (Either (Game, Int) (Game, (Health, Power, ShootStyle)))
matchDestroyer = arr matchDestroyer'


matchBoss' :: (Game, Int) -> Either (Game, Int) (Game, (Health, Power, ShootStyle))
matchBoss' (game, 4) = Right (game, (100, 20, Single))
matchBoss' other = Left other

matchBoss :: Arrow a => a (Game, Int) (Either (Game, Int) (Game, (Health, Power, ShootStyle)))
matchBoss = arr matchBoss'