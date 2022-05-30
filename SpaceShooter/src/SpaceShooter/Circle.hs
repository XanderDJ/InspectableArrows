module SpaceShooter.Circle where

import SpaceShooter.Point

class Circle c where
    getCenter :: c -> Point Float
    getRadius :: c -> Float



touches :: (Circle c1, Circle c2) => c1 -> c2 -> Bool
touches c1 c2 = abs distBetween <= sumOfRadii
 where distBetween = distanceBetween (getCenter c1) (getCenter c2)
       sumOfRadii = getRadius c1 + getRadius c2
       

