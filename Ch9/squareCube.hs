module SquareCube where

mySqr  = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

tupes = [(x, y) | x <- mySqr, y <- myCube]

tupesLow = [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]

tupesLowLength = length tupesLow
