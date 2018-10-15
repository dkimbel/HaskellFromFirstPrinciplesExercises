module Binding where

bindExp :: Integer -> String
bindExp x = let y = 5 in
              "the integer was: " ++ show x
              ++ " and y was: " ++ show y

bindExp' :: Integer -> String
bindExp' x = let z = y + x
                 y = 5 
             in "the integer was: "
              ++ show x ++ " and y was: " 
              ++ show y ++ " and z was: " ++ show z

bindShadow :: Integer -> String
bindShadow x = let x = 10; y = 5 in
                 "the integer was: " ++ show x
                 ++ " and y was: " ++ show y
