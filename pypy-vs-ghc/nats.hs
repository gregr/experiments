module Main where

two f x = let y = f x in y `seq` f y

three f x = let y = two f x in y `seq` f y

test = three three two (1 +) 0

main = print test
