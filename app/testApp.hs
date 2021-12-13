{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}

{- |
Detect failures of www and top infrastructure and send an alarm in case of failure.

TODO:
- direct top tests
- self-test, every app includes its own test
-}
module Main where

import           Test

main :: IO ()
main = run $ map wwwTest [("https://quid2.org","Flat"),("http://quid2.org","Flat"),("http://kamus.it/","Assini"),("http://quid2.org/app/ui","channels"),("http://giga.quid2.org/","top-router"),("http://giga.quid2.org:7000/","BLOB")]