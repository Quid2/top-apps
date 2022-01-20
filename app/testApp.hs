{-# LANGUAGE OverloadedStrings #-}

{- |
Detect failures of www and top infrastructure and send an alarm in case of failure.

TODO:
- remove single points of failure:

Full test:
- direct top tests
-}
module Main where

import           Test

{-
>>> 3 + 3

-}

main :: IO ()
-- main = run [ovhTest "KS-17"] --


main = run $ [ovhTest "KS-1",ovhTest "KS-2"] ++ map wwwTest [("https://quid2.org","Flat"),("http://quid2.org","Flat"),("http://kamus.it/","Assini"),("http://quid2.org/app/ui","channels"),("http://vip.quid2.org/","top-router"),("http://vip.quid2.org:7000/","BLOB")]
