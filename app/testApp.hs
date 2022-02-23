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
-- [ovhTest "KS-1",ovhTest "KS-2"] ++

main = run $ map wwwTest $
    map (\n -> ("http://"++ n,"Flat")) ["quid2.biz","quid2.com","quid2.info","quid2.org","quicquid.org","golo.so"]
    ++ [("https://quid2.org","Flat")
        ,("http://kamus.it/","Assini")
        ,("http://massimoassini.quicquid.org/","Massimo Assini")
        ,("http://quid2.org/app/ui","channels")
        ,("http://quid2.net/","top-router")
        ,("http://quid2.net:7000/","BLOB")
        ,("http://vip.quid2.org/","top-router")
        ,("http://vip.quid2.org:7000/","BLOB")]
