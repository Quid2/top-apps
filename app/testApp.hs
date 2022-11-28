{-# LANGUAGE OverloadedStrings #-}

{- |
Detect failures of www and top infrastructure and send an alarm in case of failure.

TODO:
- remove single points of failure:
impossibility to connect with pushover (api changes)

Full test:
- direct top tests

NOTE: report only first failure
-}
module Main where

import           Test

{-
>>> 3 + 3
-}

t = run [wwwTest ("http://vip.quid2.org/","top-router")]

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
        ,("http://quid2.net:7000/","ADT")
        ,("http://net.quid2.org/","top-router")
        ,("http://net.quid2.org:7000/","ADT")
        ]
