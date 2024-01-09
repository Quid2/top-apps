module Quid2.Util.String(after,till,between) where

-- import Data.List.Utils

after :: (Eq a) => [a] -> [a] -> Maybe [a]
after srch text = subIndex srch text >>= \n -> return $ drop (n + length srch) text

till :: (Eq a) => [a] -> [a] -> Maybe [a]
till srch text = subIndex srch text >>= \n -> return $ take n text

between :: (Eq a) => [a] -> [a] -> [a] -> Maybe [a]
between beg end txt = after beg txt >>= till end

