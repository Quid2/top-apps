{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeFamilies              #-}

-- | Repository of absolute data types with an embedded web interface to browse data types and access the Haskell/JS ... equivalents
module Main where

import           Data.Bifunctor
import           Data.Foldable                        (toList)
import           Data.List                            (nub, partition, sort,
                                                       sortBy, sortOn)
import qualified Data.ListLike.String                 as L
import qualified Data.Map                             as M
import           Data.Maybe
import           Data.Ord
import           Data.String
import qualified Data.Text                            as T
import           Data.Word
import           Model.Validate
import           Network.Top                          hiding (solve, (<>))
import           Network.Top.Repo
import qualified Network.Top.Repo0                    as R0
import qualified Network.Wai
import qualified Network.Wai.Handler.Warp             as Warp
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Quid2.Util.Service
import           Repo.DB
import           Repo.Types                           hiding (Repo (..))
import           System.IO                            (stdout)
import           Text.Blaze.Html.Renderer.Text
import           Text.Blaze.Html5                     hiding (head, html, input,
                                                       main, map, output, param)
import           Text.Blaze.Html5.Attributes          hiding (async)
import           Text.PrettyPrint
import           Util
import           Web.Scotty
import qualified ZM.Parser                            as P
import           ZM.To.ZMT                            (generate)
import           ZM.Type.Repo
import qualified ZM.Type.String                       as Z

type LangChan = Function (Validate (SourceCode ZM)) [Note Z.String Range]

type SolveChan = Function (Solve AbsRef AbsADT) (Maybe AbsADT) -- AbsADT)

-- Functions
recordADTFun :: DB -> Record AbsADT -> IO ()
--recordADTFun db (Model.Function.Record adt) = recordADT db adt
recordADTFun db (Record adt) = recordADT db adt

{- |
>>> validateZMFun $ Validate (SourceCode ZM (Z.String "Bool == False \n| True"))

-}
-- Validate ZM data declarations, return errors if found
validateZMFun :: Monad m => Validate (SourceCode ZM) -> m [Note Z.String Range]
validateZMFun (Validate sc@(SourceCode ZM (Z.String s))) = return $ errs s
  where
    errs =
      either
        (map
           (\lerr ->
              let r = P.label lerr
               in Note
                    (Z.String (P.object lerr))
                    (Range
                       (Position (P.line r) (P.start r))
                       (Position (P.line r) (P.end r)))))
        (const []) .
      P.parseADTs

-- solveRef :: DB -> (Solve AbsRef) -> IO (Either Z.String AbsADT)
solveRefFun :: DB -> (Solve AbsRef AbsADT) -> IO AbsADT
solveRefFun db (Solve ref) = do
  madt <- getDB db ref
  case madt of
    Nothing  -> error $ unwords ["Unknown type", prettyShow ref]
      -- return Left $ Z.String $ unwords ["Unknown type", prettyShow ref]
    --Just adt -> Right $ Z.String $ prettyShow adt
    Just adt -> return adt

--knownDataTypesFun :: DB -> AllKnown (AbsRef, AbsADT) -> IO [(AbsRef, AbsADT)]
knownDataTypesFun :: DB -> AllKnown (AbsRef, AbsADT) -> IO [(AbsRef, AbsADT)]
knownDataTypesFun db AllKnown = allAssocs db

knownDataTypeRefsFun :: DB -> AllKnown (AbsRef, String) -> IO [(AbsRef, String)]
knownDataTypeRefsFun db AllKnown
  --map (second (Z.String . convert . declName)) <$> allAssocs db
 = map (second (convert . declName)) <$> allAssocs db

allAssocs db = (\(DBState env) -> M.assocs env) <$> wholeDB db

main :: IO ()
main = initService "top-repo2" setup

setup :: Quid2.Util.Service.Config () -> IO ()
setup cfg
  --updateGlobalLogger rootLoggerName $ setLevel DEBUG -- INFO
 = do
  logLevelOut DEBUG stdout
  db <- openDB (stateDir cfg)
  readPreviousRepo db
  -- Channel Agents
  runFunction $ recordADTFun db
  runFunction $ solveRefFun db
  runFunction $ knownDataTypesFun db
  runFunction $ knownDataTypeRefsFun db
  runFunction validateZMFun
  wwwUI db
  async $ do
    run $ recordType (Proxy :: Proxy ((), (), ()))
    run $ recordType (Proxy :: Proxy ((), (), (), ()))
  forever $ threadDelay 1000000000

-- Add types from previous repo service
readPreviousRepo db = do
  ets <- R0.knownTypes def
  case ets of
    Right ts -> liftIO $ mapM_ (recordADT db . snd) ts
    Left e   -> warn ["Unable to contact top-repo", show e]

-- WWW interface to show stored types
wwwUI db = do
  sapp :: Network.Wai.Application <-
    scottyApp $ do
      middleware logStdoutDev
      get "/" $ do
        lst <-
          liftIO $ do
            DBState db <- wholeDB db
            return . renderHtml . dbIndex $ db
        html lst
      get "/type/:typeCode" $ do
        key <- AbsRef . unPrettyRef <$> param "typeCode"
        out <-
          liftIO $ do
            DBState env <- wholeDB db
            madt <- getDB db key
            return $
              maybe
                "Unknown type"
                (renderHtml . pre . fromString . ppr . (env, ))
                madt
        html out
  let warpOpts =
        Warp.setLogger noRequestLogger . Warp.setPort 7000 . Warp.setTimeout 60 $ -- MOVED FROM 8000 TO 7000
        Warp.defaultSettings
  Warp.runSettings warpOpts sapp
  where
    dbIndex db = do
      table .
        mconcat .
        (tr (mconcat [th "Types", th "Unique Code"]) :) .
        map
          (\(adtS, r) ->
             tr $
             mappend
               (td . toHtml $
                a ! href (fromString $ "/type/" ++ ppr r) $ toHtml adtS)
               (td . toHtml . ppr $ r)) .
        sortOn fst .
        map
          (\(r, adt) -> ((convert :: Identifier -> String) . declName $ adt, r)) .
        M.toList $
        db
    -- table . mconcat . (tr (mconcat [th "Types",th "Unique Code"]) :) . map (\(adtS,r) -> tr $ mappend (td . toHtml $ a ! href (fromString $ "/type/"++ ppr r) $ toHtml adtS) (td . toHtml . ppr $ r)) . sortBy (comparing fst) . map (\(r,adt) -> (adtName adt,r)) . M.toList $ db
      p $
        mconcat
          [ b "NOTE:"
          , " The way the Types' unique codes are calculated will change and should not be relied upon."
          ]
        -- table . mconcat . (tr (mconcat [th "Type(s)"]) :) . map (\(adtS,r) -> tr (td . toHtml $ a ! href (fromString $ "/type/"++ ppr r) $ toHtml adtS)) . sortBy (comparing fst) . map (\(r,adt) -> (adtName adt,r)) . M.toList $ db
     -- table . mconcat . (tr (mconcat [th "Types"]) :) . map (\(adtS,r) -> tr (td . toHtml $ a ! href (fromString $ "/type/"++ ppr r) $ toHtml adtS)) . sortBy (comparing fst) . map (\(r,adt) -> (adtName adt,r)) . M.toList $ db
    adtName = unwords . sort . map declName . toList

recordADT db adt -- do
  --dbg ["Record",prettyShow (refS adt),show (refS adt),prettyShow adt]
 = putDB db (absRef adt) adt

-- Record a definition
inDB db rs =
  lift $
  (\(ps, as) -> (map (second fromJust) ps, map fst as)) .
  partition (isJust . snd) <$>
  mapM (\r -> (r, ) <$> getDB db r) rs

runFunction f = do
  async (runAppForever def ByType $ funReply f)
  run $ recordType (funProxy f)

funProxy :: forall i o. (i -> IO o) -> Proxy (Function i o)
funProxy _ = Proxy

-- pp = head . toList . snd . head . M.elems . snd $ absTypeEnv (Proxy :: Proxy (Bool))
ppr = render . pPrint
