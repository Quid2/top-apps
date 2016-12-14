{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
module Main where

-- |Repository of absolute data types
-- with an embedded web interface to browse data types and access the Haskell/JS ... equivalents
import           Data.Bifunctor
import           Data.Foldable                        (toList)
import           Data.List                            (nub, partition, sort,
                                                       sortBy)
import qualified Data.ListLike.String                 as L
import qualified Data.Map                             as M
import           Data.Maybe
import           Data.Ord
import           Data.String
import qualified Data.Text                            as T
import           Network.Top                          hiding (solve, (<>))
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

u= recordType def (Proxy::Proxy RepoProtocol)

main = initService "quid2-repo" setup

setup :: Quid2.Util.Service.Config () -> IO ()
setup cfg = do
  -- updateGlobalLogger rootLoggerName $ setLevel DEBUG -- INFO
  logLevelOut DEBUG stdout

  db <- openDB (stateDir cfg)

  async $ runAgent (agent db)

  sapp :: Network.Wai.Application <- scottyApp $ do
     middleware logStdoutDev
     get "/" $ do
       lst <- liftIO $ do
         DBState db <- wholeDB db
         return . renderHtml . dbIndex $ db
       html lst

     get "/type/:typeCode" $ do
        key <- AbsRef . unPrettyRef <$> param "typeCode"
        out <- liftIO $ do
          DBState env <- wholeDB db
          madt <- getDB db key
          return $ maybe "Unknown type" (renderHtml . pre . fromString . ppr . (env,)) madt
        html out

  let warpOpts = Warp.setLogger noRequestLogger . Warp.setPort 8000 . Warp.setTimeout 60 $ Warp.defaultSettings
  Warp.runSettings warpOpts sapp

    where

      dbIndex db = do
            table . mconcat . (tr (mconcat [th "Types",th "Unique Code"]) :) . map (\(adtS,r) -> tr $ mappend (td . toHtml $ a ! href (fromString $ "/type/"++ ppr r) $ toHtml adtS) (td . toHtml . ppr $ r)) . sortBy (comparing fst) . map (\(r,adt) -> (L.toString . declName $ adt,r)) . M.toList $ db
            -- table . mconcat . (tr (mconcat [th "Types",th "Unique Code"]) :) . map (\(adtS,r) -> tr $ mappend (td . toHtml $ a ! href (fromString $ "/type/"++ ppr r) $ toHtml adtS) (td . toHtml . ppr $ r)) . sortBy (comparing fst) . map (\(r,adt) -> (adtName adt,r)) . M.toList $ db
            p $ mconcat [b "NOTE:"," The way the Types' unique codes are calculated will change and should not be relied upon."]
            -- table . mconcat . (tr (mconcat [th "Type(s)"]) :) . map (\(adtS,r) -> tr (td . toHtml $ a ! href (fromString $ "/type/"++ ppr r) $ toHtml adtS)) . sortBy (comparing fst) . map (\(r,adt) -> (adtName adt,r)) . M.toList $ db
         -- table . mconcat . (tr (mconcat [th "Types"]) :) . map (\(adtS,r) -> tr (td . toHtml $ a ! href (fromString $ "/type/"++ ppr r) $ toHtml adtS)) . sortBy (comparing fst) . map (\(r,adt) -> (adtName adt,r)) . M.toList $ db

      adtName = unwords . sort . map declName . toList

      agent db = do
        msg <- await
        dbg  ["MSG",show msg]
        case msg of
          Record adt -> do
            --dbg ["Record",prettyShow (refS adt),show (refS adt),prettyShow adt]
            lift $ putDB db (absRef adt) adt

          AskDataTypes -> do
            vs <- lift $ (\(DBState env) -> M.assocs env) <$> wholeDB db
            yield . KnownDataTypes $ vs

          Solve ref -> do
            madt <- lift $ getDB db ref
            case madt of
              Nothing  -> return ()
              Just adt -> yield $ Solved ref adt

          -- Solve typ -> do
          --   rs <- lift $ mapM (\r -> (r,) <$> getDB db r) . references $ typ
          --   let errs = map fst . filter (isNothing . snd) $ rs
          --   yield . Solved typ $ if null errs
          --                        then Right (map (second fromJust) rs)
          --                        else Left $ T.unwords ["Unknown types:",T.pack $ show errs]

          _ -> return ()

        agent db

-- newAgent db = runAgent (agent db)
--   where
--     agent db = do

--       -- Load initial state from other agents
--       yield AskRefs

--       msg <- await
--       case msg of

--         AskIsKnown r -> do
--           known <- lift $ isJust <$> getDB db r
--           when known $ yield (IsKnown r)

--         IsKnown r -> return () -- ?

--         AskADT r -> do
--           mADT <- lift $ getDB db r
--           case mADT of
--             Nothing -> return ()
--             Just adt -> yield $ KnownADT r adt

--         AskRefs -> do
--           vs <- lift $ (\(DBState env) -> M.assocs env) <$> wholeDB db
--           yield . KnownRefs $ map (second declName) vs

--         KnownRefs ks -> do
--           unknowns <- snd <$> inDB db (map fst ks)
--           mapM_ (yield . AskADT) unknowns
--         -- TODO: Store when fully known
--         -- KnownADT ref adt -> lift $ putDB db ref adt
--       agent db

-- Record a definition
-- runRecord typ = runAgent $ do yield

inDB db rs = lift $ (\(ps,as) -> (map (second fromJust) ps,map fst as)) . partition (isJust . snd) <$> mapM (\r -> (r,) <$> getDB db r) rs

runAgent agent = runClientForever def ByType $ \conn -> runEffect $ pipeIn conn >-> agent >-> pipeOut conn

-- pp = head . toList . snd . head . M.elems . snd $ absTypeEnv (Proxy :: Proxy (Bool))
ppr = render . pPrint



