{-# LANGUAGE QuasiQuotes #-}

-- | Represents the various jobs that can be run
module Flora.OddJobs
  ( scheduleReadmeJob
  , scheduleIndexImportJob
  , jobTableName
  , runner
  , checkIfIndexImportJobIsNotRunning

    -- * exposed for testing

  --   prefer using smart constructors.
  , ReadmePayload (..)
  , FloraOddJobs (..)
  , IntAesonVersion (..)
  )
where

import qualified Commonmark
import Control.Exception
import Control.Monad.IO.Class
import Data.Aeson (Result (..), fromJSON)
import Data.Pool
import Data.Text
import Data.Text.Display
import qualified Data.Text.Lazy as TL
import Database.PostgreSQL.Entity.DBT
import qualified Database.PostgreSQL.Simple as PG
import Distribution.Types.Version
import GHC.Stack
import Log
import qualified Lucid
import Network.HTTP.Types (notFound404)
import OddJobs.Job (Job (..), createJob, scheduleJob)
import Optics.Core
import Servant.Client (ClientError (..))
import Servant.Client.Core (ResponseF (..))
import qualified System.Process.Typed as System

import Control.Monad (void)
import Data.Time (nominalDay)
import qualified Data.Time as Time
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Transact (DBT)
import Flora.Model.Package
import Flora.Model.Release.Types
import Flora.Model.Release.Update (updateReadme)
import Flora.OddJobs.Types
import Flora.ThirdParties.Hackage.API (VersionedPackage (..))
import qualified Flora.ThirdParties.Hackage.Client as Hackage

scheduleReadmeJob :: Pool PG.Connection -> ReleaseId -> PackageName -> Version -> IO Job
scheduleReadmeJob pool rid package version =
  withResource pool $ \conn ->
    createJob
      conn
      jobTableName
      (MkReadme $ MkReadmePayload package rid $ MkIntAesonVersion version)

scheduleIndexImportJob :: Pool PG.Connection -> IO Job
scheduleIndexImportJob pool = withResource pool $ \conn -> do
  t <- Time.getCurrentTime
  let runAt = Time.addUTCTime nominalDay t
  scheduleJob
    conn
    jobTableName
    ()
    runAt

checkIfIndexImportJobIsNotRunning :: (MonadIO m) => DBT m Bool
checkIfIndexImportJobIsNotRunning = do
  (result :: Maybe (Only Int)) <-
    queryOne_
      Select
      [sql|
              select count(*)
              from "oddjobs"
              where payload ->> 'tag' = 'ImportHackageIndex'
      |]
  case result of
    Nothing -> pure True
    Just (Only 0) -> pure True
    Just (Only 1) -> pure True
    _ -> pure False

runner :: Pool PG.Connection -> Job -> JobsRunnerM ()
runner pool job = localDomain "job-runner" $
  case fromJSON (jobPayload job) of
    Error str -> logAttention "decode error" str
    Success val -> case val of
      MkReadme x -> makeReadme pool x
      ImportHackageIndex -> fetchNewIndex pool

fetchNewIndex :: Pool PG.Connection -> JobsRunnerM ()
fetchNewIndex pool = localDomain "index-import" $ do
  logInfo_ "Fetching new index"
  System.runProcess_ "cabal update"
  System.runProcess_ "~/.cabal/packages/hackage.haskell.org/01-index.tar 01-index/"
  System.runProcess_ "cd 01-index && tar -xf 01-index.tar"
  System.runProcess_ "make import-from-hackage"
  void $ liftIO $ scheduleIndexImportJob pool

makeReadme :: HasCallStack => Pool PG.Connection -> ReadmePayload -> JobsRunnerM ()
makeReadme pool pay@MkReadmePayload{..} = localDomain ("for-package " <> display mpPackage) $ do
  logInfo "making readme" pay
  let payload = VersionedPackage mpPackage mpVersion
  gewt <- Hackage.request $ Hackage.getPackageReadme payload
  case gewt of
    Left e@(FailureResponse _ response) -> do
      -- If the README simply doesn't exist, we skip it by marking it as successful.
      if response ^. #responseStatusCode == notFound404
        then liftIO $ withPool pool $ updateReadme mpReleaseId Nothing
        else throw e
    Left e -> throw e
    Right bodyText -> do
      logInfo "got a body" bodyText

      htmlTxt <- do
        -- let extensions = emojiSpec
        -- Commonmark.commonmarkWith extensions ("readme " <> show mpPackage) bodyText
        pure (Commonmark.commonmark ("readme " <> show mpPackage) bodyText)
          >>= \case
            Left exception -> throw (MarkdownFailed exception)
            Right (y :: Commonmark.Html ()) -> pure $ Commonmark.renderHtml y

      let readmeBody :: Lucid.Html ()
          readmeBody = Lucid.toHtmlRaw @Text $ TL.toStrict htmlTxt

      liftIO $ withPool pool $ updateReadme mpReleaseId (Just $ MkTextHtml readmeBody)
