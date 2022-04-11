module Main where

import           Control.Concurrent      (threadDelay)
import           Control.Monad           (void)
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Data.Aeson              (ToJSON)
import qualified Data.Aeson.Text         as Aeson
import           Data.Function           ((&))
import qualified Data.Text               as T
import qualified Data.Text.Lazy.IO       as L
import           Data.Time.Clock         (getCurrentTime)
import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Servant.Client          (runClientM)
import           System.Environment      (getArgs, lookupEnv)


import qualified Autoteka                as Att

main :: IO ()
main = do
  Just clientId <- fmap T.pack <$> lookupEnv "CLIENT_ID"
  Just clientSecret <- fmap T.pack <$> lookupEnv "CLIENT_SECRET"
  [regNumber] <- map T.pack <$> getArgs

  env <- Att.mkClientEnv <$> newManager tlsManagerSettings
  res <- flip runClientM env $ do
    tok <- Att.getToken clientId clientSecret
    liftIO $ print tok
    Att.getActivePackage tok >>= liftIO . print

    pid <- Att.createPreview tok (Att.RegNumber regNumber)
    void $ loop (1 & seconds) (\p -> Att.p_status p == "processing") $ do
      Att.getPreview tok pid
    rep <- Att.createReport tok pid
    void $ loop (10 & seconds) (\r -> Att.r_status r == "processing") $ do
      Att.getReport tok (Att.r_reportId rep)
  print res


seconds :: Int -> Int
seconds n = n * 1000000

loop :: (MonadIO m, ToJSON r) => Int -> (r -> Bool) -> m r -> m r
loop usec cond f = go
  where
    go = do
      res <- f
      liftIO $ putStrLn "\n===" >> getCurrentTime >>= print
      liftIO $  L.putStrLn $ Aeson.encodeToLazyText res
      if cond res
         then liftIO (threadDelay usec) >> go
         else return res
