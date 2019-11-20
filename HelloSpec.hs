#!/usr/bin/env stack
-- stack --resolver lts-11.6 script

{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeApplications           #-}

import           Control.Monad.Logger (runNoLoggingT)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Reader (ReaderT)
import           Database.Persist.Sqlite (SqlBackend, LogFunc, Filter, runMigrationSilent, withSqliteConn, wrapConnection, insert, count, runSqlConn)
import           Database.Persist.TH
import           Database.Sqlite (open)
import           Test.Hspec
import           Conduit (MonadUnliftIO)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name String
    age Int Maybe
    deriving Show
BlogPost
    title String
    authorId PersonId
    deriving Show
|]

main :: IO ()
main = hspec $ spec

spec :: Spec
spec = withDatabaseConnection specs

specs :: SpecWith SqlBackend
specs = describe "insertion" $ do
            it "can insert a person" $ \c -> do
                personCount <- runDb c $ do
                                  let person = Person "John Doe" (Just 35)
                                  _ <- insert person
                                  getPersonCount
                personCount `shouldBe` 1
            it "tests don't interact" $  \c -> do
                personCount <- runDb c $ do
                                  let person = Person "John Doe" (Just 35)
                                  let person' = Person "Jane Doe" (Just 35)
                                  insert person
                                  insert person'
                                  getPersonCount
                personCount `shouldBe` 2 -- will be 3 if first tests interacts

getPersonCount :: MonadIO m => ReaderT SqlBackend m Int
getPersonCount = count ([] :: [Filter Person])

withDatabaseConnection :: SpecWith SqlBackend -> Spec
withDatabaseConnection = around provideDb

provideDb :: ActionWith SqlBackend -> IO ()
provideDb func = runNoLoggingT $ withSqliteConn ":memory:" $ \conn -> do
                    _ <- runDb conn (runMigrationSilent migrateAll)
                    liftIO $ func conn

-- | runDb ensures that the action is ran in a transaction which is rolled back.
-- This ensures tests do not interact with one another.
runDb :: (MonadUnliftIO m) => SqlBackend -> ReaderT SqlBackend m a -> m a
runDb conn action = runSqlConn action conn
