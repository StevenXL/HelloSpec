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

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader (ReaderT)
import           Database.Persist.Sqlite (SqlBackend, LogFunc, Filter, Entity, runSqlite, runMigration, wrapConnection, rawSql, insert, count, runSqlConn, transactionSave, transactionUndo)
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
                                  transactionSave
                                  let person = Person "John Doe" (Just 35)
                                  _ <- insert person
                                  count <- getPersonCount
                                  transactionUndo
                                  pure count
                personCount `shouldBe` 1
            it "tests don't interact" $  \c -> do
                personCount <- runDb c $ do
                                  transactionSave
                                  let person = Person "John Doe" (Just 35)
                                  let person' = Person "Jane Doe" (Just 35)
                                  insert person
                                  insert person'
                                  count <- getPersonCount
                                  transactionUndo
                                  pure count
                personCount `shouldBe` 2 -- will be 3 if first tests interacts

getPersonCount :: MonadIO m => ReaderT SqlBackend m Int
getPersonCount = count ([] :: [Filter Person])

withDatabaseConnection :: SpecWith SqlBackend -> Spec
withDatabaseConnection = beforeAll dbSetup

runDb :: (MonadUnliftIO m) => SqlBackend -> ReaderT SqlBackend m a -> m a
runDb = flip runSqlConn

-- | Use an in-memory SQLite database, run the migration, return the connection
-- for use the the specs
dbSetup :: IO SqlBackend
dbSetup = do
    rawConn     <- open ":memory:"
    conn        <- wrapConnection rawConn noLogging
    _           <- runSqlConn (runMigration migrateAll) conn
    return conn

-- | We don't care about logging
noLogging :: LogFunc
noLogging _ _ _ _ = return ()