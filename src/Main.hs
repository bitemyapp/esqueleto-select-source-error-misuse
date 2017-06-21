module Main where

import Control.Arrow (first)
import Control.Monad
import Control.Monad.IO.Class(liftIO)
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Map as Map
import Database.Esqueleto
import Database.Esqueleto.Internal.Sql
import Database.Sqlite (reset)
import Database.Persist.Sqlite(runSqlite)
import Database.Persist.TH
import GHC.IORef
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name String
    deriving Show
|]

selectPerson :: MonadResource m
             => String
             -> Source (SqlPersistT m) (Key Person)
selectPerson name = do
    let source = selectSource $ from $ \person -> do
                       where_ $ person ^. PersonName ==. val name
                       return $ person ^. PersonId
    source =$= CL.map unValue

-- queryPerson :: MonadResource m
--              => String
--              -> SqlPersistT m (Value (Key Person))
queryPerson :: String -> SqlQuery (SqlExpr (Value (Key Person)))
queryPerson name = do
  from $ \person -> do
    where_ $ person ^. PersonName ==. val name
    return $ person ^. PersonId

-- main :: IO ()
-- main = do
--     runSqlite ":memory:" $ do
--       runMigration migrateAll
--       void $ insert $ Person "John Smith"
--       void $ insert $ Person "Jane Doe"

--     runSqlite ":memory:" $ do
--       selectSource (queryPerson "John Smith")
--         $$ CL.map unValue .| CL.consume >>= liftIO . print
--     runSqlite ":memory:" $ do
--       selectSource (queryPerson "Jane Doe")
--         $$ CL.map unValue .| CL.consume >>= liftIO . print

builderToText :: TLB.Builder -> T.Text
builderToText = TL.toStrict . TLB.toLazyTextWith defaultChunkSize
  where
    defaultChunkSize = 1024 - 32

main :: IO ()
main = runSqlite ":memory:" $ do
    runMigration migrateAll

    void $ insert $ Person "John Smith"
    void $ insert $ Person "Jane Doe"

    let js = queryPerson "John Smith"
    selectSource js
      $$ CL.map unValue .| CL.consume >>= liftIO . print

    conn <- ask
    stmts <- liftIO $ readIORef (connStmtMap conn)
    let queryText = fst $ first builderToText $ toRawSql SELECT (conn, initialIdentState) js
    liftIO $ print queryText
    forM_ (Map.lookup queryText stmts) (liftIO . stmtReset)
    -- select (queryPerson "John Smith") >>= liftIO . print
    select (queryPerson "Jane Doe") >>= liftIO . print
    -- selectSource (queryPerson "Jane Doe")
    --   $$ CL.map unValue .| CL.consume >>= liftIO . print

    -- selectPerson "John Smith" $$ CL.consume >>= liftIO . print
    -- selectPerson "Jane Doe"   $$ CL.consume >>= liftIO . print

    -- runResourceT $ do
    --   lift $ selectPerson "John Smith" $$ CL.consume >>= liftIO . print
    --   lift $ selectPerson "Jane Doe"   $$ CL.consume >>= liftIO . print

-- main :: IO ()
-- main = runSqlite ":memory:" $ do
--     runMigration migrateAll

--     void $ insert $ Person "John Smith"
--     void $ insert $ Person "Jane Doe"
--     runResourceT $ do
--       lift $ selectPerson "John Smith" $$ CL.consume >>= liftIO . print
--     runResourceT $ do
--       lift $ selectPerson "Jane Doe"   $$ CL.consume >>= liftIO . print
