{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Main where
import Yesod
import Yesod.Static
import Foundation
import Application
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Database.Persist.Postgresql

connStr = "dbname=d8gvpk56lm77g4 host=ec2-54-83-49-44.compute-1.amazonaws.com user=gmzzrpxkugxpzg password=7a7d5b9d667fe212382a0da7d282088f42d1c86f2dcaee29dcd875fb4d05603f port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool 
       t@(Static settings) <- static "static"
       warp 3000 (Sitio t pool)