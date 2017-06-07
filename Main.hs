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

connStr = "dbname=d4grpv8th4igam host=ec2-54-227-237-223.compute-1.amazonaws.com user=pdtfvtgliuyxfi password=75c1324323c93253692db7091cb6e2610f70117dfd3524469af5b535d0ea7bc2 port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool 
       t@(Static settings) <- static "static"
       warp 3000 (Sitio t pool)