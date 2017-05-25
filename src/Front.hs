{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Front where
import Yesod
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Handlers
import Database.Persist.Postgresql

mkYesodDispatch "Sitio" resourcesSitio

-- PARA USAR AS IMAGENS, EH NECESSARIO stack clean
-- E DEPOIS stack build para o Yesod criar as funcoes
-- baseadas no arquivos
-- haskell.jpg -> haskell_jpg

-- O lucius e cassius FICAM NO EXECUTAVEL
-- addStylesheet/addScript NAO deixa css/js no EXECUTAVEL
getTesteR :: Handler Html
getTesteR = do 
    defaultLayout $ do
        addStylesheet (StaticR teste_css)
        toWidgetHead [julius|
            function ola(){
                alert("Ola mundo!");
            }
        |]
        toWidgetHead [lucius|
            ul {
                list-style: none;
            }
            li {
                float:left;
                padding:5px;
            }
        |]
        [whamlet|
            <ul>
                <li> 
                    <a href=@{Pag1R}> Página 1
                <li>
                    <a href=@{Pag2R}> Página 2
                <li>
                    <a href=@{Pag3R}> Página 3
            <br>
            <h1>
                _{MsgHello}
            <p>
                UM PARAGRAFO!
            <button onclick="ola()">
                Click
            <img src=@{StaticR haskell_jpg}>
        |]

getPag1R :: Handler Html
getPag1R = do
    defaultLayout $ do
        [whamlet|
            <h1> 
                BEM-VINDO À Página 1
            
            <a href=@{TesteR}> 
                Voltar
        |]

getPag2R :: Handler Html
getPag2R = do
    defaultLayout $ do
        [whamlet|
            <h1> 
                BEM-VINDO À Página 2
            
            <a href=@{TesteR}> 
                Voltar
        |]

getPag3R :: Handler Html
getPag3R = do
    defaultLayout $ do
        [whamlet|
            <h1> 
                BEM-VINDO À Página 3
            
            <a href=@{TesteR}> 
                Voltar
        |]