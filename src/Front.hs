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


getMenuR :: Handler Html
getMenuR = defaultLayout $ do
    setTitle "Menu"
    addStylesheet $ StaticR menu_css
    sess <- lookupSession "_USER"
    [whamlet|
<html>
    <head>
        <title>
            Music Library (Versão Inicial)
<body>
<header>
    <div class="nav">
        <nav>
            <ul>
                <li><a>Home
                
                <li><a>Biblioteca
                    <ul>
                        <li><a href=@{ArtistaR}>Nova lista
                        <li><a href=@{ArtistaR}>Exibir listas
                
                <li><a>Músicas
                    <ul>
                        <li><a href=@{MusicaR}>Nova música
                        <li><a href=@{MusicaLstR}>Exibir músicas
                
                <li><a>Artistas
                    <ul>
                        <li><a href=@{ArtistaR}>Novo artista
                        <li><a href=@{ArtistaLstR}>Exibir artistas
                        
                <li><a>Álbuns</a>
                    <ul>
                        <li><a href=@{AlbumR}>Novo álbum
                        <li><a href=@{AlbumMusR}>Adicionar música
                        <li><a href=@{ArtistaR}>Exibir álbuns
                    
                <li><a>Vertentes
                    <ul>
                        <li><a href=@{VertenteR}>Nova vertente
                        <li><a href=@{VertenteLstR}>Exibir vertentes
    <div class="welcome">
        <h1>
            Bem vindo<br><br>
            Atualmente o site/app dispões das seguintes funcionalidades:
            <li>Cadastro de artistas
            <li>Cadastro de músicas
            <li>Cadastro de vertentes
            <li>Cadastro de álbuns
            <li>Organização de listas
            <br><br>
            Em desenvolvimento:
            <li>Layout
            <li>Importação de dados via biblioteca online
            <li>Pré-cadastro de músicas, artistas, vertentes e álbuns
<footer class="footer">
    <div class="rodape">
        <input class="logar" type="submit" value="Logout">
    |]

-- Sempre devemos usar o defaultLayout
-- pois hamlets, lucius, cassius e julius sao
-- da Monad Widget. A funcao defaultLayout
-- transforma Widgets em Handlers
getPag4R :: Handler Html
getPag4R = defaultLayout $ do
    [whamlet|
        <div id="alvo">
           ^{widget1}
    |]

widget1 :: Widget
widget1 = [whamlet|
    <p>
        Parágrafo dentro da div
|]

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    setTitle "Minha página"
    --addStylesheet $ StaticR teste_css
    sess <- lookupSession "_USER"
    toWidgetHead [hamlet|
        <meta name="keywords" content="Teste, Haskell">
    |]
    toWidgetHead [julius|
        function ola(){
            alert("Ola mundo!");
        }
    |]
    [whamlet|
        <h1>
            _{MsgHello}
        <a href=@{Pag1R}> _{MsgMenu1}
        <p>
            Uma parágrafo
        <button onclick="ola()">
            Clique!
        <br>
        $maybe sess' <- sess
            <form action=@{LogoutR} method=post>
                ite
                
    |]


-- PARA USAR AS IMAGENS, EH NECESSARIO stack clean
-- E DEPOIS stack build para o Yesod criar as funcoes
-- baseadas no arquivos
-- haskell.jpg -> haskell_jpg

-- O lucius e cassius FICAM NO EXECUTAVEL
-- addStylesheet/addScript NAO deixa css/js no EXECUTAVEL
getTesteR :: Handler Html
getTesteR = do 
    defaultLayout $ do
        --addStylesheet (StaticR teste_css)
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
            <img src=@{StaticR teste_jpg}>
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
