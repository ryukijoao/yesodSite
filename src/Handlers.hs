{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Handlers where
import Yesod
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text

import Database.Persist.Postgresql

vts = do
    entidades <- runDB $ selectList [] [Asc VertentesNome] 
    optionsPairs $ fmap (\ent -> (vertentesNome $ entityVal ent, entityKey ent)) entidades

art = do
    entidades <- runDB $ selectList [] [Asc ArtistasNome] 
    optionsPairs $ fmap (\ent -> (artistasNome $ entityVal ent, entityKey ent)) entidades

msc = do
    entidades <- runDB $ selectList [] [Asc MusicasNome] 
    optionsPairs $ fmap (\ent -> (musicasNome $ entityVal ent, entityKey ent)) entidades

abn = do
    entidades <- runDB $ selectList [] [Asc AlbunsNome] 
    optionsPairs $ fmap (\ent -> (albunsNome $ entityVal ent, entityKey ent)) entidades

formVertentes :: Form Vertentes
formVertentes = renderDivs $ Vertentes <$>
                areq textField "Nome: " Nothing

formArtistas :: Form Artistas
formArtistas = renderDivs $ Artistas <$>
                areq textField "Nome: " Nothing

formMusicas :: Form Musicas
formMusicas = renderDivs $ Musicas <$>
                areq textField "Nome: " Nothing  <*>
                areq (selectField art) "Artista principal: "  Nothing <*>
                areq (selectField vts) "Vertente: "  Nothing

formAlbuns :: Form Albuns
formAlbuns = renderDivs $ Albuns <$>
                areq textField "Nome: " Nothing  <*>
                areq (selectField art) "Artista principal: "  Nothing

formAlbunsMusicas :: Form AlbunsMusicas
formAlbunsMusicas = renderDivs $ AlbunsMusicas <$>
                areq (selectField abn) "Álbum: "  Nothing <*>
                areq (selectField msc) "Música: "  Nothing <*>
                areq intField "Disco: " Nothing


formDepto :: Form Departamento
formDepto = renderDivs $ Departamento <$>
            areq textField "Nome" Nothing <*>
            areq textField FieldSettings{fsId=Just "hident2",
                           fsLabel="Sigla",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("maxlength","3")]} Nothing

formPessoa :: Form Pessoa
formPessoa = renderDivs $ Pessoa <$>
             areq textField "Nome" Nothing <*>
             areq intField "Idade" Nothing <*>
             areq doubleField "Salario" Nothing <*>
             areq (selectField dptos) "Depto" Nothing

dptos = do
       entidades <- runDB $ selectList [] [Asc DepartamentoNome] 
       optionsPairs $ fmap (\ent -> (departamentoSigla $ entityVal ent, entityKey ent)) entidades

getHelloR :: Handler Html
getHelloR = defaultLayout [whamlet|
     <h1> _{MsgHello}
|]

getVertenteR :: Handler Html
getVertenteR = do
             (widget, enctype) <- generateFormPost formVertentes
             defaultLayout $ do
             addStylesheet $ StaticR cadastrosimples_css
             widgetFormCadastroSimples VertenteR enctype widget "Nova vertente"

getVertenteIdR :: VertentesId -> Handler Html
getVertenteIdR vid = do
             vertente <- runDB $ get404 vid 
             defaultLayout [whamlet| 
                 <h1> Vertente #{vertentesNome vertente}
             |]

getVertenteLstR :: Handler Html
getVertenteLstR = do
             listaV <- runDB $ selectList [] [Asc VertentesNome]
             defaultLayout $ do 
             addStylesheet $ StaticR listagemsimples_css
             [whamlet|
                <div class="container">
                    <h1> Artistas cadastrados
                    $forall Entity vid vertente <- listaV
                        <a href=@{VertenteIdR vid}> #{vertentesNome vertente} 
                        <form method=post action=@{VertenteIdR vid}> 
                            <input type="submit" value="Deletar"><br>
             |] 
             toWidget [lucius|
                form  { display:inline; }
                input { background-color: #ecc; border:0;}
             |]

getArtistaR :: Handler Html
getArtistaR = do
             (widget, enctype) <- generateFormPost formArtistas
             defaultLayout $ do
             addStylesheet $ StaticR cadastrosimples_css
             widgetFormCadastroSimples ArtistaR enctype widget "Novo artista"

getArtistaIdR :: ArtistasId -> Handler Html
getArtistaIdR aid = do
             artista <- runDB $ get404 aid
             defaultLayout [whamlet| 
                 <h1> Artista #{artistasNome artista}
             |]
             
getArtistaLstR :: Handler Html
getArtistaLstR = do
             listaA <- runDB $ selectList [] [Asc ArtistasNome]
             defaultLayout $ do 
             addStylesheet $ StaticR listagemsimples_css
             [whamlet|
                <div class="container">
                    <h1> Artistas cadastrados
                    $forall Entity vid artista <- listaA
                        <a href=@{ArtistaIdR vid}> #{artistasNome artista} 
                        <form method=post action=@{ArtistaIdR vid}> 
                            <input type="submit" value="Deletar"><br>
             |] 
             toWidget [lucius|
                form  { display:inline; }
                input { background-color: #ecc; border:0;}
             |]
             
getMusicaLstR :: Handler Html
getMusicaLstR = do
             listaM <- runDB $ selectList [] [Asc MusicasNome]
             defaultLayout $ do 
             addStylesheet $ StaticR listagemsimples_css
             [whamlet|
                <div class="container">
                    <h1> Músicas cadastradas
                    $forall Entity vid musica <- listaM
                        <a href=@{MusicaIdR vid}> #{musicasNome musica} 
                        <form method=post action=@{MusicaIdR vid}> 
                            <input type="submit" value="Deletar"><br>
             |] 
             toWidget [lucius|
                form  { display:inline; }
                input { background-color: #ecc; border:0;}
             |]
    
getMusicaR :: Handler Html
getMusicaR = do
             (widget, enctype) <- generateFormPost formMusicas
             defaultLayout $ do
             addStylesheet $ StaticR cadastromusica_css
             widgetFormCadastroMusica MusicaR enctype widget "Nova música"

getMusicaIdR :: MusicasId -> Handler Html
getMusicaIdR mid = do
             musica <- runDB $ get404 mid
             defaultLayout [whamlet| 
                 <h1> Música #{musicasNome musica}
             |]             

getAlbumLstR :: Handler Html
getAlbumLstR = do
             listaA <- runDB $ selectList [] [Asc AlbunsNome]
             defaultLayout $ do 
             addStylesheet $ StaticR listagemsimples_css
             [whamlet|
                <div class="container">
                    <h1> Álbuns cadastrados:
                    $forall Entity vid album <- listaA
                        <a href=@{AlbumIdR vid}> #{albunsNome album} 
                        <form method=post action=@{AlbumIdR vid}> 
                            <input type="submit" value="Deletar"><br>
             |] 
             toWidget [lucius|
                form  { display:inline; }
                input { background-color: #ecc; border:0;}
             |] 

getAlbumR :: Handler Html
getAlbumR = do
             (widget, enctype) <- generateFormPost formAlbuns
             defaultLayout $ do
             addStylesheet $ StaticR cadastrosimples_css
             widgetFormCadastroAlbum AlbumR enctype widget "Novo álbum"

getAlbumIdR :: AlbunsId -> Handler Html
getAlbumIdR aid = do
             album <- runDB $ get404 aid
             defaultLayout [whamlet| 
                 <h1> Álbum #{albunsNome album}
             |]             

getAlbumMusR :: Handler Html
getAlbumMusR = do
             (widget, enctype) <- generateFormPost formAlbunsMusicas
             defaultLayout $ do
             addStylesheet $ StaticR cadastrosimples_css
             widgetFormCadastroSimples AlbumMusR enctype widget "Novo álbum"

getCadastroR :: Handler Html
getCadastroR = do
             (widget, enctype) <- generateFormPost formPessoa
             defaultLayout $ do 
                 --addStylesheet $ StaticR teste_css
                 widgetForm CadastroR enctype widget "Pessoas"

getPessoaR :: PessoaId -> Handler Html
getPessoaR pid = do
             pessoa <- runDB $ get404 pid 
             dpto <- runDB $ get404 (pessoaDeptoid pessoa)
             defaultLayout [whamlet| 
                 <h1> Seja bem-vindx #{pessoaNome pessoa}
                 <p> Salario: #{pessoaSalario pessoa}
                 <p> Idade: #{pessoaIdade pessoa}
                 <p> Departamento: #{departamentoNome dpto}
             |]

getListarR :: Handler Html
getListarR = do
             listaP <- runDB $ selectList [] [Asc PessoaNome]
             defaultLayout $ do 
             [whamlet|
                 <h1> Pessoas cadastradas:
                 $forall Entity pid pessoa <- listaP
                     <a href=@{PessoaR pid}> #{pessoaNome pessoa} 
                     <form method=post action=@{PessoaR pid}> 
                         <input type="submit" value="Deletar"><br>
             |] 
             toWidget [lucius|
                form  { display:inline; }
                input { background-color: #ecc; border:0;}
             |]

postCadastroR :: Handler Html
postCadastroR = do
                ((result, _), _) <- runFormPost formPessoa
                case result of
                    FormSuccess pessoa -> do
                       runDB $ insert pessoa 
                       defaultLayout [whamlet| 
                           <h1> #{pessoaNome pessoa} Inseridx com sucesso. 
                       |]
                    _ -> redirect CadastroR

getDeptoR :: Handler Html
getDeptoR = do
             (widget, enctype) <- generateFormPost formDepto
             defaultLayout $ widgetForm DeptoR enctype widget "Departamentos"

postVertenteR :: Handler Html
postVertenteR = do
                ((result, _), _) <- runFormPost formVertentes
                case result of
                    FormSuccess vertente -> do
                       runDB $ insert vertente
                       defaultLayout [whamlet|
                           <h1> #{vertentesNome vertente} Inserido com sucesso. 
                       |]
                    _ -> redirect VertenteR

postArtistaR :: Handler Html
postArtistaR = do
                ((result, _), _) <- runFormPost formArtistas
                case result of
                    FormSuccess artista -> do
                       runDB $ insert artista
                       defaultLayout [whamlet|
                           <h1> #{artistasNome artista} Inserido com sucesso. 
                       |]
                    _ -> redirect ArtistaR


postMusicaR :: Handler Html
postMusicaR = do
                ((result, _), _) <- runFormPost formMusicas
                case result of
                    FormSuccess musica -> do
                       runDB $ insert musica
                       defaultLayout [whamlet|
                           <h1> #{musicasNome musica} Inserido com sucesso. 
                       |]
                    _ -> redirect MusicaR

postAlbumR :: Handler Html
postAlbumR = do
                ((result, _), _) <- runFormPost formAlbuns
                case result of
                    FormSuccess album -> do
                       runDB $ insert album
                       defaultLayout [whamlet|
                           <h1> #{albunsNome album} Inserido com sucesso. 
                       |]
                    _ -> redirect AlbumR

postAlbumMusR :: Handler Html
postAlbumMusR = do
                ((result, _), _) <- runFormPost formAlbunsMusicas
                case result of
                    FormSuccess albummsc -> do
                       runDB $ insert albummsc
                       defaultLayout [whamlet|
                           <h1> Inserido com sucesso. 
                       |]
                    _ -> redirect AlbumMusR

postDeptoR :: Handler Html
postDeptoR = do
                ((result, _), _) <- runFormPost formDepto
                case result of
                    FormSuccess depto -> do
                       runDB $ insert depto
                       defaultLayout [whamlet|
                           <h1> #{departamentoNome depto} Inserido com sucesso. 
                       |]
                    _ -> redirect DeptoR

postArtistaIdR :: ArtistasId -> Handler Html
postArtistaIdR pid = do
     runDB $ delete pid
     redirect ArtistaR

postAlbumIdR :: AlbunsId -> Handler Html
postAlbumIdR pid = do
     runDB $ delete pid
     redirect AlbumR

postVertenteIdR :: VertentesId -> Handler Html
postVertenteIdR pid = do
     runDB $ delete pid
     redirect VertenteR

postMusicaIdR :: MusicasId -> Handler Html
postMusicaIdR mid = do
     runDB $ delete mid
     redirect MusicaR

postPessoaR :: PessoaId -> Handler Html
postPessoaR pid = do
     runDB $ delete pid
     redirect ListarR