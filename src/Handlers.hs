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

formVertentes :: Form Vertentes
formVertentes = renderDivs $ Vertentes <$>
                areq textField "Nome: " Nothing  <*>
                areq (selectField vts) "Vertente Mãe: "  Nothing

art = do
    entidades <- runDB $ selectList [] [Asc ArtistasNome] 
    optionsPairs $ fmap (\ent -> (artistasNome $ entityVal ent, entityKey ent)) entidades

formArtistas :: Form Artistas
formArtistas = renderDivs $ Artistas <$>
                areq textField "Nome: " Nothing

formMusicas :: Form Musicas
formMusicas = renderDivs $ Musicas <$>
                areq textField "Nome: " Nothing  <*>
                areq (selectField art) "Artista principal: "  Nothing

formAlbuns :: Form Albuns
formAlbuns = renderDivs $ Albuns <$>
                areq textField "Nome: " Nothing  <*>
                areq (selectField art) "Artista principal: "  Nothing
            

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
             defaultLayout $ widgetFormCadastroSimples VertenteR enctype widget "Vertentes"


getArtistaR :: Handler Html
getArtistaR = do
             (widget, enctype) <- generateFormPost formArtistas
             defaultLayout $ widgetFormCadastroSimples ArtistaR enctype widget "Artistas"

getMusicaR :: Handler Html
getMusicaR = do
             (widget, enctype) <- generateFormPost formMusicas
             defaultLayout $ widgetFormCadastroSimples MusicaR enctype widget "Musicas"
             

getAlbumR :: Handler Html
getAlbumR = do
             (widget, enctype) <- generateFormPost formAlbuns
             defaultLayout $ widgetFormCadastroSimples AlbumR enctype widget "Albuns"


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

postPessoaR :: PessoaId -> Handler Html
postPessoaR pid = do
     runDB $ delete pid
     redirect ListarR