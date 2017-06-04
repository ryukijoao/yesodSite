{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns #-}
module Foundation where
import Yesod
import Yesod.Static
import Data.Text
import Database.Persist.Postgresql
    ( ConnectionPool, SqlBackend, runSqlPool, runMigration )

data Sitio = Sitio {getStatic :: Static, connPool :: ConnectionPool }

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

Usuario
   nome Text
   email Text
   senha Text
   UniqueEmail email

Artistas
    nome Text sqltype=varchar(100)
    deriving Show

Vertentes
    nome Text sqltype=varchar(100)
    vertenteMae VertentesId
    deriving Show

Musicas
    nome Text sqltype=varchar(200)
    artistaPrincipalId ArtistasId
    deriving Show

MusicasArtistas
    musica MusicasId
    artista ArtistasId

MusicasVertentes
    musica MusicasId
    vertente ArtistasId

Albuns
    nome Text sqltype=varchar(100)
    artistaMae ArtistasId
    deriving Show

AlbunsArtistas
    descricao Text sqltype=varchar(300)
    album AlbunsId
    artista ArtistasId
    deriving Show
    
AlbunsMusicas
    albumId AlbunsId
    musicaId MusicasId
    disco Int default=1
    deriving Show
    
    
Departamento
   nome Text
   sigla Text sqltype=varchar(3)
   deriving Show

Pessoa
   nome Text
   idade Int
   salario Double
   deptoid DepartamentoId
   deriving Show

|]

staticFiles "static"

mkYesodData "Sitio" $(parseRoutesFile "config/routes")

mkMessage "Sitio" "messages" "pt-BR"

instance YesodPersist Sitio where
   type YesodPersistBackend Sitio = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool

instance Yesod Sitio where
    authRoute _ = Just LoginR
    isAuthorized LoginR _ = return Authorized
    isAuthorized HomeR _ = return Authorized
    isAuthorized UsuarioR _ = isAdmin
    isAuthorized AdminR _ = isAdmin
    isAuthorized _ _ = isUser

isAdmin = do
    mu <- lookupSession "_USER"
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just "admin" -> Authorized
        Just _ -> Unauthorized "Soh o admin acessa aqui!"

isUser = do
    mu <- lookupSession "_USER"
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just _ -> Authorized

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance RenderMessage Sitio FormMessage where
    renderMessage _ _ = defaultFormMessage

widgetForm :: Route Sitio -> Enctype -> Widget -> Text -> Widget
widgetForm x enctype widget y = $(whamletFile "templates/form.hamlet")

widgetFormLogin :: Route Sitio -> Enctype -> Widget -> Text -> Widget
widgetFormLogin x enctype widget y = $(whamletFile "templates/login.hamlet")