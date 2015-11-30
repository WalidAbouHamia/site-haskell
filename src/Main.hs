{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns #-}
module Main where
import Database.Persist.Postgresql
import Control.Monad.Logger (runStdoutLoggingT)
import Data.Text (Text)
import Data.Time
import qualified Data.Text as T
import Control.Applicative
import Yesod
import Yesod.Static
import Text.Lucius

--import qualified Database.Esqueleto as E
--import Database.Esqueleto ((^.))
data Pagina = Pagina{connPool :: ConnectionPool,getStatic :: Static}

instance Yesod Pagina

staticFiles "static"

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Fornecedor
   nome Text
   deriving Show

Fruta
   nome Text sqltype=varchar(20)
   descricao Text
   estoque Int

Ordem
   fornId FornecedorId
   frutaId FrutaId
   qtde Int
   data UTCTime default=now()
   processado Bool
   UniqueFornFruta fornId frutaId
|]

mkYesod "Pagina" [parseRoutes|
  /fruta FrutaR GET POST
  /forncedor FornR GET POST
  /listfrut ListarFrutaR GET
  /listforn ListarFornR GET
  /ordem OrdemR GET POST
  / ListarOrdemR GET
  /static StaticR Static getStatic
|]
instance YesodPersist Pagina where
   type YesodPersistBackend Pagina = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance RenderMessage Pagina FormMessage where
    renderMessage _ _ = defaultFormMessage

formOrdem :: Form Ordem
formOrdem = renderDivs $ Ordem <$>
             areq (selectField forns) "Fornecedor" Nothing <*>
             areq (selectField frutas) "Fruta" Nothing <*>
             areq intField "Qtde" Nothing <*>
             lift (liftIO getCurrentTime) <*>
             lift (liftIO $ return False)

frutas = do
       entidades <- runDB $ selectList [] [Asc FrutaNome] 
       optionsPairs $ fmap (\ent -> (frutaNome $ entityVal ent, entityKey ent)) entidades

forns = do
       entidades <- runDB $ selectList [] [Asc FornecedorNome] 
       optionsPairs $ fmap (\ent -> (fornecedorNome $ entityVal ent, entityKey ent)) entidades

formFruta :: Form Fruta
formFruta = renderDivs $ Fruta <$>
             areq textField "Nome" Nothing <*>
             areq textField "Desc" Nothing <*>
             areq intField "Qtde Estoque" Nothing

formForn :: Form Fornecedor
formForn = renderDivs $ Fornecedor <$>
             areq textField "Nome" Nothing 

widgetForm :: Route Pagina -> Enctype -> Widget -> Text -> Widget
widgetForm x enctype widget y = [whamlet|
            <style>
             h1{
              background:red;
              color:white;
             }
              label{
                   color:white;
                   }
             body{
                background-color:black;
             }
             .botao {
                  font: bold 11px Arial;
                  text-decoration: none;
                  background-color: #EEEEEE;
                  color: #333333;
                  padding: 2px 6px 2px 6px;
                  border-top: 1px solid #CCCCCC;
                  border-right: 1px solid #333333;
                  border-bottom: 1px solid #333333;
                  border-left: 1px solid #CCCCCC;
                }
            <h1>
                Cadastro de #{y}
            <form method=post action=@{x} enctype=#{enctype}>
                ^{widget}
                <input class="botao" type="submit" value="Cadastrar">
                <a class="botao" href=@{ListarOrdemR}>Inicio</a>
|]


getFrutaR :: Handler Html
getFrutaR = do
           (widget, enctype) <- generateFormPost formFruta
           defaultLayout $ widgetForm FrutaR enctype widget "Frutas"

postFrutaR :: Handler Html
postFrutaR = do
            ((result,_),_) <- runFormPost formFruta
            case result of
                FormSuccess fruta -> (runDB $ insert fruta)  >> defaultLayout ( toWidget [hamlet|
                    <h1> Fruta inserida com Sucesso!!!
                    <center>
                     <a class="botao" href=@{ListarOrdemR}>Inicio</a>
                |] >> toWidget [lucius| 
                     h1{
                         background:red;
                         color:white;
                     }                     
                     body{
                      background-color:black;
                     }
                    .botao {
                      font: bold 11px Arial;
                      text-decoration: none;
                      background-color: #EEEEEE;
                      color: #333333;
                      padding: 2px 6px 2px 6px;
                      border-top: 1px solid #CCCCCC;
                      border-right: 1px solid #333333;
                      border-bottom: 1px solid #333333;
                      border-left: 1px solid #CCCCCC;
                    }
                 |])
                _ -> redirect FrutaR


getFornR :: Handler Html
getFornR = do
           (widget, enctype) <- generateFormPost formForn
           defaultLayout $ widgetForm FornR enctype widget "Fornecedores"

postFornR :: Handler Html
postFornR = do
            ((result,_),_) <- runFormPost formForn
            case result of
                FormSuccess forn -> (runDB $ insert forn) >> defaultLayout ( toWidget [hamlet|
                    <h1> Fornecedor inserido com sucesso!!
                    <center>
                     <a class="botao" href=@{ListarOrdemR}>Inicio</a>
                |] >> toWidget [lucius| 
                     h1{
                         background:red;
                         color:white;
                     }                     
                     body{
                      background-color:black;
                     }
                    .botao {
                      font: bold 11px Arial;
                      text-decoration: none;
                      background-color: #EEEEEE;
                      color: #333333;
                      padding: 2px 6px 2px 6px;
                      border-top: 1px solid #CCCCCC;
                      border-right: 1px solid #333333;
                      border-bottom: 1px solid #333333;
                      border-left: 1px solid #CCCCCC;
                    }
                 |])
                _ -> redirect FornR

getListarFrutaR :: Handler Html
getListarFrutaR = do
                 frutas <- runDB $ selectList [] [Asc FrutaNome]
                 defaultLayout ( toWidget [hamlet|
                      <h1> Lista de Frutas
                      $forall Entity pid pent <- frutas
                          <h3> #{frutaNome pent}
                      <center>
                       <a class="botao" href=@{ListarOrdemR}>Inicio</a>
                 |] >> toWidget [lucius| 
                     h1{
                         background:red;
                         color:white;
                     }
                     h3{
                        color:white;
                     } 
                     body{
                      background-color:black;
                     }
                    .botao {
                      font: bold 11px Arial;
                      text-decoration: none;
                      background-color: #EEEEEE;
                      color: #333333;
                      padding: 2px 6px 2px 6px;
                      border-top: 1px solid #CCCCCC;
                      border-right: 1px solid #333333;
                      border-bottom: 1px solid #333333;
                      border-left: 1px solid #CCCCCC;
                    }
                 |])
                
getListarFornR :: Handler Html
getListarFornR = do
                 forns <- runDB $ selectList [] [Asc FornecedorNome]
                 defaultLayout (toWidget [hamlet|
                      <h1> Lista de Fornecedores
                      $forall Entity fid fent <- forns
                          <h3> #{fornecedorNome fent}
                      <center>
                       <a class="botao" href=@{ListarOrdemR}>Inicio</a>
                       
                 |]>> toWidget [lucius| 
                     h1{
                         background:red;
                         color:white;
                     }
                     h3{
                        color:white;
                     } 
                     body{
                      background-color:black;
                     }
                    .botao {
                      font: bold 11px Arial;
                      text-decoration: none;
                      background-color: #EEEEEE;
                      color: #333333;
                      padding: 2px 6px 2px 6px;
                      border-top: 1px solid #CCCCCC;
                      border-right: 1px solid #333333;
                      border-bottom: 1px solid #333333;
                      border-left: 1px solid #CCCCCC;
                    }
                      

                 |])

getOrdemR :: Handler Html
getOrdemR = do
           (widget, enctype) <- generateFormPost formOrdem
           defaultLayout $ widgetForm OrdemR enctype widget "Ordens" 

postOrdemR :: Handler Html
postOrdemR = do
            ((result,_),_) <- runFormPost formOrdem
            case result of
                FormSuccess x -> (runDB $ insert x)  >> defaultLayout ( toWidget [hamlet|
                    <h1> Ordem inserida!!! 
                    <center>
                     <a class="botao" href=@{ListarOrdemR}>Inicio</a>
                |] >> toWidget [lucius| 
                     h1{
                         background:red;
                         color:white;
                     }                     
                     body{
                      background-color:black;
                     }
                    .botao {
                      font: bold 11px Arial;
                      text-decoration: none;
                      background-color: #EEEEEE;
                      color: #333333;
                      padding: 2px 6px 2px 6px;
                      border-top: 1px solid #CCCCCC;
                      border-right: 1px solid #333333;
                      border-bottom: 1px solid #333333;
                      border-left: 1px solid #CCCCCC;
                    }
                 |])
                _ -> redirect OrdemR

getListarOrdemR :: Handler Html
getListarOrdemR = do
                 ordens <- runDB $ (rawSql "SELECT ??, ?? \
                                   \FROM ordem INNER JOIN fruta \
                                   \ON ordem.fruta_id=fruta.id" [])::Handler [(Entity Ordem, Entity Fruta)]
                 defaultLayout (toWidget [hamlet|                      
                      <marquee>
                       <h1> Bem Vindo a Feira do Seu Alexandre !!!                      
                     <center>
                         <img src=@{StaticR feira_jpg}>
                         <br>
                         <a class="botao" href=@{FrutaR}>Cadastrar Fruta</a>
                         <a class="botao" href=@{ListarFrutaR}>Lista De Frutas</a>
                         <a class="botao" href=@{FornR}>Cadastrar Fornecedor</a>
                         <a class="botao" href=@{ListarFornR}>Lista De Fornecedores</a>
                         <a class="botao" href=@{OrdemR}>Ordem</a>
                         $forall (Entity oq _, Entity _ np) <- ordens
                          <p> Ordem #{fromSqlKey oq}: #{frutaNome np} | Descrição: #{frutaDescricao np}
                 |] >> toWidget [lucius| 
                     h1{
                         background:red;
                         
                     }
                    p{
                       color:white;
                        }
                     body{
                      background-color:black;
                     }
                    img{
                        width:400px;
                    }
                    .botao {
                      font: bold 11px Arial;
                      text-decoration: none;
                      background-color: #EEEEEE;
                      color: #333333;
                      padding: 2px 6px 2px 6px;
                      border-top: 1px solid #CCCCCC;
                      border-right: 1px solid #333333;
                      border-bottom: 1px solid #333333;
                      border-left: 1px solid #CCCCCC;
                    }
                      

                 |])
 

connStr = "dbname=d11g6ngf7p6tci host=ec2-107-21-219-201.compute-1.amazonaws.com user=ijagduiduelyqb password=DpOyAoCDTrFTeZaoIpaV3t9hCs port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do
       s <- static "static"
       runSqlPersistMPool (runMigration migrateAll) pool
       warpEnv (Pagina pool s)