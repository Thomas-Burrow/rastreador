{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Pagina utilizada para criar ordens de serviço
module Handler.Ordem where

import Import
import Yesod.Form.Bootstrap3

import Status

import Database.Persist.Sql (toSqlKey, fromSqlKey)
import Model

-- | Utilizado para receber o form de pagina de ordens
-- | provavelmente vai ser expandido no futuro
data OrdemData = OrdemData
    { placaCampo :: Text -- ^ Placa do veiculo a ser registrada
    }


ordemForm :: Form OrdemData
ordemForm = renderBootstrap3 BootstrapBasicForm $ OrdemData
    <$> areq textField textSettings Nothing
    -- Add attributes like the placeholder and CSS classes.
    where textSettings = FieldSettings
            { fsLabel = "Placa do veículo"
            , fsTooltip = Nothing
            , fsId = Just "placaField"
            , fsName = Nothing
            , fsAttrs =
                [ ("class", "form-control")
                , ("placeholder", "XYZ-1234")
                ]
            }

getOrdemR :: Handler Html
getOrdemR = do
    (formWidget, formEnctype) <- generateFormPost ordemForm
    defaultLayout $ do
        setTitle "Criar Ordem de Serviço"
        $(widgetFile "ordem")


postOrdemR :: Handler Html
postOrdemR = do
    ((result, formWidget), formEnctype) <- runFormPost ordemForm
    case result of
            FormSuccess res ->
                let ordem = OrdemServico {ordemServicoTesteCompleto=False,
                                    ordemServicoStatus=Aguardando, 
                                    ordemServicoPlaca= placaCampo res, 
                                    ordemServicoOficinaCompleta=False, 
                                    ordemServicoLavagemCompleta=False
                                    }
                in do
                    ordemid <- runDB $ insert ordem
                    setMessage "Ordem Criado"
                    redirect (QrcodeR (fromSqlKey ordemid))

            _ -> setMessage "Erro no processamento"

    defaultLayout $ do
        setTitle "Criar Ordem de Serviço"
        $(widgetFile "ordem")