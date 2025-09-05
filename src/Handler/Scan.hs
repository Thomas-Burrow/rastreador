{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Aqui será feito checkin e checkout
module Handler.Scan where

import Import
import Yesod.Form.Bootstrap3
import Status
import Database.Persist.Sql (toSqlKey)
import Data.Time.Clock.POSIX (getPOSIXTime)

data TiposEstado = EstadoOficina | EstadoTeste | EstadoLavagem
    deriving (Show, Eq, Read)
data Operation  = CheckIn TiposEstado | CheckOut TiposEstado | Corrigir TiposEstado| Concluir | MarcarRetirado
    deriving (Show, Eq, Read)

--TODO: após authn/authz adicionar usuario
data ScanData = ScanData {
    timeStamp :: UTCTime
    }

-- | Pagina com detalhes do veiculo e checkin/checkout etc.
getScanR :: Int64 -> Handler Html
getScanR ordemID = do
    veiculo :: OrdemServico <- runDB $ get404 $ toSqlKey ordemID
    --Aqui vão ser decididas quais operações mostraremos na pagina
    let placa = ordemServicoPlaca veiculo

    defaultLayout $ do
        (formWidget, formEnctype) <- handlerToWidget $ generateFormPost $ scanForm
        setTitle "Status do Veículo"
        $(widgetFile "scan")
        let
            statusAtual = ordemServicoStatus veiculo
            lavado = ordemServicoLavagemCompleta veiculo
            testado = ordemServicoTesteCompleto veiculo
            inspecionado = ordemServicoOficinaCompleta veiculo
            in case statusAtual of
                Aguardando -> do
                    --Usa inputs com nomes no .hamlet
                    --agora vamos decidir quais destes botões são visiveis
                    let oficinaVisivel = not inspecionado
                        testeVisivel = not testado
                        lavagemVisivel = not lavado
                        concluirPossivel = inspecionado && testado && lavado
                        in $(widgetFile "scan-checkin")
                Oficina ->
                    do
                        $(widgetFile "scan-checkout")
                Teste ->
                    do
                        $(widgetFile "scan-checkout")
                Lavagem ->
                    do
                        $(widgetFile "scan-checkout")
                Completo ->
                    do
                        $(widgetFile "scan-completo")
                Retirado ->
                    $(widgetFile "scan-retirado")

scanForm :: Form ScanData
scanForm =
    renderBootstrap3 BootstrapBasicForm $ ScanData
    <$> lift (liftIO getCurrentTime)

-- | Um pouco mais complicado, aqui temos a logica de maquina de estados
postScanR :: Int64 -> Handler Html
postScanR ordemID = do
    veiculo :: OrdemServico <- runDB $ get404 $ toSqlKey ordemID
    --Leremos os forms dependendo do estado atual
    res <- runFormPost scanForm
    let
        statusAtual = ordemServicoStatus veiculo
        lavado = ordemServicoLavagemCompleta veiculo
        testado = ordemServicoTesteCompleto veiculo
        inspecionado = ordemServicoOficinaCompleta veiculo
        in case statusAtual of
            Aguardando -> do
                --usar runFormPost quando authz para obter usuario, tempo, etc
                --gambiarra detectada
                isOficina <- runInputPost $ iopt textField "checkinOficina"
                isTeste <- runInputPost $ iopt textField "checkinTeste"
                isLavagem <- runInputPost $ iopt textField "checkinLavagem"
                isConcluir <- runInputPost $ iopt textField "concluir"
                isCorrigirOficina <- runInputPost $ iopt textField "corrigirOficina"
                isCorrigirTeste <- runInputPost $ iopt textField "corrigirTeste"
                isCorrigirLavagem <- runInputPost $ iopt textField "corrigirLavagem"
                when (isJust isOficina) $ do
                    runDB $ update (toSqlKey ordemID) [OrdemServicoStatus =. Oficina]
                    setMessage "Checkin realizado para oficina."
                when (isJust isTeste) $ do
                    runDB $ update (toSqlKey ordemID) [OrdemServicoStatus =. Teste]
                    setMessage "Checkin realizado para testagem."
                when (isJust isLavagem) $ do
                    runDB $ update (toSqlKey ordemID) [OrdemServicoStatus =. Lavagem]
                    setMessage "Checkin realizado para lavagem."
                when (isJust isCorrigirOficina) $ do
                    runDB $ update (toSqlKey ordemID) [OrdemServicoOficinaCompleta =. False]
                    setMessage "Oficina resetada."
                when (isJust isCorrigirTeste) $ do
                    runDB $ update (toSqlKey ordemID) [OrdemServicoTesteCompleto =. False]
                    setMessage "Testagem resetada."
                when (isJust isCorrigirLavagem) $ do
                    runDB $ update (toSqlKey ordemID) [OrdemServicoLavagemCompleta =. False]
                    setMessage "Lavagem resetada."
                when (lavado && testado && inspecionado  && isJust isConcluir) $ do
                    runDB $ update (toSqlKey ordemID) [OrdemServicoStatus =. Completo]
                    setMessage "Marcado como concluido"
            --Fim do maior caso Aguardando
            Oficina -> do
                isCheckout <- runInputPost $ iopt textField "checkout"
                when (isJust isCheckout) $ do
                    runDB $ update (toSqlKey ordemID) [OrdemServicoStatus =. Aguardando, OrdemServicoOficinaCompleta =. True]
                    setMessage "Checkout para oficina realizado."
            Teste -> do
                isCheckout <- runInputPost $ iopt textField "checkout"
                when (isJust isCheckout) $ do
                    runDB $ update (toSqlKey ordemID) [OrdemServicoStatus =. Aguardando, OrdemServicoTesteCompleto =. True]
                    setMessage "Checkout para teste realizado."
            Lavagem -> do
                isCheckout <- runInputPost $ iopt textField "checkout"
                when (isJust isCheckout) $ do
                    runDB $ update (toSqlKey ordemID) [OrdemServicoStatus =. Aguardando,  OrdemServicoLavagemCompleta =. True]
                    setMessage "Checkout para lavagem realizado."
            Completo -> do
                isRetirar <- runInputPost $ iopt textField "retirar"
                when (isJust isRetirar) $ do
                    runDB $ update (toSqlKey ordemID) [OrdemServicoStatus =. Retirado]
                    setMessage "Veicula marcado como retirado."
            Retirado -> do
                setMessage "Operação Invalida"

    --TODO: adicionar entradas para um log de eventos em um banco de dados

    redirect (ScanR ordemID) --refresh para mostrar nossos novos dados

getUnixTime :: IO UTCTime
getUnixTime = getCurrentTime