{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- | Pagina que mostra um QRcode
module Handler.Qrcode where

import Import
import Codec.QRCode
import Codec.QRCode.JuicyPixels

import Text.Blaze ( unsafeLazyByteString )
import Database.Persist.Sql (toSqlKey)



getQrcodeR :: Int64 -> Handler Html
getQrcodeR ordemId = do
    render <- getUrlRender
    ordem <- runDB $ get404 $ toSqlKey ordemId
    defaultLayout $ do
        let 
            options = QRCodeOptions{
            qroMinVersion = 3,
            qroMaxVersion = 7,
            qroErrorLevel = H,
            qroBoostErrorLevel = False,
            qroMask = Nothing
            }
            encoding = Utf8WithoutECI
            borda = 4 -- ^ em pixels
            escala = 8 -- ^ Multiplicador de escala do qrcode
            placa =  ordemServicoPlaca ordem
            linkOrdem = render (ScanR ordemId)
        

        
        --Apesar de ser unsafe, vamos estufar este valor em um tag do tipo <img> então não deve importar
        let mcodeParaPlaca = unsafeLazyByteString . toPngDataUrlBS borda escala <$> encodeText options encoding linkOrdem
        setTitle "QR code gerado"
        $(widgetFile "qrcode")

