{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Pagina utilizada para ver todos os carros
module Handler.Dashboard where

import Import
import Status
import Model

import Database.Persist.Sql



getDashR :: Handler Html
getDashR = do
    veiculos <- runDB $ selectList [OrdemServicoStatus !=. Retirado ] []
    defaultLayout $ do
        setTitle "Visão Geral"
        $(widgetFile "dashboard")