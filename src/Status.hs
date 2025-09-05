{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Status where

import Database.Persist.TH
import Prelude
import ClassyPrelude (Text)

data StatusVeicular = Aguardando | Oficina | Teste | Lavagem | Completo | Retirado
    deriving (Show, Read, Eq)
derivePersistField "StatusVeicular"

ordemStatusToText :: StatusVeicular -> Text
ordemStatusToText status =
    case status of
        Aguardando -> "Aguardando"
        Oficina -> "Oficina"
        Teste -> "Teste"
        Lavagem -> "Lavagem"
        Completo -> "Completo"
        Retirado -> "Retirado por cliente"
