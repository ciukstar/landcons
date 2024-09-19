{-# LANGUAGE TemplateHaskell #-}

module Handler.Users (getUsersR) where

import Foundation
    ( Handler
    , widgetTheme, widgetLang, widgetMainMenu, widgetMainMenuTrigger
    , Route (HomeR, LangR)
    , AppMessage
      ( MsgUsers 
      )
    )

import Settings (widgetFile)

import Text.Hamlet (Html)

import Yesod.Core (Yesod(defaultLayout), setTitleI, newIdent, getUrlRenderParams)


getUsersR :: Handler Html
getUsersR = do
    rndr <- getUrlRenderParams
    defaultLayout $ do
        setTitleI MsgUsers
        idOverlay <- newIdent
        idDialogMainMenu <- newIdent
        idArticleSensorMap <- newIdent
        idSensorMap <- newIdent
        $(widgetFile "data/users/users")
