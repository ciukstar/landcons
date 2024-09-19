{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Header (getHeaderR, postHeaderR) where


import Control.Monad (void)

import Data.Maybe (isJust)
import Data.Text (Text)

import Database.Esqueleto.Experimental
    (selectOne, from, table)
import Database.Persist
    ( Entity, entityVal, upsertBy, (=.))

import Foundation
    ( Handler, Form, widgetTopbar, widgetSnackbar
    , Route (DataR, PageR)
    , DataR (WebpageR, WebpagesR, HeaderR, WebpageEditR, BodyR)
    , AppMessage
      ( MsgHeader, MsgText, MsgHtml, MsgCancel, MsgSave, MsgDetails, MsgBody
      , MsgFooter, MsgContentType, MsgContents, MsgHeadingLevel, MsgRecordEdited
      , MsgPage
      )
    )

import Model
    ( msgSuccess
    , ContentsType (ContentsTypeText, ContentsTypeHtml)
    , SiteId, WebpageId
    , DocHeader (DocHeader, docHeaderContentsType, docHeaderHtml, docHeaderLevel)
    , HeadingLevel
      ( HeadingLevelH1, HeadingLevelH2, HeadingLevelH3, HeadingLevelH4
      , HeadingLevelH5, HeadingLevelH6
      )
    , Unique (UniqueDocHeader)
    , EntityField (DocHeaderContentsType, DocHeaderHtml, DocHeaderLevel)
    )

import Text.Hamlet (Html)
import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, SomeMessage (SomeMessage), addMessageI
    , redirect, getMessages, newIdent, getMessageRender
    )
import Settings (widgetFile)
import Yesod.Form.Functions (generateFormPost, mreq, mopt, runFormPost)
import Yesod.Persist.Core (YesodPersist(runDB))
import Yesod.Form.Fields (selectField, optionsPairs, htmlField)
import Yesod.Form.Types
    ( FieldView (fvInput, fvLabel, fvErrors, fvRequired)
    , FieldSettings (fsLabel, fsTooltip, fsId, fsName, fsAttrs, FieldSettings)
    , FormResult (FormSuccess)
    )


postHeaderR :: SiteId -> WebpageId -> Handler Html
postHeaderR sid pid = do

    header <- runDB $ selectOne $ from $ table @DocHeader
    
    ((fr,fw),et) <- runFormPost $ formHeader pid header

    case fr of
      FormSuccess r@(DocHeader _ ct html level) -> do
          void $ runDB $ upsertBy (UniqueDocHeader pid) r
              [ DocHeaderContentsType =. ct
              , DocHeaderHtml =. html
              , DocHeaderLevel =. level
              ]
          addMessageI msgSuccess MsgRecordEdited
          redirect $ DataR $ HeaderR sid pid
          
      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgHeader
              idOverlay <- newIdent
              idDialogDelete <- newIdent
              $(widgetFile "data/header/header")


getHeaderR :: SiteId -> WebpageId -> Handler Html
getHeaderR sid pid = do

    header <- runDB $ selectOne $ from $ table @DocHeader
    
    (fw,et) <- generateFormPost $ formHeader pid header
    
    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgHeader
        idOverlay <- newIdent
        idDialogDelete <- newIdent
        $(widgetFile "data/header/header")


formHeader :: WebpageId -> Maybe (Entity DocHeader) -> Form DocHeader
formHeader pid header extra = do
    
    (typeR,typeV) <- mreq (selectField typeOptions) FieldSettings
        { fsLabel = SomeMessage MsgContentType
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (docHeaderContentsType . entityVal <$> header)
    
    (htmlR,htmlV) <- mopt htmlField FieldSettings
        { fsLabel = SomeMessage MsgContents
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (docHeaderHtml . entityVal <$> header)
    
    (levelR,levelV) <- mopt (selectField levelOptions) FieldSettings
        { fsLabel = SomeMessage MsgHeadingLevel
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (docHeaderLevel . entityVal <$> header)
    
    let r = DocHeader pid <$> typeR <*> htmlR <*> levelR
    let w = $(widgetFile "data/header/form")
    return (r,w)
  where

      levelOptions = optionsPairs [ ("h1" :: Text, HeadingLevelH1),("h2", HeadingLevelH2)
                                  , ("h3", HeadingLevelH3),("h4", HeadingLevelH4)
                                  , ("h5", HeadingLevelH5),("h6", HeadingLevelH6)
                                  ]
      
      typeOptions = optionsPairs [(MsgText, ContentsTypeText),(MsgHtml,ContentsTypeHtml)]
      
