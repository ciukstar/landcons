{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Sites
  ( getSitesR, postSitesR
  , getSiteR, postSiteR
  , getSiteNewR, getSiteEditR, postSiteDeleR
  ) where

import Data.Maybe (isJust)

import Database.Esqueleto.Experimental
    ( select, from, table, orderBy, asc, selectOne, where_, val
    , (^.), (==.)
    )
import Database.Persist
    ( Entity (Entity), entityVal, insert_, replace, delete, SelectOpt (Asc)
    , selectFirst
    )
import qualified Database.Persist as P ((==.))


import Foundation
    ( Handler, Form, widgetTopbar, widgetSnackbar
    , Route (DataR, SiteHomeR, PageR)
    , DataR (SitesR, SiteR, SiteNewR, SiteEditR, SiteDeleR, WebpagesR)
    , AppMessage
      ( MsgSites, MsgSite, MsgName, MsgDescription, MsgConfirmPlease
      , MsgDeleteAreYouSure, MsgCancel, MsgDele, MsgSave, MsgRecordAdded
      , MsgRecordEdited, MsgInvalidFormData, MsgRecordDeleted, MsgDetails
      , MsgPages, MsgPreview, MsgHomepage, MsgUrl
      )
    )

import Model
    ( msgSuccess, msgError
    , SiteId, Site(Site, siteName, siteDescr, siteHome)
    , Webpage (Webpage)
    , EntityField (SiteName, SiteId, WebpageSite, WebpageId)
    )

import Settings (widgetFile)

import Text.Hamlet (Html)
import Text.Shakespeare.I18N (SomeMessage (SomeMessage))

import Yesod.Core (Yesod(defaultLayout), getMessageRender, liftHandler)
import Yesod.Core.Handler
    (getMessages, newIdent, addMessageI, redirect)
import Yesod.Core.Widget (whamlet)
import Yesod.Form.Fields (textField, textareaField, selectField, optionsPairs)
import Yesod.Form.Functions (mreq, mopt, generateFormPost, runFormPost)
import Yesod.Form.Types
    ( FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    , FormResult (FormSuccess)
    , fvInput, fvLabel, fvErrors, fvRequired
    )
import Yesod.Persist.Core (runDB)


postSiteDeleR :: SiteId -> Handler Html
postSiteDeleR sid = do
    ((fr,_),_) <- runFormPost formSiteDelete
    case fr of
      FormSuccess () -> do
          runDB $ delete sid
          addMessageI msgSuccess MsgRecordDeleted
          redirect $ DataR SitesR
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ DataR $ SiteR sid


formSiteDelete :: Form ()
formSiteDelete extra = return (pure (), [whamlet|#{extra}|])


postSiteR :: SiteId -> Handler Html
postSiteR sid = do

    site <- runDB $ selectOne $ do
        x <- from $ table @Site
        where_ $ x ^. SiteId ==. val sid
        return x

    ((fr,fw),et) <- runFormPost $ formSite site
    case fr of
      FormSuccess r -> do
          runDB $ replace sid r
          addMessageI msgSuccess MsgRecordEdited
          redirect $ DataR $ SiteR sid
      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              idOverlay <- newIdent
              $(widgetFile "data/sites/edit")


getSiteEditR :: SiteId -> Handler Html
getSiteEditR sid = do

    site <- runDB $ selectOne $ do
        x <- from $ table @Site
        where_ $ x ^. SiteId ==. val sid
        return x

    (fw,et) <- generateFormPost $ formSite site

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        idOverlay <- newIdent
        $(widgetFile "data/sites/edit")


postSitesR :: Handler Html
postSitesR = do

    ((fr,fw),et) <- runFormPost $ formSite Nothing

    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI msgSuccess MsgRecordAdded
          redirect $ DataR SitesR
      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              idOverlay <- newIdent
              $(widgetFile "data/sites/new")



getSiteNewR :: Handler Html
getSiteNewR = do

    (fw,et) <- generateFormPost $ formSite Nothing

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        idOverlay <- newIdent
        $(widgetFile "data/sites/new")


formSite :: Maybe (Entity Site) -> Form Site
formSite site extra = do

    (nameR,nameV) <- mreq textField FieldSettings
        { fsLabel = SomeMessage MsgName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (siteName . entityVal <$> site)

    (descrR,descrV) <- mopt textareaField FieldSettings
        { fsLabel = SomeMessage MsgDescription
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (siteDescr . entityVal <$> site)

    pages <- liftHandler $ runDB $ select $ do
        x <- from $ table @Webpage
        case site of
          Just (Entity sid _) -> where_ $ x ^. WebpageSite ==. val sid
          Nothing -> where_ $ val False
        orderBy [asc (x ^. WebpageId)]
        return x
        
    let homeOptions = optionsPairs ((\(Entity pid (Webpage _ name _)) -> (name,pid)) <$> pages)

    (homeR,homeV) <- mopt (selectField homeOptions) FieldSettings
        { fsLabel = SomeMessage MsgHomepage
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (siteHome . entityVal <$> site)

    let r = Site <$> nameR <*> descrR <*> homeR
    let w = $(widgetFile "data/sites/form")
    return (r,w)



getSiteR :: SiteId -> Handler Html
getSiteR sid = do

    site <- runDB $ selectOne $ do
        x <- from $ table @Site
        where_ $ x ^. SiteId ==. val sid
        return x

    page <- runDB $ selectFirst [WebpageSite P.==. sid] [Asc WebpageId]

    (fw0,et0) <- generateFormPost formSiteDelete

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        idOverlay <- newIdent
        idDialogDelete <- newIdent
        $(widgetFile "data/sites/site")
    

getSitesR :: Handler Html
getSitesR = do

    sites <- runDB $ select $ do
        x <- from $ table @Site
        orderBy [asc (x ^. SiteName)]
        return x

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        idOverlay <- newIdent
        $(widgetFile "data/sites/sites")
