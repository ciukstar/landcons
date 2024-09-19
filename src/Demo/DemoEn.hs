{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Demo.DemoEn (fillDemoEn) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader (ReaderT)

import Database.Persist (PersistStoreWrite (insert, insert_))
import Database.Persist.SqlBackend (SqlBackend)

import Model
    ( Role(Role)
    , User (User, userEmail, userPassword, userRole)
    , Site (Site, siteName, siteDescr, siteHome)
    , Webpage (Webpage, webpageTitle, webpageFooter, webpageSite)
    , Item (itemName, itemDescr, itemPrice, itemCurrency, itemRating, itemLink, Item)
    , ItemPhoto (itemPhotoItem, ItemPhoto, itemPhotoMime, itemPhotoPhoto, itemPhotoAttribution)
    , DocHeader (DocHeader, docHeaderPage, docHeaderContentsType, docHeaderHtml, docHeaderLevel)
    , ContentsType (ContentsTypeText)
    , HeadingLevel (HeadingLevelH1)
    , DocBody (DocBody, docBodyPage, docBodyBgColor, docBodyLayout)
    , DisplayLayout (DisplayLayoutTable)
    , Product (Product, productDisplay, productItem)
    )
    
import Text.Hamlet (shamlet)

import Yesod.Auth.Email (saltPass)
import qualified Data.ByteString as BS


fillDemoEn :: MonadIO m => ReaderT SqlBackend m ()
fillDemoEn = do

    let freepik = [shamlet|
                          Designed by #
                          <a href="https://www.freepik.com/" target=_blank>
                            Freepik
                          |]
    
    r1 <- insert $ Role "Администратор"

    r2 <- insert $ Role "Пользователь"

    pass1 <- liftIO $ saltPass "admin"
    uid1 <- insert $ User { userEmail = "admin@mail.ru"
                          , userPassword = Just pass1
                          , userRole = r1
                          }

    pass2 <- liftIO $ saltPass "user1"
    uid2 <- insert $ User { userEmail = "user1@mail.ru"
                          , userPassword = Just pass2
                          , userRole = r2
                          }

    pass3 <- liftIO $ saltPass "oleg"
    uid3 <- insert $ User { userEmail = "oleg@mail.ru"
                          , userPassword = Just pass3
                          , userRole = r1
                          }

    item1 <- insert $ Item { itemName = "Boots"
                           , itemDescr = Nothing
                           , itemPrice = 10
                           , itemCurrency = "usd"
                           , itemRating = Just 5
                           , itemLink = Just "https://www.google.com/search?q=boots&newwindow=1&sca_esv=d0c9352aa5174606&sca_upv=1&udm=2&sxsrf=ADLYWIKsKqVyh8lU5t_ZkIz6OfvOuHw2LQ:1726700988986&source=lnms&fbs=AEQNm0BNfNUooeQ9zNOHfD5dM-cuenhkgmBhnYUHrz76pe-e8wZDv0RMPHSRkHCNsZbIP5HzHYUD1XUTm-HLqxdFxra0L7ZkGto12nYXFok_FFPnEp1mJMBgsJboGp1vcvDK6HJsWCHzVgb0MD9onKVKCLcfkz3j3O3efOUHtwoipmPXsfidnf4&sa=X&ved=2ahUKEwiVrNO4zs2IAxWPRPEDHZlLAT0Q_AUoAnoECAQQBA&biw=1536&bih=744&dpr=1.25"
                           }
             
    liftIO (BS.readFile "demo/item_1.avif") >>= \bs ->
      insert_ ItemPhoto { itemPhotoItem = item1
                        , itemPhotoMime = "image/avif"
                        , itemPhotoPhoto = bs
                        , itemPhotoAttribution = Just freepik
                        }

    item2 <- insert $ Item { itemName = "Shirts"
                           , itemDescr = Nothing
                           , itemPrice = 20.89
                           , itemCurrency = "usd"
                           , itemRating = Just 5
                           , itemLink = Just "https://www.google.com/search?q=shirts&newwindow=1&sca_esv=d0c9352aa5174606&sca_upv=1&udm=2&biw=1536&bih=744&sxsrf=ADLYWILaj7Jh_HmBRUhl2wy96h7y86EOHQ%3A1726700991047&ei=v13rZsPJAruQxc8PieXGwAg&ved=0ahUKEwiDpdG5zs2IAxU7SPEDHYmyEYgQ4dUDCBA&uact=5&oq=shirts&gs_lp=Egxnd3Mtd2l6LXNlcnAiBnNoaXJ0czIFEAAYgAQyBRAAGIAEMgUQABiABDIFEAAYgAQyBRAAGIAEMgUQABiABDIFEAAYgAQyBRAAGIAEMgUQABiABDIFEAAYgARIgiZQwA5YtR1wAXgAkAEAmAFioAGzBKoBATa4AQPIAQD4AQGYAgegApIFwgIEECMYJ8ICChAAGIAEGEMYigWYAwCIBgGSBwMxLjagB7Ae&sclient=gws-wiz-serp"
                           }
             
    liftIO (BS.readFile "demo/item_2.avif") >>= \bs ->
      insert_ ItemPhoto { itemPhotoItem = item2
                        , itemPhotoMime = "image/avif"
                        , itemPhotoPhoto = bs
                        , itemPhotoAttribution = Just freepik
                        }


    sid1 <- insert $ Site { siteName = "Site #1"
                          , siteDescr = Nothing
                          , siteHome = Nothing
                          }

    sid2 <- insert $ Site { siteName = "Site #2"
                          , siteDescr = Nothing
                          , siteHome = Nothing
                          }


    pid11 <- insert $ Webpage { webpageSite = sid1
                              , webpageTitle = "Sales #1"
                              , webpageFooter = "Footer 1"
                              }

    h111 <- insert $ DocHeader { docHeaderPage = pid11
                               , docHeaderContentsType = ContentsTypeText
                               , docHeaderHtml = Just "Marketplace #1"
                               , docHeaderLevel = Just HeadingLevelH1
                               }

    b111 <- insert $ DocBody { docBodyPage = pid11
                             , docBodyBgColor = Just "white"
                             , docBodyLayout = Just DisplayLayoutTable
                             }

    insert_ $ Product { productItem = item1
                      , productDisplay = b111
                      }

    insert_ $ Product { productItem = item2
                      , productDisplay = b111
                      }


    pid12 <- insert $ Webpage { webpageSite = sid1
                              , webpageTitle = "Sales #2"
                              , webpageFooter = "Footer 2"
                              }
    

    return ()
