{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Text.Lazy (Text)

import Lucid (renderText)
import Web.Scotty
import Network.Wai.Handler.Warp (defaultSettings, setBeforeMainLoop)
import Web.Browser (openBrowser)
import Data.Aeson (decode)
import Network.URI (parseURI)
import Network.HTTP.Types (status403)
import Network.Wai
import Network.Wai.Middleware.Static (staticPolicy, hasPrefix)
import Network.Wai.Middleware.Local (local)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

import ESProfileVis.Types
import ESProfileVis.Util (runProfile)
import ESProfileVis.View (view)

main :: IO ()
main = scottyOpts scottyOptions $ do
    middleware $ logStdoutDev
    middleware $ staticPolicy (hasPrefix "static")
    middleware $ local $ responseBuilder status403 [] mempty

    get "/" $
        html (renderApp (ElasticForm Nothing Nothing Nothing Nothing) Nothing) 
    post "/" $ do
        form <- getForm
        response <- liftIO $ runProfile form
        html (renderApp form response)


    where
        scottyOptions = Options {
            verbose = 0,
            settings = setBeforeMainLoop beforeMainLoop defaultSettings
        }
        beforeMainLoop = do
            putStrLn "Opening app at http://localhost:3000"
            openBrowser "http://localhost:3000"
            return ()
        
        getForm = ElasticForm
            <$> maybeParam "server"
            <*> maybeParam "index"
            <*> maybeParam "type"
            <*> maybeParam "json"
        
        renderApp f r = renderText (view f r)

        maybeParam name = rescue (Just <$> param name) $ \_ -> return Nothing
