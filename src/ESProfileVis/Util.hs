{-# LANGUAGE OverloadedStrings #-}

module ESProfileVis.Util where

import Control.Monad ((<=<), join)
import Data.List (init, isSuffixOf)
import Data.Maybe (fromJust)

import Data.Aeson (FromJSON, decode, encode)
import qualified Data.Aeson as Aeson
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Data.ByteString.Builder (toLazyByteString)
import Data.HashMap.Strict as H
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8Builder)
import Network.URI (URI(..))
import Network.HTTP (simpleHTTP, postRequestWithBody, getResponseCode, getResponseBody)
import qualified Network.HTTP as HTTP 

import ESProfileVis.Types



decodeText :: FromJSON a => Text -> Maybe a
decodeText = decode . toLazyByteString . encodeUtf8Builder

decodeString :: FromJSON a => String -> Maybe a
decodeString = decode . pack

encodeString = unpack . encode

emptyNothing :: [a] -> Maybe [a]
emptyNothing [] = Nothing
emptyNothing xs = Just xs


runProfile :: ElasticForm -> IO (Maybe Response)
runProfile form =
    let uri = fmap addSearchPath (server form)
        body = encodeString <$> (searchJSON form >>= decodeString >>= setProfile)
        rq = postRequestWithBody <$> uri <*> pure "application/json" <*> body
    in join <$> traverse (buildResp <=< simpleHTTP) rq
    where
        addSearchPath base = stripTrailingSlash base ++ searchPath (index form) (indexType form)

        stripTrailingSlash base
            | "/" `isSuffixOf` base = init base
            | otherwise = base

        searchPath Nothing Nothing = "/_search"
        searchPath (Just ix) Nothing = "/" ++ ix ++ "/_search"
        searchPath Nothing (Just ty) = "/_search"
        searchPath (Just ix) (Just ty) = "/" ++ ix ++ "/" ++ ty ++ "/_search"

        setProfile (Aeson.Object obj) = Just $ Aeson.Object $ H.insert "profile" (Aeson.Bool True) obj
        setProfile _ = Nothing

        buildResp rsp = do
            code <- getResponseCode rsp
            if code /= (2, 0, 0)
            then return Nothing
            else decode <$> pack <$> getResponseBody rsp
            