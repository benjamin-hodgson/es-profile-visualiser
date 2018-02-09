{-# LANGUAGE OverloadedStrings #-}

module ESProfileVis.View (view) where

import Data.Foldable as F
import Data.List (sortBy)
import Data.Maybe (maybe, fromMaybe)
import Data.Ord (Down(..), comparing)

import Data.Aeson (encode)
import Data.Text (Text, pack)
import Data.Text.Lazy.Encoding (decodeUtf8)

import Lucid as H

import ESProfileVis.Types as Types

view :: ElasticForm -> Maybe Response -> Html ()
view f r = 
    doctypehtml_ $ do
        head_ $ do
            link_ [rel_ "stylesheet", H.type_ "text/css", href_ "static/main.css"]
            script_ [H.type_ "text/javascript", src_ "static/jquery-3.3.1.js"] ("" :: Html ())
            script_ [H.type_ "text/javascript", src_ "static/main.js"] ("" :: Html ())
        body_ $ do
            form f
            maybe (return ()) renderResponse r

form :: ElasticForm -> Html ()
form f =
    form_ [H.id_ "input-form", class_ "left", method_ "POST", action_ "/"] $ do
        input_ [name_ "server", placeholder_ "server url"] `withValue` fmap pack (server f)
        input_ [name_ "index", placeholder_ "index name"] `withValue` fmap pack (index f)
        input_ [name_ "type", placeholder_ "type name"] `withValue` fmap pack (indexType f)
        br_ []
        textarea_ [class_ "monospace input", name_ "json", placeholder_ "paste JSON here"] (toHtmlRaw $ fromMaybe "" (searchJSON f))
        br_ []
        button_ [H.type_ "submit", rel_ "noreferrer"] "Submit"


renderResponse :: Response -> Html ()
renderResponse rsp =
    let p = profile rsp
        shardsWithTime = sortBy (comparing (Down . snd)) [ (s, totalShardTime s) | s <- shards p ]
    in div_ [class_ "right"] $ ul_ $ traverse_ (uncurry renderShard) shardsWithTime

renderShard :: ShardProfile -> Integer -> Html ()
renderShard shard totalTime = do
    let queries = sortQueries (searches shard >>= query)
    li_ [class_ (nodeCls queries)] $ do
        span_ (toHtml $ Types.id_ shard)
        span_ [class_ "time"] $ renderTime totalTime
        ul_ $ traverse_ renderQuery queries

renderQuery :: QueryProfile -> Html ()
renderQuery query = do
    let queries = sortQueries $ childrenList query
    li_ [class_ (nodeCls queries), title_ (description query)] $ do
        span_ $ toHtml $ Types.type_ query
        span_ [class_ "time"] $ renderTime (time_in_nanos query)
        ul_ $ traverse_ renderQuery queries


sortQueries :: [QueryProfile] -> [QueryProfile]
sortQueries = sortBy (comparing (Down . time_in_nanos))

renderTime t
    | t > 1000000 = toHtml $ show (fromIntegral t / 1000000) ++ "ms"
    | t > 1000 = toHtml $ show (fromIntegral t / 1000) ++ "us"
    | otherwise = toHtml $ show t ++ "ns"

nodeCls [] = "node"
nodeCls _ = "node has-children hidden-children"

withValue :: Html () -> Maybe Text -> Html ()
withValue html Nothing = html
withValue html (Just attr) = html `with` [value_ attr]