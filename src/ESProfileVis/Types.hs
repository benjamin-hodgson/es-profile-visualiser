{-# LANGUAGE TemplateHaskell #-}

module ESProfileVis.Types where


import Data.Aeson as Aeson
import Data.Aeson.TH
import Data.List (isSuffixOf)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Network.URI (URI)


data ElasticForm = ElasticForm {
    server :: Maybe String,
    index :: Maybe String,
    indexType :: Maybe String,
    searchJSON :: Maybe String
} deriving (Eq, Show)

-- typed model of JSON profile data returned Elastic


data Response = Response {
    profile :: Profile
} deriving (Eq, Ord, Show, Read)

data Profile = Profile {
    shards :: [ShardProfile]
} deriving (Eq, Ord, Show, Read)

data ShardProfile = ShardProfile {
    id_ :: Text,
    searches :: [SearchProfile]
} deriving (Eq, Ord, Show, Read)

totalShardTime :: ShardProfile -> Integer
totalShardTime shard = sum [ time_in_nanos q | s <- searches shard, q <- query s]

data SearchProfile = SearchProfile {
    rewrite_time :: Integer,
    query :: [QueryProfile]
} deriving (Eq, Ord, Show, Read)

data QueryProfile = QueryProfile {
    time :: Text,
    time_in_nanos :: Integer,
    type_ :: Text,
    description :: Text,
    breakdown :: QueryProfileBreakdown,
    children :: Maybe [QueryProfile]
} deriving (Eq, Ord, Show, Read)

childrenList :: QueryProfile -> [QueryProfile]
childrenList = fromMaybe [] . children


data QueryProfileBreakdown = QueryProfileBreakdown {
    advance :: Int,
    advance_count :: Int,
    build_scorer :: Int,
    build_scorer_count :: Int,
    create_weight :: Int,
    create_weight_count :: Int,
    match :: Int,
    match_count :: Int,
    next_doc :: Int,
    next_doc_count :: Int,
    score :: Int,
    score_count :: Int
} deriving (Eq, Ord, Show, Read)


$(do
    let types = [''Response, ''Profile, ''ShardProfile, ''SearchProfile, ''QueryProfile, ''QueryProfileBreakdown]
    let jsonOptions = defaultOptions { fieldLabelModifier = \f -> if last f == '_' then take (length f - 1) f else f }
    concat <$> traverse (deriveJSON jsonOptions) types
    )
    

