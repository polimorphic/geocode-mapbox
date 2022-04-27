{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}

module Geocode.Mapbox (Location(..), geocode) where

import Prelude hiding (id, zip)

import Data.Aeson (FromJSON, Value(Object), decode, parseJSON, withObject, (.:), (.:?))
import Data.Aeson.Types (typeMismatch)
import Data.Bool (bool)
import qualified Data.ByteString.Char8 as BC
import Data.Foldable (fold)
import Data.List (find)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Traversable (for)
import Network.HTTP.Client (responseBody)
import Network.HTTP.Simple (httpLBS, parseRequest, setRequestHeaders, setRequestQueryString)

geocode :: String -> String -> Bool -> Maybe (Double, Double) -> IO (Maybe [Location])
geocode addr token perm mloc = do
    req <- parseRequest $ "https://api.mapbox.com/geocoding/v5/mapbox.places"
                       <> bool "" "-permanent" perm <> "/" <> addr <> ".json"
    rsp <- httpLBS . setRequestHeaders
                  [ ("Accept", "applciation/vnd.geo+json")
                  ] $ setRequestQueryString qstr req
    pure . fold $ fmap (\(Output o) -> o) <$> decode (responseBody rsp)
  where
    qstr =
        [ ("access_token", Just $ BC.pack token)
        , ("country", Just "US")
        , ("types", Just "address")
        ] <> fold prox
    prox = (\(lon, lat) -> [("proximity", Just . BC.pack $ show lon ++ "," ++ show lat)]) <$> mloc

reverseGeocode :: String -> String -> (Double, Double) -> IO (Maybe [Location])
reverseGeocode token perm (lon, lat) = do
    req <- parseRequest $ "https://api.mapbox.com/geocoding/v5/mapbox.places"
                       <> bool "" "-permanent" perm <> "/" <> lon <> "," <> lat <> ".json"
    rsp <- httpLBS . setRequestHeaders
                 [ ("Accept", "applciation/vnd.geo+json")
                 ] $ setRequestQueryString qstr req
    pure . fold $ fmap (\(Output o) -> o) <$> decode (responseBody rsp)
  where
    qstr =
        [ ("access_token", Just $ BC.pack token)
        , ("country", Just "US")
        , ("types", Just "address")
        ]

newtype Output = Output [Location]
    deriving Show

data Location = Location
    { lCoordinates :: (Double, Double) --(lon, lat)
    , lStreetAddress :: String
    , lCity :: String
    , lState :: String
    , lZipCode :: String
    , lRelevance :: Double
    } deriving Show

data Geometry = Geometry { gtype :: String, coords :: [Double] }
    deriving Show

instance FromJSON Geometry where
    parseJSON (Object g) = Geometry
        <$> g .: "type"
        <*> g .: "coordinates"

    parseJSON invalid = typeMismatch "Object" invalid

data Context = Context { ctype :: String, cname :: String, ccode :: Maybe String }
    deriving Show

instance FromJSON Context where
    parseJSON (Object c) = Context
        <$> c .: "id"
        <*> c .: "text"
        <*> c .:? "short_code"

    parseJSON invalid = typeMismatch "Object" invalid

instance FromJSON Output where
    parseJSON = withObject "Output" $ \o -> do
        rs <- o .: "features"
        cs <- for rs $ \r -> do
            rel <- r .: "relevance"
            ad <- r .:? "address"
            str <- r .: "text"
            ctxraw <- r .: "context"
            let ctx = ((\ctx' -> Context (takeWhile (/= '.') $ ctype ctx')
                                         (cname ctx')
                                         (drop 3 <$> ccode ctx')
                       ) <$> ctxraw
                      ) :: [Context]
            let zc = cname <$> find (\ctx' -> ctype ctx' == "postcode") ctx
            let ct = cname <$> find (\ctx' -> ctype ctx' == "place") ctx
            let st = find (\ctx' -> ctype ctx' == "region") ctx >>= ccode
            g <- r .: "geometry"
            case (gtype g, coords g) of
                ("Point", [x, y]) -> pure . Just $ Location (x, y)
                                                            (maybe "" (++ " ") ad ++ str)
                                                            (fromMaybe "" ct)
                                                            (fromMaybe "" st)
                                                            (fromMaybe "" zc)
                                                            rel
                _ -> pure Nothing
        pure . Output $ catMaybes cs
