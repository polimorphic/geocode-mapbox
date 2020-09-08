{-# LANGUAGE DataKinds, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, OverloadedStrings, TypeApplications, TypeOperators #-}

module Geocode.Mapbox (Location(..), geocode) where

import Prelude hiding (id, zip)

import Data.Aeson (FromJSON, Value(Object), parseJSON, withObject, (.:), (.:?))
import Data.Aeson.Types (typeMismatch)
import Data.Bool (bool)
import Data.Default (def)
import Data.List (find)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Traversable (for)
import Data.Witherable (catMaybes)
import Network.HTTP.Req
    ( GET(GET), NoReqBody(NoReqBody)
    , header, https, jsonResponse, req, responseBody, runReq
    , (/:), (=:)
    )

geocode :: String -> String -> Bool -> IO (Maybe [Location])
geocode addr token perm = do
    let rq = req GET
                 ( https "api.mapbox.com" /: "geocoding"
                                          /: "v5"
                                          /: (bool "mapbox.places" "mapbox.places-permanent" perm)
                                          /: (T.pack addr <> ".json")
                 )
                 NoReqBody
                 jsonResponse
                 (header "Accept" "application/vnd.geo+json" <> ("access_token" =: token)
                                                             <> ("country" =: ("US" :: String))
                                                             <> ("types" =: ("address" :: String))
                 )
    mo <- responseBody <$> runReq def rq
    case mo of
        Nothing -> pure Nothing
        Just (Output cs) -> pure $ Just cs

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
                                                            (maybe "" (\ad' -> ad' ++ " ") ad ++ str)
                                                            (fromMaybe "" ct)
                                                            (fromMaybe "" st)
                                                            (fromMaybe "" zc)
                                                            rel
                _ -> pure Nothing
        pure . Output $ catMaybes cs
