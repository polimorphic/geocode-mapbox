{-# LANGUAGE DataKinds, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, OverloadedStrings, TypeApplications, TypeOperators #-}

module Geocode.Mapbox (geocode) where

import Prelude hiding (id, zip)

import Data.Aeson (FromJSON, Value(Object), parseJSON, withObject, (.:))
import Data.Aeson.Types (typeMismatch)
import Data.Bool (bool)
import Data.Default (def)
import qualified Data.Text as T
import Data.Traversable (for)
import Data.Witherable (catMaybes)
import Network.HTTP.Req
    ( GET(GET), NoReqBody(NoReqBody)
    , header, https, jsonResponse, req, responseBody, runReq
    , (/:), (=:)
    )

geocode :: String -> String -> Bool -> IO (Maybe [(Double, Double, String, Double)])
geocode addr token perm = do
    let rq = req GET
                 ( https "api.mapbox.com" /: "geocoding"
                                          /: "v5"
                                          /: (bool "mapbox.places" "mapbox.places-permanent" perm)
                                          /: (T.pack addr <> ".json")
                 )
                 NoReqBody
                 jsonResponse
                 (header "Accept" "application/vnd.geo+json" <> ("access_token" =: token))
    mo <- responseBody <$> runReq def rq
    case mo of
        Nothing -> pure Nothing
        Just (Output cs) -> pure $ Just cs

newtype Output = Output [(Double, Double, String, Double)]
    deriving Show

data Geometry = Geometry { gtype :: String, coords :: [Double] }
    deriving Show

instance FromJSON Geometry where
    parseJSON (Object g) = Geometry
        <$> g .: "type"
        <*> g .: "coordinates"

    parseJSON invalid = typeMismatch "Object" invalid

instance FromJSON Output where
    parseJSON = withObject "Output" $ \o -> do
        rs <- o .: "features"
        cs <- for rs $ \r -> do
            rel <- r .: "relevance"
            pn <- r .: "place_name"
            g <- r .: "geometry"
            case (gtype g, coords g) of
                ("Point", [x, y]) -> pure $ Just (x, y, pn, rel)
                _ -> pure Nothing
        pure . Output $ catMaybes cs
