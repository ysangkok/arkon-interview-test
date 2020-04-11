{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE LambdaCase            #-}

module Server (api, Zoned(Zoned)) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text as T

import Control.Monad (join)
import Data.Aeson (ToJSON)
import Data.Maybe (catMaybes, isNothing)
import Data.String (fromString)
import Data.Text                  (Text)
import Data.Time.LocalTime (ZonedTime)
import Data.Time.Format.ISO8601 (formatShow, iso8601Format, iso8601ParseM)
import GHC.Generics (Generic)

import Data.Morpheus       (interpreter)
import Data.Morpheus.Types (GQLRootResolver (..), Undefined(Undefined), lift, GQLType, GQLScalar(..))
import qualified Data.Morpheus.Types as MT
import Data.Morpheus.Kind (SCALAR)

import Database.PostgreSQL.Simple (connectPostgreSQL, Connection, query, query_, Only(Only), fromOnly)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified Database.PostgreSQL.Simple as PSQL

data Query m = Query
  { ubicaciones :: QueryUbicacionesArgs -> m [UnidadHoraUbicacion m]
  , alcaldiasDisponibles :: QueryAlcaldiasDisponiblesArgs -> m [Text]
  } deriving (Generic, GQLType)

data UnidadHoraUbicacion m = UnidadHoraUbicacion
  { uHora :: m Zoned
  , uAlcaldia :: m Text
  , uLatLonText :: m Text
  , uEwkbB64 :: m Text
  , uVehicleId :: m Int
  } deriving (Generic,GQLType)

data QueryUbicacionesArgs = QueryUbicacionesArgs
  { alcaldia :: Maybe Text
  , unidadId :: Maybe Int
  , horaIso8601 :: Maybe Zoned
  } deriving (Generic)

data QueryAlcaldiasDisponiblesArgs = QueryAlcaldiasDisponiblesArgs
  { aHoraIso8601 :: Maybe Zoned
  } deriving (Generic)

instance MonadFail (Either Text) where
  fail = Left . T.pack

data Zoned = Zoned ZonedTime deriving (Generic, Show, ToJSON)

instance GQLScalar Zoned where
  parseValue (MT.String x) = fmap Zoned $ iso8601ParseM $ T.unpack x
  serialize  (Zoned value) = MT.String $ T.pack $ formatShow iso8601Format value

instance GQLType Zoned where
  type KIND Zoned = SCALAR

sqlToGQL :: (T.Text, BS.ByteString, T.Text, ZonedTime, Int) -> UnidadHoraUbicacion _
sqlToGQL (lat_long_text, ewkb, alcaldia, hora, vehicleId) =
  UnidadHoraUbicacion {
    uLatLonText = pure lat_long_text
  , uEwkbB64 = pure $ T.pack $ BS.unpack $ B64.encode ewkb
  , uAlcaldia = pure alcaldia
  , uHora = pure $ Zoned hora
  , uVehicleId = pure vehicleId
  }

data SQLClauseAndBinding a = SQLClauseAndBinding {
    sqlClause :: Text
  , bindingValue :: a
} deriving (Show)

clauses :: Maybe Int -> Maybe Text -> Maybe ZonedTime -> [Text]
clauses unidad alcaldia zoned =
    catMaybes [vehicleClause, alcaldiaClause, timeClauses]
  where
    vehicleClause :: Maybe Text
    vehicleClause =
      fmap (\_ -> "vehicle_id = ?" ) unidad
    alcaldiaClause :: Maybe Text
    alcaldiaClause =
      fmap (\_ -> "alcaldia = ?" ) alcaldia
    timeClauses :: Maybe Text
    timeClauses =
        fmap (\_ -> "age(time_update, ?) > interval '-2 hours' and age(time_update, ?) < interval  '2 hours'") zoned

rootResolver :: Connection -> GQLRootResolver IO () Query Undefined Undefined
rootResolver conn =
  GQLRootResolver
    {
      queryResolver = Query {ubicaciones=queryUbicaciones, alcaldiasDisponibles=queryAlcaldiasDisponibles},
      mutationResolver = Undefined,
      subscriptionResolver = Undefined
    }
  where
    queryAlcaldiasDisponibles QueryAlcaldiasDisponiblesArgs {aHoraIso8601=hora} =
      case hora of
        Nothing -> do
          alcaldias <- lift $ query_ conn "select alcaldia from refined group by alcaldia"
          pure $ map ( T.pack . fromOnly ) alcaldias
        Just (Zoned t) -> do
          alcaldias <- lift $
            query
              conn
              [sql|
                 select alcaldia from refined
                   where age(time_update, ?) > interval '-2 hours'
                     and age(time_update, ?) < interval '2 hours'
                   group by alcaldia
              |]
              (t, t)
          pure $ map ( T.pack . fromOnly ) alcaldias
    queryUbicaciones QueryUbicacionesArgs {unidadId=unidad, horaIso8601=hora, alcaldia=alcaldia} =
      let
        zoned :: Maybe ZonedTime
        zoned = fmap (\case Zoned x -> x) hora
        allClauses = clauses unidad alcaldia zoned
        full_query :: PSQL.Query
        full_query = "select lat_lon_text, ewkb, alcaldia, time_update, vehicle_id from refined" <> where_clause
          where
            where_clause :: PSQL.Query
            where_clause =
              if allClauses == []
                then ""
                else fromString $ T.unpack $ " where " <> T.intercalate " and " allClauses
      in do
        latlons <- lift $
          case hora of
            Nothing ->
              case (unidad, alcaldia) of
                (Nothing, Nothing) ->
                  query_ conn full_query
                (Just vehicle, Nothing) ->
                  query conn full_query $ (Only $ vehicle)
                (Just vehicle, Just alcaldia) ->
                  query conn full_query $ (vehicle, alcaldia)
                (Nothing , Just alcaldia) ->
                  query conn full_query $ (Only $ alcaldia)
            Just (Zoned zoned) ->
              case (unidad, alcaldia) of
                (Nothing, Nothing) ->
                  query conn full_query $ (zoned, zoned)
                (Just vehicle, Nothing) ->
                  query conn full_query $ (vehicle, zoned, zoned)
                (Just vehicle, Just alcaldia) ->
                  query conn full_query $ (vehicle, alcaldia, zoned, zoned)
                (Nothing, Just alcaldia) ->
                  query conn full_query $ (alcaldia, zoned, zoned)
        pure $ map sqlToGQL latlons

api :: B.ByteString -> IO B.ByteString
api input = do
  conn <- connectPostgreSQL ""
  interpreter (rootResolver conn) input
