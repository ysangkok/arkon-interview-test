{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE QuasiQuotes           #-}

module Server (api) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text as T

import Control.Monad (join)
import Data.Maybe (catMaybes, isNothing, maybeToList)
import           Data.Morpheus              (interpreter)
import           Data.Morpheus.Document     (importGQLDocumentWithNamespace)
import           Data.Morpheus.Types        (GQLRootResolver (..), Undefined, Undefined(Undefined), lift, failRes)
import           Data.Text                  (Text)
import           GHC.Generics ()
import Database.PostgreSQL.Simple (connectPostgreSQL, Connection, query, query_, Only(Only), fromOnly)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Data.String (fromString)
import qualified Database.PostgreSQL.Simple as PSQL
import Data.Time.LocalTime (ZonedTime)
import Data.Time.Format.ISO8601 (formatShow, iso8601Format, iso8601ParseM)

importGQLDocumentWithNamespace "schema.gql"

sqlToGQL :: (T.Text, BS.ByteString, T.Text, ZonedTime, T.Text) -> UnidadHoraUbicacion _
sqlToGQL (lat_long_text, ewkb, alcaldia, hora, vehicleId) =
  UnidadHoraUbicacion {
    unidadHoraUbicacionLatLongText = pure lat_long_text
  , unidadHoraUbicacionEwkbHex = pure $ T.pack $ BS.unpack $ B64.encode ewkb
  , unidadHoraUbicacionAlcaldia = pure alcaldia
  , unidadHoraUbicacionHora = pure $ T.pack $ formatShow iso8601Format hora
  , unidadHoraUbicacionVehicleId = pure $ vehicleId
  }

rootResolver :: Connection -> GQLRootResolver IO () Query Undefined Undefined
rootResolver conn =
  GQLRootResolver
    {
      queryResolver = Query {queryUbicaciones, queryAlcaldiasDisponibles},
      mutationResolver = Undefined,
      subscriptionResolver = Undefined
    }
  where
    queryAlcaldiasDisponibles QueryAlcaldiasDisponiblesArgs {queryAlcaldiasDisponiblesArgsHoraIso8601=hora} =
      case hora of
        Nothing -> do
          alcaldias <- lift $ query_ conn "select alcaldia from refined group by alcaldia"
          pure $ map ( T.pack . fromOnly ) alcaldias
        Just hora ->
          case iso8601ParseM @Maybe @ZonedTime (T.unpack hora) of
            Nothing -> failRes "invalid iso8601 time"
            Just t -> do
              alcaldias <-
                lift $
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
    queryUbicaciones QueryUbicacionesArgs {queryUbicacionesArgsUnidadId=unidad, queryUbicacionesArgsHoraIso8601=hora, queryUbicacionesArgsAlcaldia=alcaldia} =
        let
          zoned :: Maybe ZonedTime
          zoned = join $ fmap (iso8601ParseM . T.unpack) hora
          textClauses :: [(Text, Text)]
          textClauses = catMaybes [
              fmap (\vehicle_id_str -> ("vehicle_id = ?", vehicle_id_str)) unidad
            , fmap (\alcaldia -> ("alcaldia = ?", alcaldia)) alcaldia
            ]
          timeClauses :: Maybe (Text, ZonedTime)
          timeClauses =
              fmap (\time_update -> ("age(time_update, ?) > interval '-2 hours' and age(time_update, ?) < interval  '2 hours'", time_update)) zoned
          allClauses = fmap fst textClauses ++ maybeToList (fmap fst timeClauses)
          where_clause :: PSQL.Query
          where_clause =
            if length textClauses == 0 && isNothing timeClauses
              then ""
              else fromString $ T.unpack $ " where " <> T.intercalate " and " allClauses
          full_query :: PSQL.Query
          full_query = "select lat_lon_text, ewkb, alcaldia, time_update, vehicle_id from refined" <> where_clause
        in do
          case hora of
            Nothing -> do
              latlongs <-
                lift $ case textClauses of
                  [] ->
                    query_ conn full_query
                  one:[] ->
                    query conn full_query $ (Only $ snd one)
                  one:two:[] ->
                    query conn full_query $ (snd one, snd two)
              pure $ map sqlToGQL latlongs
            Just hora ->
              case iso8601ParseM @Maybe @ZonedTime (T.unpack hora) of
                Nothing -> failRes "invalid iso8601 time"
                Just zoned -> do
                  latlongs <-
                    lift $ case textClauses of
                      [] ->
                        query conn full_query $ (zoned, zoned)
                      one:[] ->
                        query conn full_query $ (snd one, zoned, zoned)
                      one:two:[] ->
                        query conn full_query $ (snd one, snd two, zoned, zoned)
                  pure $ map sqlToGQL latlongs

api :: B.ByteString -> IO B.ByteString
api input = do
  conn <- connectPostgreSQL ""
  interpreter (rootResolver conn) input
