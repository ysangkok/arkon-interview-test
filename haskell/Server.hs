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
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE LambdaCase            #-}

module Server (api, Zoned(Zoned)) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text as T

import Data.Aeson (ToJSON)
import Data.Maybe (catMaybes)
import Data.String (fromString)
import Data.Text                  (Text)
import Data.Time.LocalTime (ZonedTime)
import Data.Time.Format.ISO8601 (formatShow, iso8601Format, iso8601ParseM)
import GHC.Generics (Generic)

-- 'Morpheus' is the library we use implement GraphQL query parsing.
import Data.Morpheus       (interpreter)
import Data.Morpheus.Types (GQLRootResolver (..), Undefined(Undefined), lift, GQLType, GQLScalar(..))
import qualified Data.Morpheus.Types as MT
import Data.Morpheus.Kind (SCALAR)

-- The bus coordinates are imported into PostgreSQL, which we will query after
-- receiving a GraphQL query.
import Database.PostgreSQL.Simple (connectPostgreSQL, Connection, query, query_, Only(Only), fromOnly)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified Database.PostgreSQL.Simple as PSQL


-- This record contains the two methods also described in schema.gql.
data Query m = Query
  -- This query fetches locations/time/vehicle-ID's. See the Args type for filters.
  { ubicaciones :: QueryUbicacionesArgs -> m [UnidadHoraUbicacion m]
  -- This query returns a list of alcaldias (city districts)
  , alcaldiasDisponibles :: QueryAlcaldiasDisponiblesArgs -> m [Text]
  } deriving (Generic, GQLType)

-- This is the record returned for the ubicaciones query.
data UnidadHoraUbicacion m = UnidadHoraUbicacion
  { uHora :: m Zoned
  , uAlcaldia :: m Text
  -- A textual coordinate
  , uLatLonText :: m Text
  -- A binary coordinate encoding (EWKB), also encoded in Base64
  , uEwkbB64 :: m Text
  , uVehicleId :: m Int
  } deriving (Generic,GQLType)

-- An 'ubicaciones' query takes an alcaldia, a vehicle ID, or
-- a timestamp (which will be the middle of a 4 hours interval)
-- All parameters are optional (implemented with 'Maybe').
data QueryUbicacionesArgs = QueryUbicacionesArgs
  { alcaldia :: Maybe Text
  , unidadId :: Maybe Int
  , horaIso8601 :: Maybe Zoned
  } deriving (Generic)

-- An 'alcaldiasDisponibles' query only takes
-- an optional timestamp which is the middle of a 4 hour
-- interval.
data QueryAlcaldiasDisponiblesArgs = QueryAlcaldiasDisponiblesArgs
  { aHoraIso8601 :: Maybe Zoned
  } deriving (Generic)

-- This instance is used in the parseValue implementation
-- below. A failure is the Left of an Either, by convention.
instance MonadFail (Either Text) where
  fail = Left . T.pack

-- A datatype for transmitting a timestamp with timezone
-- over GraphQL. The ToJSON instance is used in the client,
-- but we define it here to avoid orphan instances.
data Zoned = Zoned ZonedTime deriving (Generic, Show, ToJSON)

-- The timestamp is encoded as a String which stores the
-- ISO 8601 representation.
instance GQLScalar Zoned where
  parseValue (MT.String x) = fmap Zoned $ iso8601ParseM $ T.unpack x
  serialize  (Zoned value) = MT.String $ T.pack $ formatShow iso8601Format value

-- This tells Morpheus that our timestamp is encoded as
-- a JSON scalar (a String)
instance GQLType Zoned where
  type KIND Zoned = SCALAR

-- This function converts the data returned by Postgres to
-- the GraphQL datatype which is returned to the client.
sqlToGQL :: (T.Text, BS.ByteString, T.Text, ZonedTime, Int) -> UnidadHoraUbicacion _
sqlToGQL (lat_long_text, ewkb, alcaldia, hora, vehicleId) =
  UnidadHoraUbicacion {
    uLatLonText = pure lat_long_text
  , uEwkbB64 = pure $ T.pack $ BS.unpack $ B64.encode ewkb
  , uAlcaldia = pure alcaldia
  , uHora = pure $ Zoned hora
  , uVehicleId = pure vehicleId
  }

-- A utility function for generating WHERE clauses for the
-- SQL query, depending on whether a given filter was supplied or
-- or not. The returned WHERE clauses should be connected
-- by ANDs.

-- Caution: this order has to be in sync with the
--          Maybe-matching tree in queryUbicaciones!

-- Example:
--   clauses Nothing (Just "Coyoacán") Nothing
--   ==
--   ["alcaldia = \"Coyoacán\""]
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

-- The rootResolver resolves GraphQL queries. We need a PostgreSQL connection,
-- so it is passed in as a parameter.
rootResolver :: Connection -> GQLRootResolver IO () Query Undefined Undefined
rootResolver conn =
  GQLRootResolver
    {
      -- The two query resolvers that are supported are passed into Morpheus here.
      queryResolver = Query {ubicaciones=queryUbicaciones, alcaldiasDisponibles=queryAlcaldiasDisponibles},
      -- We don't support mutations or subscriptions.
      mutationResolver = Undefined,
      subscriptionResolver = Undefined
    }
  where
    queryAlcaldiasDisponibles QueryAlcaldiasDisponiblesArgs {aHoraIso8601=hora} =
      case hora of
        Nothing -> do
          -- If there is no timestamp filter, fetch all alcaldias that have
          -- ever been visited by a metrobus.
          alcaldias <- lift $ query_ conn "select alcaldia from refined group by alcaldia"
          pure $ map ( T.pack . fromOnly ) alcaldias
        Just (Zoned t) -> do
          alcaldias <- lift $
            query
              conn
              -- If there is a timestamp filter, fetch all the alcaldias
              -- visited by any metrobus within the 4 hours timeslot with
              -- the passed timestamp as the middle.
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
        -- We extract the ZonedTime from our GraphQL type.
        -- Since the value can only be constructed with valid
        -- timestamps, we don't need to worry about validation here.
        zoned :: Maybe ZonedTime
        zoned = fmap (\case Zoned x -> x) hora
        allClauses = clauses unidad alcaldia zoned
        full_query :: PSQL.Query
        full_query = "select lat_lon_text, ewkb, alcaldia, time_update, vehicle_id from refined" <> where_clause
          where
            where_clause :: PSQL.Query
            where_clause =
              if allClauses == []
                -- if there are no filters, there will be no where clause (empty)
                then ""
                -- join the generated where clauses by ANDs
                else fromString $ T.unpack $ " where " <> T.intercalate " and " allClauses
      in do
        -- See the co-domain of sqlToGQL for the type of this list of tuples.
        results <- lift $
          case hora of
            Nothing ->
              -- This branch is for queries with no timestamp-filter.
              case (unidad, alcaldia) of
                (Nothing, Nothing) ->
                  -- The PostgreSQL-binding that we use sadly doesn't
                  -- support passing bindings in a list, so we have to
                  -- duplicate the function call for each parameter
                  -- combination. One could use a different library...
                  -- But there are only 8 possibilities, so I think this
                  -- is reasonably simple.
                  query_ conn full_query
                (Just vehicle, Nothing) ->
                  query conn full_query $ (Only $ vehicle)
                (Just vehicle, Just alcaldia) ->
                  query conn full_query $ (vehicle, alcaldia)
                (Nothing , Just alcaldia) ->
                  query conn full_query $ (Only $ alcaldia)
            Just (Zoned zoned) ->
              -- This branch is for timestamp-filtered queries.
              -- The timestamps are passed last, since they occur
              -- last in the list returned by the 'clauses' function.
              case (unidad, alcaldia) of
                (Nothing, Nothing) ->
                  query conn full_query $ (zoned, zoned)
                (Just vehicle, Nothing) ->
                  query conn full_query $ (vehicle, zoned, zoned)
                (Just vehicle, Just alcaldia) ->
                  query conn full_query $ (vehicle, alcaldia, zoned, zoned)
                (Nothing, Just alcaldia) ->
                  query conn full_query $ (alcaldia, zoned, zoned)
        pure $ map sqlToGQL results

-- This exported function takes a hostname of the PostgreSQL server
-- to connect to. It also takes a an encoded GraphQL query, which
-- Morpheus will decode for us, and delegate to the relevant query
-- function from above. The database name is assumed to be 'postgres',
-- and no password is used for the connection. (which could be changed
-- easily, of course)
api :: String -> B.ByteString -> IO B.ByteString
api host input = do
  conn <- connectPostgreSQL $ "host='" <> BS.pack host <> "' dbname='postgres' user='postgres'"
  interpreter (rootResolver conn) input
