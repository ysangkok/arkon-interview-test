{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleInstances     #-}

-- This file shows a few sample queries
-- for the GraphQL API.
module Main where

-- Text and ByteString modules.
import Data.ByteString (pack)
import qualified Data.Text as T
import           Data.Text (Text)

-- Standard library
import System.Environment (getArgs)
import Control.Monad (forM_)

-- For showing Google links for locations
import Network.URI.Encode (encodeText)

-- time
import Data.Time.LocalTime (ZonedTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)

-- transformers and mtl
import Control.Monad.Except (runExceptT, liftEither)
import Control.Monad.IO.Class (liftIO)

-- MorpheusGraphQL
import Data.Morpheus.Client
-- This is used by the generated code
import Data.Morpheus.Types (ScalarValue)

-- Since this is just a demo, we just include
-- the server function 'api' directly,
-- and we skip the HTTP encoding.
import Server (api, Zoned(Zoned))

-- This instance is used so that we can conveniently
-- short-circuit when an error occurs in the 'run'
-- function below.
instance MonadFail (Either String) where
  -- Left values are failures by convention
  fail = Left

-- This function uses TemplateHaskell to generate
-- types based on the queries and the schema.
defineByDocumentFile
  "./schema.gql"
  [gql|
  # These three parameters are all optional!
  query Q ($unidadId: Int, $alcaldia: String, $horaIso8601: Zoned) {
    ubicaciones (unidadId: $unidadId, alcaldia: $alcaldia, horaIso8601: $horaIso8601) {
      # These names have to match what is defined in Server.hs and schema.gql.
      uLatLonText
      uEwkbB64
      uHora
      uAlcaldia
      uVehicleId
    }
  }
  |]

defineByDocumentFile
  "./schema.gql"
  [gql|
  # Optional parameter to specify a 4-hour time slot to
  # search in. If not supplied, all alcaldias ever visited
  # will be returned.
  query Q2 ($horaIso8601: Zoned) {
    # Since returns a list of Strings, as can be seen
    # in schema.gql and Server.hs.
    alcaldiasDisponibles (aHoraIso8601: $horaIso8601)
  }
  |]

-- This function prepends a Google Maps URL prefix, so that
-- the text with latitude/longitude returned by the Server
-- can easily be checked by clicking the link in the terminal,
-- which shows the location in a browser window.
prependGoogle :: Text -> Text
prependGoogle = ("https://www.google.com/maps/place/" <>)

-- Takes a PostgreSQL server hostname an returns
-- an IO action which will print various examples
-- of usage of the GraphQL API.
-- A Left will be returned in case of error,
-- which will typically be because of a problem
-- connecting to the PostgreSQL server, or because
-- the tables to be queried have not been created
-- in the database.
run :: String -> IO (Either String ())
run host = runExceptT $ do
    liftIO $ putStrLn "Ubicaciones de bus 1288:"
    res <- liftIO $ getUbicacionDeUnidad 1288
    -- The following line, which is used a few more times,
    -- extracts the list of locations from the response,
    -- and aborts and propagates the error in case one occurred.
    (Q { ubicaciones }) <- liftEither res
    forM_ ubicaciones $
        -- Show Google Maps links made using the latitude/lonitude-text.
        liftIO . putStrLn . T.unpack . prependGoogle . encodeText . uLatLonText

    liftIO $ putStrLn "Quáles unidades han estado dentro de Coyoacán?"
    res <- liftIO $ getDentroAlcaldia "Coyoacán"
    (Q { ubicaciones })  <- liftEither res
    -- Only show vehicle ID's.
    liftIO $ print $ map uVehicleId ubicaciones

    liftIO $ putStrLn "Quáles unidades han estado dentro de Coyoacán el 8 de Abril de las 20:00 a las 23:59 en la noche?"
    hora <- liftEither $ iso8601ParseM "2020-04-08T22:00:00-05:00" -- the middle of a 4 hours timeslot
    res <- liftIO $ getBusEnHoraEnAlcaldia hora "Coyoacán"
    (Q { ubicaciones }) <- liftEither res
    -- Only show vehicle ID's.
    liftIO $ print $ map uVehicleId ubicaciones

    liftIO $ putStrLn "Alcaldías disponibles (este 4 horas de arriba)"
    res <- liftIO $ getAlcaldiasDisponibles $ Just hora
    (Q2 { alcaldiasDisponibles=alcaldias }) <- liftEither res
    liftIO $ print alcaldias

    liftIO $ putStrLn "Alcaldías disponibles (todo el tiempo). Por ejemplo, no contiene Xochimilco."
    res <- liftIO $ getAlcaldiasDisponibles $ Nothing
    (Q2 { alcaldiasDisponibles=alcaldias }) <- liftEither res
    liftIO $ print alcaldias
  where
    apiWithArgs = api host

    -- Filter 'ubicaciones' (bus locations) only by Vehicle ID number.
    -- The parameter could be e.g. 1288.
    getUbicacionDeUnidad :: Int -> IO (Either String Q)
    getUbicacionDeUnidad vehicleId =
       fetch apiWithArgs QArgs {qArgsUnidadId = Just vehicleId, qArgsAlcaldia = Nothing,       qArgsHoraIso8601 = Nothing}

    -- Filter 'ubicaciones' (bus locations) only by the 'alcaldia' (city district)
    -- where the bus appeared.
    -- The parameter could be e.g. 'Coyoacán'.
    getDentroAlcaldia :: String -> IO (Either String Q)
    getDentroAlcaldia alcaldia =
       fetch apiWithArgs QArgs {qArgsUnidadId = Nothing,        qArgsAlcaldia = Just alcaldia, qArgsHoraIso8601 = Nothing}

    -- Filter 'ubicaciones' (bus locations) by middle of 4-hour timeslot
    -- and additionally by the 'alcaldia' (city district).
    -- For example, pass 01-01-2000 16:00 for a timeslot
    -- between 14 and 18 on this day.
    -- The second parameter is the 'alcaldia', e.g. 'Coyoacán'.
    getBusEnHoraEnAlcaldia :: ZonedTime -> String -> IO (Either String Q)
    getBusEnHoraEnAlcaldia hora alcaldia =
       fetch apiWithArgs QArgs {qArgsUnidadId = Nothing, qArgsAlcaldia = Just alcaldia, qArgsHoraIso8601 = Just (Zoned hora) }

    -- Filter the list of 'alcaldias' which buses have visited
    -- by the middle of the 4-hour timeslot to filter by.
    -- For example, alcaldia A was visited at 10:00,
    --                       B             at 12:00,
    --                   and C             at 14:00,
    -- and a timestamp 13:55 is passed,
    -- B and C will be returned since both of their
    -- respective timestamps are within the timeslot.
    -- A would not be returned because 10 is more than
    -- 2 hours before 13:55.
    getAlcaldiasDisponibles :: Maybe ZonedTime -> IO (Either String Q2)
    getAlcaldiasDisponibles hora =
       fetch apiWithArgs Q2Args {q2ArgsHoraIso8601 = fmap Zoned hora}

main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Need PostgreSQL server hostname."
    host : _ -> do
      res <- run host
      case res of
        Right () -> return ()
        Left err -> putStrLn $ "ExceptT errored: " ++ err
