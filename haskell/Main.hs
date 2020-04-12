{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleInstances     #-}

module Main where

import System.Environment (getArgs)
import Data.ByteString (pack)
import qualified Data.Text as T
import           Data.Text (Text)
import Control.Monad (forM_)
import Network.URI.Encode (encodeText)
import Data.Morpheus.Client
import Data.Morpheus.Types (ScalarValue)
import Control.Monad.Except (runExceptT, liftEither)
import Control.Monad.IO.Class (liftIO)
import Data.Time.LocalTime (ZonedTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)

import Server (api, Zoned(Zoned))

instance MonadFail (Either String) where
  fail = Left

defineByDocumentFile
  "./schema.gql"
  [gql|
  query Q ($unidadId: Int, $alcaldia: String, $horaIso8601: Zoned) {
    ubicaciones (unidadId: $unidadId, alcaldia: $alcaldia, horaIso8601: $horaIso8601) {
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
  query Q2 ($horaIso8601: Zoned) {
    alcaldiasDisponibles (aHoraIso8601: $horaIso8601)
  }
  |]



prependGoogle :: Text -> Text
prependGoogle = ("https://www.google.com/maps/place/" <>)

run :: String -> IO (Either String ())
run host = runExceptT $ do
    hora <- liftEither $ iso8601ParseM "2020-04-08T22:00:00-05:00"

    liftIO $ putStrLn "Ubicaciones de bus 1288:"
    res <- liftIO $ getUbicacionDeUnidad 1288
    (Q { ubicaciones }) <- liftEither res
    forM_ ubicaciones $ liftIO . putStrLn . T.unpack . prependGoogle . encodeText . uLatLonText

    liftIO $ putStrLn "Quáles unidades han estado dentro de Coyoacán?"
    res <- liftIO $ getDentroAlcaldia "Coyoacán"
    (Q { ubicaciones })  <- liftEither res
    liftIO $ print $ map uVehicleId ubicaciones

    liftIO $ putStrLn "Quáles unidades han estado dentro de Coyoacán el 8 de Abril de las 20:00 a las 23:59 en la noche?"
    res <- liftIO $ getBusEnHoraEnAlcaldia hora "Coyoacán"
    (Q { ubicaciones }) <- liftEither res
    liftIO $ print $ map uVehicleId ubicaciones

    liftIO $ putStrLn "Alcaldías disponibles"
    res <- liftIO $ getAlcaldiasDisponibles $ Just hora
    (Q2 { alcaldiasDisponibles=alcaldias }) <- liftEither res
    liftIO $ print alcaldias

    res <- liftIO $ getAlcaldiasDisponibles $ Nothing
    (Q2 { alcaldiasDisponibles=alcaldias }) <- liftEither res
    liftIO $ print alcaldias
  where
    apiWithArgs = api host

    getUbicacionDeUnidad :: Int -> IO (Either String Q)
    getUbicacionDeUnidad vehicleId =
       fetch apiWithArgs QArgs {qArgsUnidadId = Just vehicleId, qArgsAlcaldia = Nothing,       qArgsHoraIso8601 = Nothing}

    getDentroAlcaldia :: String -> IO (Either String Q)
    getDentroAlcaldia alcaldia =
       fetch apiWithArgs QArgs {qArgsUnidadId = Nothing,        qArgsAlcaldia = Just alcaldia, qArgsHoraIso8601 = Nothing}

    getBusEnHoraEnAlcaldia :: ZonedTime -> String -> IO (Either String Q)
    getBusEnHoraEnAlcaldia hora alcaldia =
       fetch apiWithArgs QArgs {qArgsUnidadId = Nothing, qArgsAlcaldia = Just alcaldia, qArgsHoraIso8601 = Just (Zoned hora) }

    getAlcaldiasDisponibles :: Maybe ZonedTime -> IO (Either String Q2)
    getAlcaldiasDisponibles hora =
       fetch apiWithArgs Q2Args {q2ArgsHoraIso8601 = fmap Zoned hora}

main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Need PostgreSQL server hostname"
    host : _ -> do
      res <- run host
      case res of
        Right () -> return ()
        Left err -> putStrLn $ "ExceptT errored: " ++ err
