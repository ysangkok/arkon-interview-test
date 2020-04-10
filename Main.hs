{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main where

import qualified Data.Text as T
import           Data.Text                  (Text)
import Control.Monad (forM_)
import Network.URI.Encode (encodeText)
import Data.Morpheus.Client

import Server (api)

defineByDocumentFile
  "./schema.gql"
  [gql|
  query Q ($unidadId: String, $alcaldia: String, $horaIso8601: String) {
    ubicaciones (unidadId: $unidadId, alcaldia: $alcaldia, horaIso8601: $horaIso8601) {
      latLongText
      ewkbHex
      hora
      alcaldia
      vehicleId
    }
  }
  |]

defineByDocumentFile
  "./schema.gql"
  [gql|
  query Q2 ($horaIso8601: String) {
    alcaldiasDisponibles (horaIso8601: $horaIso8601)
  }
  |]


getUbicacionDeUnidad :: String -> IO (Either String Q)
getUbicacionDeUnidad vehicleId =
   fetch api QArgs {qArgsUnidadId = Just vehicleId, qArgsAlcaldia = Nothing,       qArgsHoraIso8601 = Nothing}

getDentroAlcaldia :: String -> IO (Either String Q)
getDentroAlcaldia alcaldia =
   fetch api QArgs {qArgsUnidadId = Nothing,        qArgsAlcaldia = Just alcaldia, qArgsHoraIso8601 = Nothing}

getBusEnHoraEnAlcaldia :: String -> String -> IO (Either String Q)
getBusEnHoraEnAlcaldia hora alcaldia =
   fetch api QArgs {qArgsUnidadId = Nothing, qArgsAlcaldia = Just alcaldia, qArgsHoraIso8601 = Just hora }

getAlcaldiasDisponibles :: Maybe String -> IO (Either String Q2)
getAlcaldiasDisponibles hora =
   fetch api Q2Args {q2ArgsHoraIso8601 = hora}

prependGoogle :: Text -> Text
prependGoogle = ("https://www.google.com/maps/place/" <>)

main :: IO ()
main = do
  putStrLn "Ubicaciones de bus 1288:"
  Right (Q { ubicaciones }) <- getUbicacionDeUnidad "1288"
  forM_ ubicaciones $ putStrLn . T.unpack . prependGoogle . encodeText . latLongText

  putStrLn "Quáles unidades han estado dentro de Coyoacán?"
  Right (Q { ubicaciones }) <- getDentroAlcaldia "Coyoacán"
  print $ map vehicleId ubicaciones

  putStrLn "Quáles unidades han estado dentro de Coyoacán el 8 de Abril de las 20:00 a las 23:59 en la noche?"
  Right (Q { ubicaciones }) <- getBusEnHoraEnAlcaldia "2020-04-08T22:00:00-05:00" "Coyoacán"
  print $ map vehicleId ubicaciones

  putStrLn "Alcaldías disponibles"
  Right (Q2 alcaldias) <- getAlcaldiasDisponibles $ Just "2020-04-08T22:00:00-05:00"
  print alcaldias

  Right (Q2 alcaldias) <- getAlcaldiasDisponibles $ Nothing
  print alcaldias

