{-# LANGUAGE OverloadedStrings #-}
import System.Environment (getArgs)
import Web.Scotty
import Server (api)
import Control.Monad.IO.Class (liftIO)

-- This binary provides an HTTP server,
-- listening on port 3000, which receives
-- GraphQL queries on the '/api' path.
-- See the schema in schema.gql.
-- See Client.hs for a client, which is
-- totally independent of this file.
-- This function is adapted from an example
-- at morpheusgraphql.com
main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Need PostgreSQL server hostname."
    host : _ ->
      scotty 3000 $ post "/api" $ raw =<< (liftIO . (api host) =<< body)
