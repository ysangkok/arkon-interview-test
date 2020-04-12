{-# LANGUAGE OverloadedStrings #-}
import System.Environment (getArgs)
import Web.Scotty
import Server (api)
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Need PostgreSQL server hostname."
    host : _ ->
      scotty 3000 $ post "/api" $ raw =<< (liftIO . (api host) =<< body)
