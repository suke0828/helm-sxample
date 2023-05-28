{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Lib (User, UserAPI, runServer) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson ()
import Data.Array.IO (IOArray)
import Data.Array.MArray
  ( MArray (getBounds),
    getElems,
    newListArray,
    writeArray,
  )
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Elm.Derive qualified (defaultOptions, deriveBoth)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (CorsResourcePolicy, cors, corsRequestHeaders, simpleCorsResourcePolicy, simpleHeaders)
import Servant
  ( Application,
    Get,
    Handler,
    JSON,
    Post,
    Proxy (..),
    ReqBody,
    Server,
    serve,
    type (:<|>) (..),
    type (:>),
  )

type UserAPI =
  "users" :> Get '[JSON] [User]
    :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] ()

data User = User
  { name :: String,
    age :: Int,
    email :: String
  }
  deriving (Show, Eq, Generic)

Elm.Derive.deriveBoth Elm.Derive.defaultOptions ''User

server :: IORef (IOArray Int User) -> Server UserAPI
server userArrayRef = getUsers :<|> postUser
  where
    getUsers :: Handler [User]
    getUsers = do
      userArray <- liftIO $ readIORef userArrayRef
      liftIO $ getElems userArray

    postUser :: User -> Handler ()
    postUser user = do
      userArray <- liftIO $ readIORef userArrayRef
      liftIO $ do
        lastIndex <- snd <$> getBounds userArray
        newArray <- newListArray (0, lastIndex + 1) =<< getElems userArray
        writeArray newArray (lastIndex + 1) user
        writeIORef userArrayRef newArray

userAPI :: Proxy UserAPI
userAPI = Proxy

policy :: CorsResourcePolicy
policy =
  simpleCorsResourcePolicy
    { corsRequestHeaders = simpleHeaders
    }

app :: IORef (IOArray Int User) -> Application
app userArrayRef = cors (const $ Just policy) $ serve userAPI (server userArrayRef) -- elm からのリクエストに対する cors の問題を回避する。

runServer :: IO ()
runServer = do
  initialUsers <-
    newListArray
      (0, 1)
      [ User "Isaac Newton" 372 "isaac@newton.co.uk",
        User "Albert Einstein" 373 "ae@mc2.org"
      ]
  userArrayRef <- newIORef initialUsers
  putStrLn "Listening on port 8081"
  run 8081 (app userArrayRef)
