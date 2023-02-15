{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Lib (User, UserAPI, someFunc) where

import Data.Aeson ()
import Elm.Derive qualified (defaultOptions, deriveBoth)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (simpleCors)
import Servant
  ( Application,
    Get,
    JSON,
    Proxy (..),
    Server,
    serve,
    type (:>),
  )

type UserAPI = "users" :> Get '[JSON] [User]

data User = User
  { name :: String,
    age :: Int,
    email :: String
  }
  deriving (Show, Eq, Generic)

Elm.Derive.deriveBoth Elm.Derive.defaultOptions ''User

users :: [User]
users = [User "Isaac Newton" 372 "isaac@newton.co.uk"]

server :: Server UserAPI
server = return users

userAPI :: Proxy UserAPI
userAPI = Proxy

app :: Application
app = simpleCors $ serve userAPI server -- elm からのリクエストに対する cors の問題を回避する。

someFunc :: IO ()
someFunc = do
  putStrLn "Listening on port 8081"
  run 8081 app
