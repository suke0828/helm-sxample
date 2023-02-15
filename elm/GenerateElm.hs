{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Lib (User, UserAPI)
import Servant.Elm
  ( DefineElm (DefineElm),
    ElmOptions (urlPrefix),
    Proxy (..),
    UrlPrefix (Static),
    defElmImports,
    defElmOptions,
    generateElmModuleWith,
  )

-- Elmコードを生成するためのデフォルトのオプションの設定
elmOpts :: ElmOptions
elmOpts = defElmOptions {urlPrefix = Static "http://localhost:8081"}

main :: IO ()
main =
  putStrLn "Generating..."
    *> generateElmModuleWith -- Elmの型定義のリストとAPIを与えて、完全なElmモジュールを生成するヘルパー
      elmOpts
      ["Generated", "UserAPI"] -- Generated.UserAPIというモジュールを生成
      defElmImports -- 生成されたElmのコードで必要とされるデフォルトのインポートを生成
      "elm/src" -- file path (elmディレクトリ以下に生成する)
      [DefineElm (Proxy :: Proxy User)] -- User type の生成
      (Proxy :: Proxy UserAPI) -- API関数の生成
