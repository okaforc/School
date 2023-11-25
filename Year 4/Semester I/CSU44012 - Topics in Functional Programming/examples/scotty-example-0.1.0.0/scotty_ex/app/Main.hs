{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import Data.Text.Lazy
import Web.Scotty
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html.Renderer.Text as R
import qualified Text.Blaze.Html5.Attributes as A
import Network.Wai.Middleware.Static

main:: IO ()
main = scotty 3000 $ do
  get "/" $ do
    html "Hello there!"
  get "/greet" $ do
    html "Hello Iris! It's Marcy :3 "

{-
  get "/greet/" $ do
      html  "Hello there"
-}

  get "/greet/:name" $ do
      name <- param "name"
      html $ response name
      -- html $ m name

m :: Text -> Text
m n = do R.renderHtml $ do
                  H.h1 "gootbye" >> H.toHtml n

response :: Text -> Text
response n = do R.renderHtml $ do
                  H.h1  "Hello " >> H.toHtml n
                  H.br
                  H.br
                  H.button myLink

myLink :: H.Html
myLink = H.a "test" H.! A.href "Marceline Abadeer"