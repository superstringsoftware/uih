{-# LANGUAGE OverloadedStrings, StandaloneDeriving, TypeSynonymInstances, FlexibleInstances #-}
module HTML.Tests where

import Control.Monad (forM_)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Pretty as R

import Text.Blaze.Internal

numbers n = docTypeHtml $ do
               H.head $ do
                   H.title "Natural numbers"
               body $ do
                   p "A list of natural numbers:"
                   ul $ forM_ [1 .. n] (li . toHtml)
