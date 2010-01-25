module Shrimp.Helpers where

import Data.ByteString.Lazy (ByteString)
import Text.StringTemplate
import Maybe (fromJust)

renderTemplate dir template attrs = do
  templates <- directoryGroup dir :: IO (STGroup ByteString)
  let t = fromJust $ getStringTemplate template templates
  return render (setManyAttrib attrs t)
