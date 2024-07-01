--------------------------------------------------------------------------------
-- Copyright 2024 Michael P Williams. All rights reserved.
--------------------------------------------------------------------------------

module Gen1.Util
  ( textShow
  ) where

import Data.Text (Text)
import qualified Data.Text as T

-- | Generates text for a type that can be shown.
textShow :: Show a => a -> Text
textShow = T.pack . show
