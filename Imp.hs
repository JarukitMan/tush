module Imp where

import qualified Data.Text as T
import qualified Data.Map as M

-- WIP
data Markdown = Topic T.Text [Markdown] | List [Markdown] | Table [Markdown] [[Markdown]] | Line T.Text | Code T.Text | Block T.Text
