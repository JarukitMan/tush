module Tsf where
import Graphics.Vty
import Graphics.Vty.CrossPlatform
import Data.Text as T
import Imp

data Config = NotDefinedYet

readline :: Config -> IO String
readline config = do
  vty <- mkVty defaultConfig
  out <- normal vty config
  shutdown vty
  return out

readMarkdown :: Markdown -> Config
readMarkdown md = undefined

normal :: Vty -> Config -> IO String
normal = undefined

render :: T.Text -> ((Int, Int), (Int, Int)) -> Picture
render = undefined
