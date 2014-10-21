module Example
(main)
where

import Ajax
import qualified Control.Monad.JQuery as J
import Debug.Trace
import Data.Maybe
main = do
  trace "foo bar"
  jsonGet "index.html" Nothing trace (\f -> trace f)


