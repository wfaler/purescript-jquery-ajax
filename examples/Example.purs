module Example
(main)
where

import Network.Ajax
import qualified Control.Monad.JQuery as J
import Debug.Trace
import Data.Maybe
import Data.Either
import Control.Monad.Cont.Trans

main = do
  runContT getContinuation print
  
getContinuation = do
  res <- ajaxCont "index.html" $ HttpRequest {accepts: Html, contentType: Form, method: GET, contents: Nothing}
  handleContent res
  where
    handleContent (Left err) = return "some error"
    handleContent (Right text) = return text
