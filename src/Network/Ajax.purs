module Network.Ajax
(ajax,ajaxCont,HttpRequest(..),URL(..),HTTP(..),Accepts(..),RequestContent(..),Method(..), C(..))
where
import Data.Foreign
import Control.Monad.Eff
import Data.Maybe
import Data.Either
import Data.Function
import Control.Monad.Cont.Trans

-- internal type short cuts
type SuccessFn a eff = (a -> (Eff (http :: HTTP | eff) Unit))
type FailFn a eff = (a -> (Eff (http :: HTTP | eff) Unit))
type HttpSettings a = {accepts :: String, contentType :: String, method :: String, contents :: Maybe a}

-- externally imported data types
foreign import data HTTP :: ! 
type C eff = ContT Unit (Eff (http :: HTTP | eff))
type URL = String
data Method = GET | PUT | POST | DELETE
data Accepts = Xml | Json | Script | Html
data RequestContent = Form | Multipart | JsonContent | XmlContent | TextContent
data HttpRequest a = HttpRequest {accepts :: Accepts, contentType :: RequestContent, method :: Method, contents :: Maybe a}

instance methodShow :: Show Method where
  show GET = "GET"
  show PUT = "PUT"
  show POST = "POST"
  show DELETE = "DELETE"

instance acceptsShow :: Show Accepts where
  show Xml = "xml"
  show Json = "json"
  show Script = "script"
  show Html = "html"

instance requestContentShow :: Show RequestContent where
  show Form = "application/x-www-form-urlencoded; charset=UTF-8"
  show Multipart = "multipart/form-data; charset=UTF-8"
  show JsonContent = "application/json; charset=UTF-8"
  show XmlContent = "application/xml; charset=UTF-8"
  show TextContent = "text/plain; charset=UTF-8"

-- FFI to JQuery.ajax, with some Javascript code to sanitise and bridge Puresript -> Javascript.
-- Not externally visible for these reasons
foreign import ajaxImpl
"""function ajaxImpl(url, settings, failFn, successFn){
     return function(){
       settings.data = settings.contents.value0;
       settings.type = settings.method;
       settings.dataType = settings.accepts;
       if(settings.accepts === 'json'){
         settings.headers = {Accept: 'application/json'};
       }else if(settings.accepts === 'xml'){
         settings.headers = {Accept: 'application/xml'};
       }else if(settings.accepts === 'html'){
         settings.headers = {Accept: 'text/html'};
       }else if(settings.accepts === 'script'){
         settings.headers = {Accept: 'application/javascript'};
       }
       // deal with json body
       if(settings.contentType.indexOf('application/json') > -1 && (settings.type === 'PUT' || settings.type === 'POST')){
         settings.data = JSON.stringify(settings.data);
       }

       settings.success = function(s){ return successFn(s)();};
       settings.error = function(a,b,c){ return failFn(c)();};
       jQuery.ajax(url,settings);
    };
  }
""" :: forall a b c eff. Fn4
       URL
       (HttpSettings a)
       (FailFn b eff)
       (SuccessFn c eff)
       (Eff (http :: HTTP | eff) Unit)

reqToSettings :: forall a. HttpRequest a -> HttpSettings a
reqToSettings (HttpRequest req) = {accepts: (show req.accepts), contentType: (show req.contentType), method: (show req.method), contents: req.contents}

-- The two externally visible interfaces of the Ajax interface

-- A traditional "callback" oriented interface
ajax :: forall a b c eff. URL -> HttpRequest a -> (Either b c -> Eff (http :: HTTP | eff) Unit) -> Eff (http :: HTTP | eff) Unit
ajax url req efn = runFn4 ajaxImpl url (reqToSettings req) (efn <<< Left) (efn <<< Right)

-- A continuation based interface that allows you to use the Continuation Monad Transformer to avoid callback hell
ajaxCont :: forall a b c eff. URL -> HttpRequest a -> C eff (Either b c) 
ajaxCont url settings = ContT $ ajax url settings




