module Ajax
(jsonGet,jsonDelete,jsonPut,jsonPost,URL(..),SuccessFn(..),FailFn(..),HTTP(..))
where
import Data.Foreign
import Control.Monad.Eff
import Data.Maybe
-- An effect type which indicates DOM manipulation
foreign import data DOM :: !

foreign import data HTTP :: ! 
-- The jQuery wrapper type

type ContentType = String
type Accepts = String
type URL = String
type Method = String

type SuccessFn a eff = (a -> (Eff (http :: HTTP | eff) Unit))
type FailFn a eff = (a -> (Eff (http :: HTTP | eff) Unit))

type HttpSettings a = {accepts :: Accepts, contentType :: ContentType, method :: Method, contents :: Maybe a}

jsonGet :: forall a b c eff. URL -> Maybe a -> FailFn b eff -> SuccessFn c eff -> Eff (http :: HTTP | eff) Unit
jsonGet url params fail succ = ajax url {accepts: "application/json", contentType: "application/x-www-form-urlencoded; charset=UTF-8", method: "GET", contents: params} fail succ

jsonDelete :: forall a b c eff. URL -> Maybe a -> FailFn b eff -> SuccessFn c eff -> Eff (http :: HTTP | eff) Unit
jsonDelete url params fail succ = ajax url {accepts: "application/json", contentType: "application/x-www-form-urlencoded; charset=UTF-8", method: "DELETE", contents: params} fail succ

jsonPut :: forall a b c eff. URL -> a -> FailFn b eff -> SuccessFn c eff -> Eff (http :: HTTP | eff) Unit
jsonPut url params fail succ = ajax url {accepts: "application/json", contentType: "application/json", method: "PUT", contents: Just params} fail succ

jsonPost :: forall a b c eff. URL -> a -> FailFn b eff -> SuccessFn c eff -> Eff (http :: HTTP | eff) Unit
jsonPost url params fail succ = ajax url {accepts: "application/json", contentType: "application/json", method: "POST", contents: Just params} fail succ

foreign import ajax
"""function ajax(url){
     return function(settings){
       return function(successFn){
         return function(failFn){
           return function(){
             settings.data = settings.contents;
             settings.type = settings.method;
             settings.success = function(s){ return successFn(s)();};
             settings.error = function(a,b,c){ return failFn(c)();};
             jQuery.ajax(url,settings);
           };
         };
       };
     };
   }
""" :: forall a b c eff. URL -> HttpSettings a -> FailFn b eff -> SuccessFn c eff -> Eff (http :: HTTP | eff) Unit

