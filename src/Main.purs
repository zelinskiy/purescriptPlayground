module Main where

import Prelude hiding (append)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.JQuery (JQuery, JQueryEvent,
  on, append, css, create, appendText,
  body, ready, setText, getValue)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Aff (Canceler, launchAff)
import Data.Foldable (for_)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import DOM (DOM)
import Partial.Unsafe (unsafePartial)
import Network.HTTP.Affjax (AJAX, affjax, defaultRequest)
import Data.Foreign.Class (read)

sendDataOnServer :: forall eff.Eff (
        "err" :: EXCEPTION
        , "ajax" :: AJAX
        , "console" :: CONSOLE
        | eff
        )
        (Canceler
           ( "ajax" :: AJAX
           , "console" :: CONSOLE
           | eff
           )
        )
sendDataOnServer = launchAff $ do
  res <- affjax $ defaultRequest { url = "http://ip.jsontest.com/", method = Left GET }
  liftEff $ log $ "GET /api response2: " <> res.response


handleChange :: forall eff.
  JQuery
  -> JQuery
  -> JQueryEvent
  -> JQuery
  -> Eff ( dom :: DOM
         , console :: CONSOLE
         , ajax :: AJAX
         , err :: EXCEPTION
         | eff
         ) Unit
handleChange input greeting _ _ = unsafePartial do
  val <- getValue input
  for_ (runExcept (read val)) \name -> do
    log $ "Name changed to " <> name
    setText ("Hello, " <> name) greeting

main :: forall eff.
  Eff ( dom :: DOM
      , console :: CONSOLE
      , ajax :: AJAX
      , err :: EXCEPTION
      | eff
      ) Unit
main = do
  ready $ do
    sendDataOnServer

    -- Get the document body
    body <- body

    -- Create a text box
    div   <- create "<div>"
    input <- create "<input>"
    appendText "Your Name: " div
    append input div
    append div body

    -- Create a paragraph to display a greeting
    greeting <- create "<p>"
    css { color: "red" } greeting
    append greeting body

    -- Listen for change events on the text box
    on "change" (handleChange input greeting) input
