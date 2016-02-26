module Main where

import Prelude hiding (div)

import Data.Either
import Data.Maybe (Maybe(..))
import Data.Foreign
import Data.Foreign.Class
import WebSocket
import Control.Monad.Aff
import Network.HTTP.Affjax as A
import Control.Bind ((=<<))
import Control.Monad (when)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class
import Control.Monad.Eff.Var (($=), get)
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Console (CONSOLE(), log)
import Data.List (List(Nil), singleton)

import DOM (DOM())
import Pux
import Pux.DOM.HTML.Elements (div, p, button, text, span, input)
import Pux.DOM.HTML.Attributes (onClick, send, className, KeyboardEvent, onKeyUp, placeholder)
import Pux.Render.DOM (renderToDOM)
import Signal as S
import Signal.Channel (CHANNEL(), Channel, channel, send, subscribe) as S

-- |=================================    ACTIONS      =================================
data Action = ButtonOne | ButtonTwo | ButtonThree | ReceiveAJAXData String | ButtonFour | ReceiveWSData String

-- |=================================    STATE      =================================

data State = State { counter :: Int, banner :: String, socket :: Connection }

initialState :: String -> forall e. Eff (ws :: WEBSOCKET|e) State
initialState url = do
  connection@(Connection ws) <- newWebSocket (URL url) []
  ws.onmessage $= \event -> do
      let received = runMessage (runMessageEvent event)
      log "message received from websocket"
      -- S.send input (singleton (ReceiveWSData "data received from websocket"))
      when (received == "goodbye") do
        log "connection closing on receipt of goodbye note"
        ws.close (Just (Code 1000)) (Just (Reason "none"))
  let state = State { counter: 0, banner: "initial string", socket: connection }
  return state

-- |=================================    Ajax      =================================
data AjaxMsg = AjaxMsg { version :: String, language :: String } -- {"version":"4.2.10092","language":"javax"}

instance showAjaxMsg :: Show AjaxMsg where
  show (AjaxMsg m) = "{ \"version\": \"" ++ m.version ++ "\"language\": \"" ++ m.language ++ "\" }"

instance ajaxMessageIsForeign :: IsForeign AjaxMsg where
  read value = do
    version  <- readProp "version" value
    language <- readProp "language" value
    return $ AjaxMsg { version: version, language: language }

-- |=================================    UPDATE      =================================
update :: forall eff. Update
          (ajax :: A.AJAX, err :: EXCEPTION, console :: CONSOLE, ws :: WEBSOCKET | eff)
          State
          Action
update action (State state) input =
  case action of
    ButtonOne ->
      { state: State state { counter = state.counter + 1 }
      , effects: [ do log "set view to ButtonOne" ] }
    ButtonTwo ->
      { state: State state { counter = state.counter - 1 }
      , effects: [ do log "set view to ButtonTwo" ] }
    ReceiveWSData msg ->
      { state: State state { banner = msg }
      , effects: []
      }
    ReceiveAJAXData msg ->
      { state: State state { banner = msg }
      , effects: [ do log $ "Updated new state: " ++ msg ]
      }
    ButtonThree ->
      { state: State state { banner = "Loading data from server..." }
      , effects: [ doAjaxCall ]
      }
    ButtonFour ->
      { state: State state { banner = "sending message on websocket"}
      , effects: [ do doWebSocketCall state.socket ]
    }
  where
    -- don't know how to write signature for this function!
    doAjaxCall = launchAff $ do
      res <- A.get "http://localhost:8080/version"  -- requires something like json-server running on port 8080
      let response = readJSON res.response :: F AjaxMsg
      liftEff $ case response of
          (Left err) -> log "Error parsing JSON!"
          (Right (AjaxMsg msg)) -> S.send input (singleton (ReceiveAJAXData msg.version))
    doWebSocketCall :: forall e. Connection -> Eff (ws::WEBSOCKET|e) Unit
    doWebSocketCall (Connection ws) =  do ws.send(Message "goodbye")

-- |=================================    VIEW      =================================
view :: State -> VirtualDOM
view (State state) = div ! className "controls" $ do
  p $ text (show state.counter)
  p $ text (show state.banner)
  p ! className "btn-group" $ do
    button ! onClick (send ButtonOne)   <> className "btn btn-primary" $ text "ButtonOne"
    button ! onClick (send ButtonTwo)   <> className "btn btn-info"    $ text "ButtonTwo"
    button ! onClick (send ButtonThree) <> className "btn btn-warning" $ text "ButtonThree"
  span $ text " "
  p ! className "btn-group" $ do
    button ! onClick (send (ReceiveAJAXData "Detail pressed" ))
          <> className "btn btn-xs btn-success" $ text "Detail"
    button ! onClick (send ButtonFour) <> className "btn btn-xs btn-info" $ text "Socket"

-- |=================================    MAIN      =================================
main :: forall e. Eff ( ws::WEBSOCKET
                      , channel::S.CHANNEL
                      , dom::DOM
                      , ajax::A.AJAX
                      , err::EXCEPTION
                      , console::CONSOLE | e ) Unit
main = do
  appState <- initialState "ws://echo.websocket.org" -- forall e. Eff (ws :: WEBSOCKET|e) State
  wsInput <- S.channel (ReceiveWSData "foo")
  S.send wsInput ((ReceiveWSData "yay") :: Action)
  let wsSignal = S.subscribe wsInput :: S.Signal Action
  renderToDOM "#app" =<< app
    { state: appState
    , update: update
    , view: view
    , inputs: [wsSignal]
    }
