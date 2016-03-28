module Main where

import Prelude hiding ((#), div)

import Data.Either
import Data.Foreign
import Data.Foreign.Class
import WebSocket
import Control.Monad.Aff
import Network.HTTP.Affjax as A
import Control.Apply ((*>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class
import Control.Monad.Eff.Var (($=))
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Console (CONSOLE(), log)

import Pux (EffModel, start, renderToDOM)
import Pux.Html (Html, (#), (!), div, p, button, text, span)
import Pux.Html.Attributes (className)
import Pux.Html.Events (onClick, onKeyUp)
import Signal as S
import Signal.Channel (CHANNEL(), Channel, channel, send, subscribe) as S

-- |=================================    ACTIONS      =================================
data Action
  = ButtonOne
  | ButtonTwo
  | ButtonThree
  | ReceiveAJAXData String
  | ButtonFour
  | ReceiveWSData String
  | Nop

-- |=================================    STATE      =================================

data State = State { counter :: Int, banner :: String, socket :: Connection }

initialState :: S.Channel Action -> String -> forall e. Eff (ws :: WEBSOCKET|e) State
initialState chan url = do
  connection@(Connection ws) <- newWebSocket (URL url) []
  ws.onmessage $= \event -> do
      let received = runMessage (runMessageEvent event)
      log "message received from websocket"
      S.send chan ((ReceiveWSData received) :: Action)
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
update :: forall e. Action -> State -> EffModel State Action
            (ajax :: A.AJAX, console :: CONSOLE, ws :: WEBSOCKET | e)
update action (State state) =
  case action of
    ButtonOne ->
      { state: State state { counter = state.counter + 1 }
      , effects: [ liftEff $ log "set view to ButtonOne" *> return Nop ] }
    ButtonTwo ->
      { state: State state { counter = state.counter - 1 }
      , effects: [ liftEff $ log "set view to ButtonTwo" *> return Nop ] }
    ReceiveWSData msg ->
      { state: State state { banner = msg }
      , effects: []
      }
    ReceiveAJAXData msg ->
      { state: State state { banner = msg }
      , effects: [ liftEff $ log ("Updated new state: " ++ msg) *> return Nop ]
      }
    ButtonThree ->
      { state: State state { banner = "Loading data from server..." }
      , effects: [ doAjaxCall ]
      }
    ButtonFour ->
      { state: State state
      , effects: [ doWebSocketCall state.socket ]
      }
    Nop -> { state: State state, effects: [] }
  where
    -- don't know how to write signature for this function!
    doAjaxCall = later' 1500 $ do
      res <- A.get "http://localhost:8080/version"  -- requires something like json-server running on port 8080
      let response = readJSON res.response :: F AjaxMsg
      case response of
        (Left err) -> do
          liftEff $ log "Error parsing JSON!"
          return Nop
        (Right (AjaxMsg msg)) -> return $ ReceiveAJAXData msg.version
    doWebSocketCall :: forall e. Connection -> Aff (ws :: WEBSOCKET | e) Action
    doWebSocketCall (Connection ws) =
      liftEff $ ws.send(Message "button four sends this message") *> return Nop

-- |=================================    VIEW      =================================
view :: State -> Html Action
view (State state) = div ! className "controls" # do
  p # text (show state.counter)
  p # text (show state.banner)
  p ! className "btn-group" # do
    button ! onClick (const ButtonOne)   ! className "btn btn-primary" # text "ButtonOne"
    button ! onClick (const ButtonTwo)   ! className "btn btn-info"    # text "ButtonTwo"
    button ! onClick (const ButtonThree) ! className "btn btn-warning" # text "ButtonThree"
  span # text " "
  p ! className "btn-group" # do
    button ! onClick (const ButtonFour) ! className "btn btn-xs btn-info" # text "Socket"
  where bind = Pux.Html.bind

-- |=================================    MAIN      =================================
main :: forall e. Eff ( ws::WEBSOCKET
                      , channel::S.CHANNEL
                      , ajax::A.AJAX
                      , err::EXCEPTION
                      , console::CONSOLE | e ) Unit
main = do
  wsInput <- S.channel (ReceiveWSData "foo")
  appState <- initialState wsInput "ws://echo.websocket.org" -- forall e. Eff (ws :: WEBSOCKET|e) State
  let wsSignal = S.subscribe wsInput :: S.Signal Action
  app <- start
    { initialState: appState
    , update: update
    , view: view
    , inputs: [wsSignal]
    }

  renderToDOM "#app" app.html
