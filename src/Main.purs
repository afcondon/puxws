module Main where

import Prelude hiding (div)

import Data.Either (Either(Right, Left))
import Data.Foreign (F)
import Data.Foreign.Class (class IsForeign, readJSON, readProp)
import WebSocket (WEBSOCKET, Connection(Connection), Message(Message), URL(URL), runMessageEvent, runMessage, newWebSocket)
import Control.Monad.Aff (Aff, later')
import Network.HTTP.Affjax as A
import Control.Apply ((*>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Var (($=))
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Console (CONSOLE(), log)

import Pux (EffModel, start, renderToDOM, noEffects, CoreEffects, App)
import Pux.Html (Html, div, p, button, text, span)
import Pux.Html.Attributes (className)
import Pux.Html.Events (onClick)
import Signal (Signal) as S
import Signal.Channel (Channel, channel, send, subscribe) as S

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

type State = { counter :: Int, banner :: String, socket :: Connection }

initialState :: S.Channel Action -> String -> forall e. Eff (err :: EXCEPTION, ws :: WEBSOCKET|e) State
initialState chan url = do
  connection@(Connection ws) <- newWebSocket (URL url) []
  ws.onmessage $= \event -> do
      let received = runMessage (runMessageEvent event)
      log "message received from websocket"
      S.send chan ((ReceiveWSData received) :: Action)
  let state = { counter: 0, banner: "initial string", socket: connection }
  pure state

-- |=================================    Ajax      =================================
data AjaxMsg = AjaxMsg { version :: String, language :: String } -- {"version":"4.2.10092","language":"javax"}

instance showAjaxMsg :: Show AjaxMsg where
  show (AjaxMsg m) = "{ \"version\": \"" <> m.version <> "\"language\": \"" <> m.language <> "\" }"

instance ajaxMessageIsForeign :: IsForeign AjaxMsg where
  read value = do
    version  <- readProp "version" value
    language <- readProp "language" value
    pure $ AjaxMsg { version: version, language: language }

-- |=================================    UPDATE      =================================
update :: forall e. Action -> State -> EffModel State Action
            (ajax :: A.AJAX, console :: CONSOLE, ws :: WEBSOCKET, err :: EXCEPTION | e)
update ButtonOne state =
      { state: state { counter = state.counter + 1 }
      , effects: [ liftEff $ log "set view to ButtonOne" *> pure Nop ] }
update ButtonTwo state =
      { state: state { counter = state.counter - 1 }
      , effects: [ liftEff $ log "set view to ButtonTwo" *> pure Nop ] }
update (ReceiveWSData msg) state = noEffects $ state { banner = msg }
update (ReceiveAJAXData msg) state =
      { state: state { banner = msg }
      , effects: [ liftEff $ log ("Updated new state: " <> msg) *> pure Nop ]
      }
update ButtonThree state =
      { state: state { banner = "Loading data from server..." }
      , effects: [ doAjaxCall ]
      }
update ButtonFour state =
      { state: state
      , effects: [ doWebSocketCall state.socket ]
      }
update Nop state = noEffects $ state

doAjaxCall :: forall e. Aff (console::CONSOLE,ajax::A.AJAX|e) Action
doAjaxCall = later' 1500 $ do
  res <- A.get "http://localhost:8080/version"  -- requires something like json-server running on port 8080
  let response = readJSON res.response :: F AjaxMsg
  case response of
    (Left err) -> do
      liftEff $ log "Error parsing JSON!"
      pure Nop
    (Right (AjaxMsg msg)) -> pure $ ReceiveAJAXData msg.version

doWebSocketCall :: forall e. Connection -> Aff (err :: EXCEPTION, ws :: WEBSOCKET | e) Action
doWebSocketCall (Connection ws) =
  liftEff $ ws.send(Message "button four sends this message") *> pure Nop

-- |=================================    VIEW      =================================

view :: State -> Html Action
view state =
  div [className "controls"]
  [ p [] [text (show state.counter)]
  , p [] [text (show state.banner)]
  , p [className "btn-group"]
    [ button [onClick (const ButtonOne),   className "btn btn-primary"] [text "ButtonOne"]
    , button [onClick (const ButtonTwo),   className "btn btn-info"]    [text "ButtonTwo"]
    , button [onClick (const ButtonThree), className "btn btn-warning"] [text "ButtonThree"]
    ]
  , span [] [text " "]
  , p [className "btn-group"]
    [ button [onClick (const ButtonFour), className "btn btn-xs btn-info"] [text "Socket"]
    ]
  ]

-- |=================================    MAIN      =================================
type AppEffects = (console :: CONSOLE, ws :: WEBSOCKET, ajax :: A.AJAX)
main :: State -> Eff (CoreEffects AppEffects) (App State Action)
main state = do
  wsInput <- S.channel Nop
  appState <- initialState wsInput "ws://echo.websocket.org" -- forall e. Eff (ws :: WEBSOCKET|e) State
  let wsSignal = S.subscribe wsInput :: S.Signal Action
  app <- start
    { initialState: appState
    , update: update
    , view: view
    , inputs: [wsSignal]
    }

  renderToDOM "#app" app.html

  pure app
