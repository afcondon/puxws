# NB - repo is not currently in a working state - problem with duplicate row effects for EXCEPTIONs

# puxws
Standalone example of purescript-pux using purescript-websocket-simple

## What's here

Simple variation on the purescript-pux `Affjax` example with an integration of websockets in a similar manner.

## Basic approach

I've put the websocket connection handler into the State of the Pux application, so that the `forall e. Eff ( ws :: WEBSOCKET | e)`
can be available inside the `update` function that is handling the `Actions`. 

Then, in the `main`, i make a `channel` for the type of action that is going to be sent in response to data received on the websocket. That channel is used in the construction of the inital state, setting up the `onmessage` for the websocket to send a `ReceiveWSData` action in that channel.

Next i `subscribe` a Signal to this Channel and i use that Signal in the config of the Pux app.

Upshot of this is that the rightmost button, marked "Socket", when clicked generates an Action `ButtonFour` which in turn sends a message on a websocket to `ws://echo.websocket.org` from whence it echoes back, causing a new Action `ReceiveWSData` which simply puts the received string into the DOM (State.banner) so that we can see it.

