# puxws
Standalone example of purescript-pux using purescript-websocket-simple

## What's here

Simple variation on the purescript-pux `Affjax` example with some attempt at integrating websockets in a similar manner.

## Basic approach

I've tried putting the websocket connection handler into the State of the Pux application, so that the `forall e. Eff ( ws :: WEBSOCKET | e)`
can be available inside the `update` function that is handling the `Actions`. 

## What works

`bower install` and `pulp serve` should pretty much work to run it but *NB* it's a work in progress - i still haven't worked
out how to integrate the callback on receipt of data from the websocket. 

## What doesn't work

As mentioned above - haven't yet worked out how to set up websocket so that it's handler (ie `ws.onmessage`) is able to `send`
on the `Signal` for the app. 

It might very well be that i need to create a separate `Signal` and add it in the `inputs` part of the app config in the `main`
