A collection of Purerl examples
==

- This app is written in Purescript on the frontend, and Purescript on the backend (using Purerl + Stetson + Pinto)
- This app demonstrates an extremely basic UI using view models from the backend
- This app demonstrates a basic SPA with client-side navigation/etc using Halogen
- This app demonstrates a contrived use of a gen server
- This app demonstrates REST handlers
- This app demonstrates Streaming handlers
- This app demonstrates WebSocket communication
- This app demonstrates the use of a message bus
- This app demonstrates the use of Monitors
- This app demonstrates some very basic interop with Erlang on the backend

That is it,  it is a grab bag of "how do I do this particular" thing and not a real application in any sense of the word.

See the [Purerl Cookbook](https://purerl-cookbook.readthedocs.io/en/latest/) for more context and more of a guided 'how to'.

Running the example
==

Note: Docker is needed in the run scripts presently, as it spins up Redis for the CRUD example.

``` bash

rebar3 release 
./run

```

Note: Release only needs doing once, after that you can use *rebar3 compile* because the relevant artifacts will have been created.

- Open a browser to http://localhost:3000, enjoy the most basic experience.
- curl -v http://localhost:3000/api/stream > /dev/null # Subscribe to a stream (check the logs)
- curl -v http://localhost:3000/api/events/stream # Stream the events happening on the server as you do things in the UI

Disclaimer
==

This software, and the opinionated libraries written to support it are very much "works in progress" - we are actively using and building these libraries out for use in own commercial software and can and will be making any changes required to further support that development. As such, they come without support and a disclaimer very much of "be it on your own heads". That said - feel free to reach out and talk to us if you have ideas though, improvements and suggestions are welcome in pull requests and conversation.
