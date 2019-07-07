A contrived purerl example
==

- This app is written in Purescript on the frontend, and Purescript on the backend (using Purerl + Stetson + Pinto)
- This app demonstrates basic CRUD 
- This app demonstrates an extremely basic UI using view models from the backend
- This app demonstrates a basic SPA with client-side navigation/etc using Halogen
- This app demonstrates a contrived use of a gen server
- This app demonstrates some very basic interop with Erlang on the backend

That is it, it isn't best practises and it isn't at all useful - but it's a good template for new apps that actually do something real.

Running the example
==

``` bash

rebar3 compile
./run

```

Open a browser to http://localhost:3000, enjoy the contrived example

Disclaimer
==

This software, and the opinionated libraries written to support it are very much "works in progress" - we are actively using and building these libraries out for use in own commercial software and can and will be making any changes required to further support that development. As such, they come without support and a disclaimer very much of "be it on your own heads". That said - feel free to reach out and talk to us if you have ideas though, improvements and suggestions are welcome in pull requests and conversation.
