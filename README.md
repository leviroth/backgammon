# Backgammon

![backgammon](https://leviroth.github.io/backgammon/images/game.png)

This is backgammon, implemented in OCaml. You have the option of playing via a
multiplayer web client or else locally on a terminal.

# Installation

For the command-line interface, you will want at least Jane Street Base, stdio,
and ppx_jane, installable via:

```bash
opam install base stdio ppx_jane
```

Running tests requires `core_kernel`.

For the web interface, substantially more is needed:

```bash
opam install core async websocket-async js_of_ocaml
# The following are either not yet published to opam, or not up to date with the
# latest master branch bugfixes:
opam pin add ocaml-vdom git://github.com/lexifi/ocaml-vdom.git
opam pin add gen_js_api git://github.com/lexifi/gen_js_api.git
```

# Build and usage

## Command-line interface

![cli](https://leviroth.github.io/backgammon/images/cli_game.png)

The command-line interface can be built and run with:

```bash
jbuilder build cli/main.exe
_build/default/cli/main.exe
```

Moves are entered by choosing a starting location and the die that should be
used from that position.

## Web interface

To build the web interface, simply:

```bash
jbuilder build @game
```

We make the game playable by running the server executable, and separately
serving static files from the `client/` build directory:

```bash
_build/default/server/main.exe

# In a separate session:
cd _build/default/client
python3 -m http.server
```

The game can then be accessed on whatever port is serving the web client. If
you want to play over the Internet, you will need to make sure that your friends
can access this port, as well as port 3000 which is used for WebSockets.
