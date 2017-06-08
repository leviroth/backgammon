The goal of this project is to provide a networked multiplayer implementation of
backgammon. Part of that goal means getting all the rules right; not just the
basic movement rules but also all the weird corner cases. For example, the game
will check that you use all available dice (where possible) and that you use the
greater of two dice if forced to use only one, but it will (correctly) allow you
to use less than the full value of a die when bearing off, even if you could
avoid this by using your other dice differently.

Currently, the game is playable, with a basic command-line interface that is not
especially user-friendly but should correctly implement the game. In the longer
run, I aim to complement the core game engine with a user interface built in
Bucklescript, which will be a nice excuse to learn that tool.