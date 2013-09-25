# Euchre Simulator

This program is an early-stage Euchre simulator that I put together in Haskell
as a learning exercise. I wrote it a couple of months ago, and there are already
things I would change so don't take this as an example of best practises!

At this point you can't play the game, but you can run simulations of valid
Euchre games in the program. Let me know what you think if you try it out or
build something on top of it!

# Use

If you're on a recent cabal, you can run:

    cabal repl
    λ: dealAndPlay

This returns a data structure representing an entire random game.

# License

MIT, see LICENSE.txt.