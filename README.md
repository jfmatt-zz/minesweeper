# Minesweeper

It's minesweeper... in Haskell!

Features:
* IO monad completely isolated in UI functions
* Generates and plays a game of Minesweeper
* Automatically reveals fields of 0s like the Windows version

Todo:
* I feel like it could be more "Haskell-y" - the fact that I didn't use any monads beyond IO worries me. The RNG handling in placeBombs could definitely be rewritten to use a State StdGen
* More point-free - the definition of buildBoard bothers me, for example
