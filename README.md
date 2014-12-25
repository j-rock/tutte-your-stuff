# tutte-your-stuff

Using Haskell + OpenGL to demonstrate force-directed graph drawing.

Implemented so far:
* Tutte's algorithm (1963)
* Fruchterman and Reingold's algorithm (1991)
* Walshaw's algorithm (2003)

Credit goes to [this awesome paper] for its splendid pseudocode.

### Installation instructions
You will need
* ghc >= 7.8.2.
* The OpenGL / GLUT C libraries installed (presumably)
* cabal >= 1.20

Now, clone this repo:
```sh
$> cd tutte-your-stuff
$> cabal sandbox init
$> cabal install
```

### Running instructions
While inside the directory (after performing cabal install):
```sh
$> cabal run
```

As the program runs, press:
* A and D to rotate the graph
* Left and Right arrow keys to switch graphs (due to lazy evaluation, you can keep generating new random graphs just by "going" right)
* Up and Down arrow keys to switch planarization methods (the sequence goes Tutte <-> Fruchterman-Reingold <-> Walshaw)
* Space bar to make a single iteration
* Hit return to make the graph autonomously iterate

### Demo
For those who cannot install the app, I put up a video of it running on [Youtube]

[this awesome paper]:https://cs.brown.edu/~rt/gdhandbook/chapters/force-directed.pdf
[Youtube]:http://youtu.be/uTGIY4P7A-8
