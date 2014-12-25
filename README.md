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

Now, clone this directory:
```sh
$> cd tutte-your-stuff
$> cabal sandbox init
$> cabal install
```

### Running instructions
To actually run the program, execute:
```sh
$> cabal run
```

While you're inside the program, press:
* A and D to rotate the graph
* Left and Right arrow keys to switch graphs
* Up and Down arrow keys to switch planarization methods
* Space bar to make a single iteration
* Return to make the graph autonomously iterate

[this awesome paper]:https://cs.brown.edu/~rt/gdhandbook/chapters/force-directed.pdf
