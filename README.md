# tutte-your-stuff

Using Haskell + OpenGL to demonstrate planar graph drawing methods.

Implemented so far:
* Tutte's algorithm
* Fruchterman and Reingold's algorithm

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
