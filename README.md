# tutte-your-stuff

Using Haskell + OpenGL to demonstrate Tutte's graph drawing algorithm.

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
* a and d to rotate the graph
* <- and -> arrow keys to switch graphs
* space bar to make a single iteration
* return to make the graph automatically iterate.
