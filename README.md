# Chess

Currently you can play chess agains an "engine" which makes a random legal move each turn.
It's likely there are bugs. If you find any please let me know.

The goal of this project is to make a good chess engine.
The current GUI exists mainly for debugging, hence the lack of polish.

# Build instructions

The program can be built with:
```console
cabal build
```
and then run with:
```console
cabal run
```

The GUI uses gloss which requires GLUT.
On artix the extra/freeglut package seems to work.

