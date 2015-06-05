#  fivequeens

This is the Haskell code for probing the DTM50 table for kqqqqqk.

I have tried to minimize use of dependencies and language features,
but it does require Conduit packages.  If you have the Haskell Platform
and Cabal, this should suffice:

```Shell
cabal install conduit-extra lzma-conduit
ghc --make -O2 query5q.hs
```

To query the table, you need the table, which is currently hosted at
http://www.filedropper.com/fivequeens and is about 48 MiB.  It should
be in the directory you run the executable from.

It takes one argument, a concatenation of the squares of the White
king, the four or five White queens, and the Black king, followed by
whose move it is as `b` or `w`.  Examples:

```Shell
runghc query5q.hs c4a6b5c8h7h8e3w
./query5q b3a1a2b1f1f2b7b
runhaskell query5q.hs d4a7b4c7d8e7h3w
```

Note that it will return values for four queens only with White
to move.

For an explanation of what DTM50 is, see my article [Endgame tablebases
with the fifty-move rule](http://galen.metapath.org/egtb50/).

