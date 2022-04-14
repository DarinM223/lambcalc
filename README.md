
Lambcalc
========

A compiler from an extended variant of lambda calculus to LLVM.

To build and run the example program (in `src-exe/Main.hs`):

First run (in the project folder):

```
cabal build
```

to build the Haskell project. Then run:

```
cabal run lambcalc
```

to compile the example program (`example` in `src-exe/Main.hs`) that calculates the factorial of 5. The compiled LLVM file should be `main.ll` in the project folder. Then run:

```
sh run.sh
```

to compile and run the LLVM file. The output should be `120`.
