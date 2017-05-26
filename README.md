# wryte

Pretty-printed source code with semi-automatic indentation

## Introduction

Wryte provides an API for generating textual source code with pretty
indentation and alignment. To achieve this, it tracks source column position,
newlines, and current indentation / alignment internally, and provides a
monadic API to conveniently manage these.

## Installation

The usual; `cabal install wryte`, or add wryte to your stack
extra-dependencies.

## Basic Usage


```haskell
putStrLn . runWryte_ defWryteOptions $ do
  wryteLn "module Main where"
  wryteLn ""
  wryteLn "main = do"
  indented $ do
    wryteLn "putStrLn \"Hello, world!\"
```

Indentation is tracked monadically, such that `indented` acts as a wrapper that
automatically prepends indentation to each new line. Indentations stack up, so
you can safely nest them, and they will do the right thing.


