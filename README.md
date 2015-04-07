<h1 align="center">
    <a href="https://github.com/tonyday567/trade">
        trade
    </a>
</h1>

<hr>

data types useful for traded securities


## Develop

The libray requires a bespoke version of the mvc library which can be found <a href="https://github.com/tonyday567/Haskell-MVC-Library"> in the `dev` branch. It also requires a few utility repos: <a href="https://github.com/tonyday567/mvc-extended">, <a href="https://github.com/tonyday567/time-extended">, <a href="https://github.com/tonyday567/pipes-extended">, <a href="https://github.com/tonyday567/foldl-extended"> and <a href="https://github.com/tonyday567/sfold">.

``` sh
$ git clone https://github.com/tonyday567/mvc-extended.git
$ git clone https://github.com/tonyday567/time-extended.git
$ git clone https://github.com/tonyday567/pipes-extended.git
$ git clone https://github.com/tonyday567/foldl-extended.git
$ git clone https://github.com/tonyday567/sfold.git
$ git clone https://github.com/tonyday567/Haskell-MVC-Library.git
$ cd Haskell-MVC-Library && git checkout dev && cd ..
$ git clone https://github.com/tonyday567/trade.git
$ cd trade
$ cabal sandbox init
$ cabal sandbox add-source ../mvc-extended
$ cabal sandbox add-source ../time-extended
$ cabal sandbox add-source ../pipes-extended
$ cabal sandbox add-source ../foldl-extended
$ cabal sandbox add-source ../sfold
$ cabal sandbox add-source ../Haskell-MVC-Library
$ cabal configure
$ cabal install --dependencies-only --dry-run
$ cabal install --dependencies-only
$ cabal build
```
