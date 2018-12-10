# RLP: Recursive Length Prefix

[![Build Status](https://travis-ci.org/Jasagredo/RLP.svg?branch=master)](https://travis-ci.org/Jasagredo/RLP)


This package implements the Recursive Length Prefix protocol defined
in the [Ethereum Yellow Paper](https://ethereum.github.io/yellowpaper/paper.pdf)
used for serializing and deserializing structures into an array of bytes.

For more information, please check the haddock documentation (can be generated running `cabal haddock` on the root of the repo).

## Installation

On 1.1.0 error handling was coded as a safe way to fail on the decoding through Eithers. Usage of >=1.1 is quite advised.

Installation can be easily done through `cabal` as the package is in Hackage. `cabal install RLP` should be enough.

## Contribuition

Feel free to contribute. I think the error handling part could be improved.