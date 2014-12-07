<a href="https://travis-ci.org/jswebtools/language-ecmascript"><img src="https://travis-ci.org/jswebtools/language-ecmascript.svg">Build status</img></a>

# Overview

Language-ecmascript is a Haskell library for parsing, transforming and pretty-printing ECMAScript (popularly known as JavaScript) source code. It is geared towards program analysis and transformation, but has also been used as a backend for compilers targeting JavaScript. The library aims to be 100% standards-compatible with an extensive test-suite and documentation. It is part of both Hackage and Stackage Haskell package databases. Language-ecmascript is licenced under the 3-clause BSD license (see LICENSE for details).

The library has started as WebBits at Brown University, and many people have contributed to it since. See language-ecmascript.cabal for the list of contributors and copyright information.

# Versioning

The library follows Semantic Versioning (http://semver.org) for its version numbers. The version is a triple of numbers MAJOR.MINOR.PATCH, where 
* MAJOR is incremented only on backwards-incompatible and reverse-dependency-breaking changes,
* MINOR is incremented on any additional backwards-compatible features,
* PATCH is incremented on any other changes, mostly bug/build-fixes.

If your publicly-released package depends on this library you are strongly encouraged to restrict allowed versions to at least the MAJOR version you are using.

# Contributing

Contributions are welcome, provided they are in agreement with the terms of the BSD3 license. Generally, any non-trivial (beyond formatting, spelling or simple bug/build fixes) contribution is to be reflected in the list of contributors. The preferred method of contribution is via pull requests. If you intend to contribute a lot, after your first pull request is accepted, you can get direct commit rights to the repository. As a contributor you are expected to follow the general formatting style and document your efforts in the source code comments and in the issue discussion.

If you would like to contribute, here's how you can get started:

* Read the Roadmap wiki page (https://github.com/jswebtools/language-ecmascript/wiki/Roadmap) to get an idea of where the project is heading and where the current efforts are focused.
* Head to the issues list (https://github.com/jswebtools/language-ecmascript/issues), read through the issues and pick an issue to work on. If someone is already working on it, contact them first to make sure you are not stepping on each other's toes.
* (Optional) Send an e-mail to the maintainer, Andrey Chudnov <oss@chudnov.com>, to let him know what you are planning to work on and ask any questions you have about the problem or the code base.
