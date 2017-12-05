packdeps-github
===============

A program to open issues on GitHub repositories when the latest
release on Hackage does not work with the newest version of all its
dependencies.


Usage
-----

```
$ cabal update
$ packdeps-github example.config
dejafu-0.9.1.0 is up-to-date
llvm-general-3.5.1.2 is behind on transformers-0.5.5.0
```

Always run `cabal update` before `packdeps-github`, or dependency
information may be out-of-date.
