packdeps-github
===============

A program to open issues on GitHub repositories when the latest
release on Hackage does not work with the newest version of all its
dependencies.


Usage
-----

```
$ cabal update
$ packdeps-github example.yaml
dejafu-0.9.1.0 is up-to-date
llvm-general-3.5.1.2 is behind on transformers-0.5.5.0
    opened issue #2
```

Always run `cabal update` before `packdeps-github`, or dependency
information may be out-of-date.

If an issue already exists, a new one won't be created:

```
$ packdeps-github example.yaml
dejafu-0.9.1.0 is up-to-date
llvm-general-3.5.1.2 is behind on transformers-0.5.5.0
    pre-existing issue found: #2
```

This is the case even if the issue is closed.  Only if a new version
of the package is pushed to Hackage, and falls behind its
dependencies, will a new issue will be opened.
