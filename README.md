Lambency [![Build Status](https://travis-ci.org/Mokosha/Lambency.svg?branch=master)](https://travis-ci.org/Mokosha/Lambency)
========

A Real-Time Rendering framework written in Haskell

Major Dependencies
--------

- [GLFW-b](http://hackage.haskell.org/package/GLFW-b) -- [github](https://github.com/bsl/GLFW-b)
- [OpenGL](http://hackage.haskell.org/package/OpenGL) -- [github](https://github.com/haskell-opengl/OpenGL)
- [OpenAL](http://hackage.haskell.org/package/OpenAL) -- [github](https://github.com/haskell-openal/OpenAL)
- [JuicyPixels](http://hackage.haskell.org/package/JuicyPixels) -- [github](https://github.com/Twinside/Juicy.Pixels)
- [HCodecs](http://hackage.haskell.org/package/HCodecs) -- [github](https://github.com/giorgidze/HCodecs)
- [Netwire 5](http://hackage.haskell.org/package/netwire) -- [darcs](http://hub.darcs.net/ertes/netwire)
- [Linear](http://hackage.haskell.org/package/linear) -- [github](https://github.com/ekmett/linear)

Try the Example:
--------

From the source directory:

    cabal install --user
    cd Examples
    cabal configure
    cabal build
    ./dist/build/lambcubedemo/lambcubedemo
