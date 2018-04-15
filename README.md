Lambency [![Build Status](https://travis-ci.org/Mokosha/Lambency.svg?branch=master)](https://travis-ci.org/Mokosha/Lambency)
========

A Real-Time Rendering framework and game engine

Major Dependencies
--------

To get a sense of the technologies used by this library, the major dependencies are

- [GLFW-b](http://hackage.haskell.org/package/GLFW-b) -- [github](https://github.com/bsl/GLFW-b)
- [OpenGL](http://hackage.haskell.org/package/OpenGL) -- [github](https://github.com/haskell-opengl/OpenGL)
- [OpenAL](http://hackage.haskell.org/package/OpenAL) -- [github](https://github.com/haskell-openal/OpenAL)
- [JuicyPixels](http://hackage.haskell.org/package/JuicyPixels) -- [github](https://github.com/Twinside/Juicy.Pixels)
- [HCodecs](http://hackage.haskell.org/package/HCodecs) -- [github](https://github.com/giorgidze/HCodecs)
- [Netwire 5](http://hackage.haskell.org/package/netwire) -- [github](https://github.com/esoeylemez/netwire)
- [Linear](http://hackage.haskell.org/package/linear) -- [github](https://github.com/ekmett/linear)
- [FreeType](https://hackage.haskell.org/package/freetype2) -- [github](https://github.com/dagit/freetype2)

Try the examples:
--------

From the source directory:

    stack init --solver
    stack build --flag lambency:examples
    stack exec lambcubedemo
