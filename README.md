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

At the moment, the library is undergoing pretty massive an unstable API changes. As such, there isn't really a good "workflow". However, I will try to keep the `stack.yaml` in the root directory up to date such that the project builds from HEAD. If the stack.yaml is not present, then it should be generated via

    stack init . --solver
    
If it is present, then you can simply continue with building and executing the examples:
    
    stack build --flag lambency:examples
    stack exec lambcubedemo
