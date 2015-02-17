A Haskell implemention of the Abalone game engine.

It is not currenty being maintained and the code almost certainly has some bugs.
The JSON specification for games has changed since this code was written.
The Haskell code uses a tuple (Int, Int) to represent a Hex position.
The canonical Go implementation, and the associated Typescript frontend, use a {q: Int, r: Int} implementation.
If you want to use the Haskell code in your AI, you will need to update the data type.