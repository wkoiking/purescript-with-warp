Sample program for PureScript with Haskell warp server.

[purescript-bridge](https://hackage.haskell.org/package/purescript-bridge) package is used for automatic generation of PureScript types/encoder/decode from Haskell types.

# How to build and run

1. Execute below commands:

~~~
npm install virtual-dom
npm install browserify

spago bundle-app && browserify index.js > main.js

stack build
stack exec purescript-with-warp-exe
~~~

2. Access: http://localhost:8080

# Development with ghci

Using reload command `:r` in ghci and `restart :: IO ()` function defined 
in `app/DevelMain.hs`, you can spare the build time for development. e.g.

~~~
$ stack ghci
*Main CodeGen DevelMain Lib Type> :r
*Main CodeGen DevelMain Lib Type> update
~~~

The `update` command will:

1. auto generate pureScript-src/src/Type.purs file from haskell src/Type.hs.
2. kill the previous server thread in ghci if any, and
3. start the new server thread in ghci
