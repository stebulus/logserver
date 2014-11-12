logserver - A server that logs POSTs
====================================

This is a simple HTTP server which appends all data POSTed to a fixed file.
To build, do

    cabal install --only-dependencies
    cabal configure
    cabal build

Then, to try it out, run

    dist/build/logserver/logserver 3042 mylogfile

and then, in another terminal window,

    echo "log entry" |curl --data-binary @- http://localhost:3042
    cat mylogfile

This server is really just an exercise; it has obvious security weaknesses.
