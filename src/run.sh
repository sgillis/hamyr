#! /bin/bash
env PATH="./.cabal-sandbox/bin:$PATH" cabal exec yesod devel
