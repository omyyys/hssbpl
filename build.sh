set -xe

ghc -no-keep-hi-files -no-keep-o-files --make src/Main.hs -isrc -o hssbpl 
