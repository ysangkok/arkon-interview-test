FROM haskell:8.8.3
RUN cabal update
RUN apt-get update && apt-get install -y --no-install-recommends --no-install-suggests libpq-dev build-essential postgresql-client-common wget unzip postgis
RUN cabal install --lib morpheus-graphql-0.10.0 postgresql-simple-0.6.2
RUN cabal install --lib scotty-0.11.5 mtl transformers
RUN apt-get install postgresql-client-9.6 apt-utils -y
RUN cabal install scotty-0.11.5 utf8-string network-uri uri-encode
# The dependencies would also be installed by the following
# command, but we install them separately so that we can use
# Docker's caching to avoid recompiling the dependencies
# each time the sources are adjusted.
ADD . haskell
RUN cd haskell && cabal install --installdir=/usr/bin
ADD scripts/arkon-init.sh /usr/bin/arkon-init.sh
ADD scripts/arkon-fetchdata.sh /usr/bin/arkon-fetchdata.sh
ENTRYPOINT ["webserver"]
