FROM haskell:8.8.3
RUN cabal update
RUN apt update && apt install -y libpq-dev build-essential
RUN cabal install morpheus-graphql-0.10.0 postgresql-simple-0.6.2
RUN cabal install scotty-0.11.5 mtl transformers
RUN cabal install aeson base64-bytestring
RUN cabal install utf8-string network-uri uri-encode
ADD . haskell
RUN cd haskell && cabal install --installdir=/usr/bin
ENTRYPOINT ["webserver"]
