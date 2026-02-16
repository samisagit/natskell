FROM haskell:9.12

RUN apt-get update \ 
 && apt-get install -y --no-install-recommends pkg-config zlib1g-dev \ 
 && rm -rf /var/lib/apt/lists/*

RUN cabal update

COPY . .

RUN cabal build all

