FROM benz0li/ghc-musl:9.6

WORKDIR /app

COPY . .

RUN cabal update && cabal install --only-dependencies

CMD ["cabal", "build", "aoc", "--enable-executable-static"]

