FROM debian:bookworm-slim AS cabal-downloader
RUN apt-get update && apt-get install -y --no-install-recommends ca-certificates curl xz-utils \
 && rm -rf /var/lib/apt/lists/*

ARG CABAL_VER=3.10.1.0
RUN set -eux; \
  curl -fsSL -o /tmp/cabal.tar.xz \
    "https://downloads.haskell.org/~cabal/cabal-install-${CABAL_VER}/cabal-install-${CABAL_VER}-x86_64-linux-deb9.tar.xz"; \
  mkdir -p /tmp/unpack /out; \
  tar -xJf /tmp/cabal.tar.xz -C /tmp/unpack; \
  CABAL_BIN="$(find /tmp/unpack -type f -name cabal -perm -111 | head -n 1)"; \
  test -n "$CABAL_BIN"; \
  install -m 0755 "$CABAL_BIN" /out/cabal; \
  /out/cabal --version

FROM haskell:8.4

COPY --from=cabal-downloader /out/cabal /opt/cabal/bin/cabal
ENV PATH="/opt/cabal/bin:${PATH}"

RUN cabal update

COPY . .

RUN cabal build all


