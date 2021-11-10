FROM ubuntu:20.04

RUN export DEBIAN_FRONTEND=noninteractive && \
    apt-get update && \
    apt-get install -y --no-install-recommends \
        ca-certificates \
        curl \
        dirmngr \
        g++ \
        git \
        gnupg \
        libsqlite3-dev \
        libtinfo-dev \
        libgmp-dev \
        make \
        netbase \
        openssh-client \
        xz-utils \
        zlib1g-dev \
        \
        file \
        gawk \
        jq \
        pkg-config \
        libsodium-dev \
        libsystemd-dev \
        bc \
        tar \
        xxd && \
    rm -rf /var/lib/apt/lists/*

ARG GHCUP_VERSION=0.1.17.3
ARG GPG_KEY=7784930957807690A66EBDBE3786C5262ECB4A3F

# install ghcup
RUN gpg --batch --keyserver keys.openpgp.org --recv-keys $GPG_KEY && \
    curl -sSfL -O https://downloads.haskell.org/~ghcup/$GHCUP_VERSION/x86_64-linux-ghcup-$GHCUP_VERSION && \
    curl -sSfL -O https://downloads.haskell.org/~ghcup/$GHCUP_VERSION/SHA256SUMS && \
    curl -sSfL -O https://downloads.haskell.org/~ghcup/$GHCUP_VERSION/SHA256SUMS.sig && \
    gpg --verify SHA256SUMS.sig SHA256SUMS && \
    sha256sum -c --ignore-missing SHA256SUMS && \
    mv x86_64-linux-ghcup-$GHCUP_VERSION /usr/bin/ghcup && \
    chmod +x /usr/bin/ghcup && \
    rm -rf SHA256SUMS SHA256SUMS.sig

ARG GHC=8.10.7
ARG CABAL_INSTALL=3.6.2.0

# install haskell toolchain
RUN export GHCUP_CURL_OPTS="--silent" NO_COLOR=1 && \
    ghcup config set gpg-setting GPGStrict && \
    ghcup --verbose install ghc   --isolate=/usr     --force ${GHC} && \
    ghcup --verbose install cabal --isolate=/usr/bin --force ${CABAL_INSTALL} && \
    find "/usr/lib/ghc-${GHC}/" \( -name "*_p.a" -o -name "*.p_hi" \) -type f -delete && \
    rm -r "/usr/share/doc/ghc-${GHC}" && \
    rm -rf /tmp/ghcup*

RUN cabal update

ARG CARDANO_NODE_GIT_REF

# allow building newer cardano-cli
RUN if [ -n "${CARDANO_NODE_GIT_REF}" ] ; then \
        git clone https://github.com/input-output-hk/cardano-node.git && \
        cd cardano-node && \
        git reset --hard ${CARDANO_NODE_GIT_REF} && \
        cabal install -f-external-libsodium-vrf --installdir=/usr/bin --overwrite-policy=always --install-method=copy cardano-cli \
    ; fi

ARG RELEASE=2021-09-29

RUN curl -L -o wallet.tar.gz \
        https://github.com/input-output-hk/cardano-wallet/releases/download/v${RELEASE}/cardano-wallet-v${RELEASE}-linux64.tar.gz && \
    tar xzf wallet.tar.gz && \
    cp cardano-wallet-v${RELEASE}-linux64/bech32 /usr/bin/bech32 && \
    cp cardano-wallet-v${RELEASE}-linux64/cardano-address /usr/bin/cardano-address && \
    if [ -z "${CARDANO_NODE_GIT_REF}" ] ; then cp cardano-wallet-v${RELEASE}-linux64/cardano-cli /usr/bin/cardano-cli ; fi && \
    rm -rf cardano-wallet-v${RELEASE}-linux64/


COPY wallet.sh /usr/bin/wallet.sh
