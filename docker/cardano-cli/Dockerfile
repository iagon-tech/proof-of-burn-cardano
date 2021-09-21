FROM alpine:3.14

ARG RELEASE=2021-09-09

RUN apk add --no-cache openssl file gawk jq bash curl tar gzip bc

RUN curl -L -o wallet.tar.gz https://github.com/input-output-hk/cardano-wallet/releases/download/v${RELEASE}/cardano-wallet-v${RELEASE}-linux64.tar.gz && \
	tar xzf wallet.tar.gz && \
	cp cardano-wallet-v${RELEASE}-linux64/bech32 /usr/bin/bech32 && \
	cp cardano-wallet-v${RELEASE}-linux64/cardano-address /usr/bin/cardano-address && \
	cp cardano-wallet-v${RELEASE}-linux64/cardano-cli /usr/bin/cardano-cli && \
	rm -rf cardano-wallet-v${RELEASE}-linux64/

COPY wallet.sh /usr/bin/wallet.sh
