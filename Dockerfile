# Build stage
FROM fukamachi/sbcl:latest-alpine AS builder

WORKDIR /usr/src/mallet

RUN apk add --no-cache make git

COPY . .

RUN make

# Final stage
FROM alpine:3

RUN apk add --no-cache zstd-libs

WORKDIR /src

COPY --from=builder /usr/src/mallet/mallet /usr/local/bin/mallet

ENTRYPOINT ["mallet"]
