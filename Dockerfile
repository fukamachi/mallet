# Build stage
FROM fukamachi/sbcl:latest-alpine as builder

WORKDIR /usr/src/app

RUN apk add --no-cache make git

COPY . .

RUN make

# Final stage
FROM fukamachi/sbcl:latest-alpine

WORKDIR /src

COPY --from=builder /usr/src/app/mallet /bin/mallet

ENTRYPOINT ["/bin/mallet"]
