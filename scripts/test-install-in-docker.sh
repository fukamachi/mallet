#!/usr/bin/env bash
# Smoke-test scripts/install.sh against a local HTTP server that mimics
# GitHub Releases. Verifies the full path: download -> SHA256 verification ->
# extract -> binary works.
#
# Requires that scripts/test-release-in-docker.sh has already produced a
# tarball under dist/.
#
# Usage:
#     scripts/test-install-in-docker.sh [VERSION] [ARCH]
#
# Default: VERSION=0.9.1-test, ARCH=x86_64.

set -euo pipefail

VERSION="${1:-0.9.1-test}"
ARCH="${2:-x86_64}"
PROJECT_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
ASSET="mallet-${VERSION}-linux-${ARCH}.tar.gz"
TARBALL="${PROJECT_ROOT}/dist/${ASSET}"

[ -f "$TARBALL" ] || {
  echo "ERROR: tarball not found: $TARBALL" >&2
  echo "       Run scripts/test-release-in-docker.sh first." >&2
  exit 1
}

case "$ARCH" in
  x86_64)  PLATFORM=linux/amd64 ;;
  aarch64) PLATFORM=linux/arm64 ;;
  *) echo "ERROR: unsupported ARCH: $ARCH" >&2; exit 1 ;;
esac

# Lay out a directory that mirrors GitHub's URL structure:
#     /<repo>/releases/download/<tag>/<asset>
#     /<repo>/releases/download/<tag>/SHA256SUMS
MOCK_ROOT="${PROJECT_ROOT}/dist/mock-server"
RELEASE_DIR="${MOCK_ROOT}/fukamachi/mallet/releases/download/${VERSION}"
rm -rf "$MOCK_ROOT"
mkdir -p "$RELEASE_DIR"
cp "$TARBALL" "$RELEASE_DIR/"
( cd "$RELEASE_DIR" && sha256sum "$ASSET" > SHA256SUMS )

NETWORK="mallet-install-test-net"
SERVER="mallet-install-mock-server"

cleanup() {
  docker rm -f "$SERVER" >/dev/null 2>&1 || true
  docker network rm "$NETWORK" >/dev/null 2>&1 || true
}
trap cleanup EXIT INT TERM

docker network create "$NETWORK" >/dev/null

echo "==> Starting mock release server (python3 http.server)"
docker run -d --rm \
  --name "$SERVER" \
  --network "$NETWORK" \
  --platform "$PLATFORM" \
  -v "${MOCK_ROOT}:/srv:ro" \
  -w /srv \
  python:3.12-slim \
  python3 -m http.server 8080 >/dev/null

# Wait for server to start (poll until reachable from inside the network).
for _ in $(seq 1 20); do
  if docker run --rm --network "$NETWORK" --platform "$PLATFORM" \
      curlimages/curl:latest -fsS -o /dev/null \
      "http://${SERVER}:8080/fukamachi/mallet/releases/download/${VERSION}/SHA256SUMS"; then
    break
  fi
  sleep 0.5
done

echo "==> Running install.sh in a clean ubuntu:22.04 container"
docker run --rm \
  --network "$NETWORK" \
  --platform "$PLATFORM" \
  --dns 8.8.8.8 --dns 1.1.1.1 \
  -v "${PROJECT_ROOT}/scripts:/scripts:ro" \
  -e MALLET_VERSION="$VERSION" \
  -e MALLET_BASE_URL="http://${SERVER}:8080" \
  -e MALLET_API_BASE_URL="http://${SERVER}:8080/api-not-used" \
  -e MALLET_INSTALL_DIR=/usr/local/bin \
  -e DEBIAN_FRONTEND=noninteractive \
  ubuntu:22.04 \
  bash -eu -c '
    apt-get update -qq
    apt-get install -y --no-install-recommends ca-certificates curl libzstd1 >/dev/null
    sh /scripts/install.sh
    echo
    echo "==> mallet --help"
    mallet --help | head -30
    echo
    echo "==> mallet --version"
    mallet --version
  '
