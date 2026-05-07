#!/usr/bin/env bash
# Run the release build locally inside an ubuntu:22.04 Docker container,
# mirroring the GitHub Actions release.yml pipeline. Useful for catching
# breakages without burning CI minutes.
#
# Usage:
#     scripts/test-release-in-docker.sh [VERSION] [ARCH]
#
# Examples:
#     scripts/test-release-in-docker.sh 0.9.1
#     scripts/test-release-in-docker.sh 0.9.1 aarch64    # needs qemu-user-static
#
# Output:
#     dist/mallet-${VERSION}-linux-${ARCH}.tar.gz
#     dist/mallet-${VERSION}-linux-${ARCH}.tar.gz.sha256

set -euo pipefail

VERSION="${1:-${MALLET_VERSION:-0.0.0-local}}"
ARCH="${2:-${MALLET_ARCH:-x86_64}}"
PROJECT_ROOT="$(cd "$(dirname "$0")/.." && pwd)"

case "$ARCH" in
  x86_64)  PLATFORM=linux/amd64 ;;
  aarch64) PLATFORM=linux/arm64 ;;
  *) echo "ERROR: unsupported ARCH: $ARCH (use x86_64 or aarch64)" >&2; exit 1 ;;
esac

command -v docker >/dev/null 2>&1 || { echo "ERROR: docker not found" >&2; exit 1; }

mkdir -p "${PROJECT_ROOT}/dist"

echo "==> Running release build inside ubuntu:22.04 (${PLATFORM}) for version=${VERSION}"

# The container script runs as root by default; install deps, set up Roswell
# (matching the workflow), then defer to scripts/release-build.sh.
docker run --rm \
  --platform "$PLATFORM" \
  --dns 8.8.8.8 --dns 1.1.1.1 \
  -v "${PROJECT_ROOT}:/src:ro" \
  -v "${PROJECT_ROOT}/dist:/out" \
  -e MALLET_VERSION="$VERSION" \
  -e MALLET_OS=linux \
  -e MALLET_ARCH="$ARCH" \
  -e DEBIAN_FRONTEND=noninteractive \
  -e CI=true \
  -w /work \
  ubuntu:22.04 \
  bash -eu -c '
    apt-get update -qq
    apt-get install -y --no-install-recommends \
      ca-certificates curl wget git make build-essential automake \
      libcurl4-openssl-dev zlib1g-dev libzstd-dev libzstd1

    # Copy source into a writable workspace; clear any host-side build outputs.
    cp -a /src/. /work/
    rm -rf .qlot .bundle-libs mallet dist

    # Install Roswell + sbcl-bin (same as release.yml).
    export LISP=sbcl-bin
    curl -fsSL https://raw.githubusercontent.com/roswell/roswell/master/scripts/install-for-ci.sh | sh

    export PATH="$HOME/.roswell/bin:$HOME/.local/bin:$PATH"
    mkdir -p "$HOME/.local/bin"
    printf "#!/bin/sh\nexec ros run -- \"\$@\"\n" > "$HOME/.local/bin/sbcl"
    chmod +x "$HOME/.local/bin/sbcl"

    ros install fukamachi/qlot
    ros install fukamachi/archive/fix/tar-symlink-entry

    bash scripts/release-build.sh

    # Hand artifacts back to the host.
    cp -a dist/. /out/
  '

echo
echo "==> Tarball(s) in ${PROJECT_ROOT}/dist/"
ls -l "${PROJECT_ROOT}/dist/" | grep "${VERSION}" || true

echo
echo "==> Inspecting tarball contents"
tar -tzf "${PROJECT_ROOT}/dist/mallet-${VERSION}-linux-${ARCH}.tar.gz" | head -20

echo
echo "==> Smoke-testing the binary inside ubuntu:22.04"
docker run --rm \
  --platform "$PLATFORM" \
  --dns 8.8.8.8 --dns 1.1.1.1 \
  -v "${PROJECT_ROOT}/dist:/dist:ro" \
  ubuntu:22.04 \
  bash -eu -c "
    apt-get update -qq
    apt-get install -y --no-install-recommends ca-certificates libzstd1 >/dev/null
    tar -xzf /dist/mallet-${VERSION}-linux-${ARCH}.tar.gz -C /tmp
    /tmp/mallet-${VERSION}-linux-${ARCH}/bin/mallet --version
  "
