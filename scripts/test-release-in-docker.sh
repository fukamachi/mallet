#!/usr/bin/env bash
# Run the release build locally inside a manylinux2014 (CentOS 7 / glibc 2.17)
# Docker container, mirroring the GitHub Actions release.yml pipeline. Building
# on glibc 2.17 means the resulting binary runs on every supported Linux LTS
# (Ubuntu 18.04+, Debian 10+, RHEL/Rocky/Alma 7+).
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
  x86_64)  PLATFORM=linux/amd64; IMAGE=quay.io/pypa/manylinux2014_x86_64  ;;
  aarch64) PLATFORM=linux/arm64; IMAGE=quay.io/pypa/manylinux2014_aarch64 ;;
  *) echo "ERROR: unsupported ARCH: $ARCH (use x86_64 or aarch64)" >&2; exit 1 ;;
esac

command -v docker >/dev/null 2>&1 || { echo "ERROR: docker not found" >&2; exit 1; }

mkdir -p "${PROJECT_ROOT}/dist"

echo "==> Running release build inside ${IMAGE} (${PLATFORM}) for version=${VERSION}"

# The container runs as root and shares the project tree as /work. The
# inner build steps live in scripts/release-linux-in-container.sh — the
# same script the GitHub Actions workflow invokes — so local repros stay
# in lockstep with CI.
docker run --rm \
  --platform "$PLATFORM" \
  --dns 8.8.8.8 --dns 1.1.1.1 \
  -v "${PROJECT_ROOT}:/work" \
  -e MALLET_VERSION="$VERSION" \
  -e MALLET_OS=linux \
  -e MALLET_ARCH="$ARCH" \
  -w /work \
  "$IMAGE" \
  bash /work/scripts/release-linux-in-container.sh

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
