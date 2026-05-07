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

# The container runs as root; install build deps, build SBCL from source with
# static libzstd (matching release.yml), then defer to scripts/release-build.sh.
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

    # --- Install Roswell first; its sbcl-bin is the bootstrap host for ---
    # --- make.sh (release.yml depends on this same ordering).           ---
    export LISP=sbcl-bin
    curl -fsSL https://raw.githubusercontent.com/roswell/roswell/master/scripts/install-for-ci.sh | sh
    HOST_SBCL=$(find "$HOME/.roswell/impls" -name sbcl -type f -perm -u+x | head -1)
    [ -n "$HOST_SBCL" ] || { echo "ERROR: Roswell sbcl-bin not found" >&2; exit 1; }
    HOST_SBCL_HOME=$(dirname "$(dirname "$HOST_SBCL")")/lib/sbcl

    # --- Build SBCL from source with static libzstd (mirror release.yml) ---
    SBCL_VERSION=2.6.0
    case "$(uname -m)" in
      x86_64)  zstd_a=/usr/lib/x86_64-linux-gnu/libzstd.a;  config=Config.x86-64-linux ;;
      aarch64) zstd_a=/usr/lib/aarch64-linux-gnu/libzstd.a; config=Config.arm64-linux  ;;
      *) echo "ERROR: unsupported arch in container: $(uname -m)" >&2; exit 1 ;;
    esac
    [ -f "$zstd_a" ] || { echo "ERROR: missing static archive: $zstd_a" >&2; exit 1; }

    (
      export PATH="$(dirname "$HOST_SBCL"):$PATH"
      export SBCL_HOME="$HOST_SBCL_HOME"
      sbcl --version

      mkdir -p /tmp/sbcl-build
      cd /tmp/sbcl-build
      curl -fsSL -o sbcl-source.tar.bz2 \
        "https://sourceforge.net/projects/sbcl/files/sbcl/${SBCL_VERSION}/sbcl-${SBCL_VERSION}-source.tar.bz2/download"
      tar xjf sbcl-source.tar.bz2
      cd "sbcl-${SBCL_VERSION}"
      sed -i.bak "s|-lzstd|${zstd_a}|" "src/runtime/${config}"
      sh make.sh --fancy
      # install.sh refuses to run while SBCL_HOME is set (it would point at
      # the host SBCL, not the freshly built one). Drop it before installing.
      unset SBCL_HOME
      INSTALL_ROOT=/usr/local sh install.sh
    )

    # /usr/local/bin first so the source-built sbcl wins. SBCL_HOME from the
    # host is no longer relevant; unset it so the new sbcl finds its own core.
    export PATH="/usr/local/bin:$HOME/.roswell/bin:$PATH"
    unset SBCL_HOME

    # --- Verify the source-built SBCL has no dynamic libzstd dependency ---
    ldd "$(command -v sbcl)" || true
    if ldd "$(command -v sbcl)" | grep -q libzstd; then
      echo "ERROR: SBCL still has dynamic libzstd dependency" >&2
      exit 1
    fi

    # --- Install qlot via Roswell ---
    ros install fukamachi/qlot
    ros install fukamachi/archive/fix/tar-symlink-entry

    # --- Verify sbcl on PATH is the source-built one ---
    if [ "$(command -v sbcl)" != "/usr/local/bin/sbcl" ]; then
      echo "ERROR: expected sbcl at /usr/local/bin/sbcl, got $(command -v sbcl)" >&2
      exit 1
    fi
    sbcl --version

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
