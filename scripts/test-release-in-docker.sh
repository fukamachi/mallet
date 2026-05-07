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
  -w /work \
  "$IMAGE" \
  bash -euo pipefail -c '
    # --- yum deps ---
    yum install -y -q epel-release >/dev/null
    yum install -y -q git make gcc curl libcurl-devel zlib-devel \
                      libzstd-devel sbcl openssl-devel >/dev/null

    # CentOS 7 ships openssl 1.0.2 as libcrypto.so.10. cl+ssl probes for
    # libcrypto.so.{3,1.1,1.0.0,unversioned} and exits non-zero on cleanup
    # when forced onto a name it does not recognise. 1.0.2 is ABI-compatible
    # with 1.0.0, so expose it under the SO names cl+ssl tries first.
    ln -sf /usr/lib64/libcrypto.so.10 /usr/lib64/libcrypto.so.1.0.0
    ln -sf /usr/lib64/libssl.so.10    /usr/lib64/libssl.so.1.0.0

    # libzstd-devel on CentOS 7 ships only the .so; build the static archive.
    ZSTD_VERSION=1.5.6
    cd /tmp
    curl -fsSL -o zstd.tgz \
      "https://github.com/facebook/zstd/releases/download/v${ZSTD_VERSION}/zstd-${ZSTD_VERSION}.tar.gz"
    tar xzf zstd.tgz
    make -C "zstd-${ZSTD_VERSION}/lib" -j"$(nproc)" libzstd.a >/dev/null
    install -m 644 "zstd-${ZSTD_VERSION}/lib/libzstd.a" /usr/local/lib/libzstd.a

    # --- Copy source into a writable workspace ---
    cp -a /src/. /work/
    cd /work
    rm -rf .qlot .bundle-libs mallet dist

    # --- Build SBCL 2.6.0 from source with static libzstd ---
    # EPEL sbcl 1.4.0 is the bootstrap host. SBCL has historically tolerated
    # multi-year host gaps, so this should chain to 2.6.0 directly.
    SBCL_VERSION=2.6.0
    case "$(uname -m)" in
      x86_64)  config=Config.x86-64-linux ;;
      aarch64) config=Config.arm64-linux  ;;
      *) echo "ERROR: unsupported arch: $(uname -m)" >&2; exit 1 ;;
    esac

    cd /tmp
    curl -fsSL -o sbcl-source.tar.bz2 \
      "https://sourceforge.net/projects/sbcl/files/sbcl/${SBCL_VERSION}/sbcl-${SBCL_VERSION}-source.tar.bz2/download"
    tar xjf sbcl-source.tar.bz2
    cd "sbcl-${SBCL_VERSION}"
    sed -i.bak "s|-lzstd|/usr/local/lib/libzstd.a|" "src/runtime/${config}"
    sh make.sh --fancy
    INSTALL_ROOT=/usr/local sh install.sh

    # /usr/local/bin must win for sbcl invocations from here on.
    export PATH="/usr/local/bin:$PATH"
    hash -r

    # --- Verify the source-built SBCL has no dynamic libzstd dependency ---
    ldd "$(command -v sbcl)" || true
    if ldd "$(command -v sbcl)" | grep -q libzstd; then
      echo "ERROR: SBCL still has dynamic libzstd dependency" >&2
      exit 1
    fi
    sbcl --version

    # --- Install qlot from source (Roswell sbcl-bin requires glibc >= 2.31) ---
    cd /tmp
    git clone --depth 1 https://github.com/fukamachi/qlot.git
    cd qlot
    make install

    # SBCL on this manylinux2014 + openssl combination consistently exits 255
    # *after* qlot has finished its work successfully (we observed deflate/etc.
    # downloads completing before the 255). Wrap qlot so 255 is treated as
    # success; any other non-zero exit is still surfaced.
    mv /usr/local/bin/qlot /usr/local/bin/qlot-real
    cat > /usr/local/bin/qlot << "WRAP"
#!/bin/sh
qlot-real "$@"
rc=$?
[ "$rc" = "255" ] && rc=0
exit "$rc"
WRAP
    chmod +x /usr/local/bin/qlot
    qlot --version

    # --- Build mallet ---
    cd /work
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
