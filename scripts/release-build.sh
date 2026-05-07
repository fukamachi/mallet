#!/usr/bin/env bash
# Build a release tarball for Mallet.
#
# Assumes the following are already on PATH:
#     sbcl (or `ros run --` shim), qlot, make, tar, sha256 tool, git
#
# Inputs (env vars):
#     MALLET_VERSION   Required. Release tag in N.N.N form (e.g. 0.9.1).
#     MALLET_OS        Optional. linux | darwin. Auto-detected from `uname -s`.
#     MALLET_ARCH      Optional. x86_64 | aarch64. Auto-detected from `uname -m`.
#     MALLET_DIST_DIR  Optional. Output directory for the tarball. Default: dist/.
#
# Output:
#     ${MALLET_DIST_DIR}/mallet-${VERSION}-${OS}-${ARCH}.tar.gz
#     ${MALLET_DIST_DIR}/mallet-${VERSION}-${OS}-${ARCH}.tar.gz.sha256

set -euo pipefail

VERSION="${MALLET_VERSION:?MALLET_VERSION is required (e.g. 0.9.1)}"

case "${MALLET_OS:-$(uname -s)}" in
  linux|Linux)   OS=linux ;;
  darwin|Darwin) OS=darwin ;;
  *) echo "ERROR: unsupported OS: ${MALLET_OS:-$(uname -s)}" >&2; exit 1 ;;
esac

case "${MALLET_ARCH:-$(uname -m)}" in
  x86_64|amd64)  ARCH=x86_64 ;;
  aarch64|arm64) ARCH=aarch64 ;;
  *) echo "ERROR: unsupported arch: ${MALLET_ARCH:-$(uname -m)}" >&2; exit 1 ;;
esac

DIST_DIR="${MALLET_DIST_DIR:-dist}"
STAGE="mallet-${VERSION}-${OS}-${ARCH}"
STAGE_DIR="${DIST_DIR}/${STAGE}"

log() { printf '==> %s\n' "$*"; }

log "version=${VERSION} os=${OS} arch=${ARCH}"

for cmd in sbcl qlot make tar; do
  command -v "$cmd" >/dev/null 2>&1 || { echo "ERROR: missing required command: $cmd" >&2; exit 1; }
done

if command -v sha256sum >/dev/null 2>&1; then
  sha256() { sha256sum "$1"; }
elif command -v shasum >/dev/null 2>&1; then
  sha256() { shasum -a 256 "$1"; }
else
  echo "ERROR: need sha256sum or shasum" >&2
  exit 1
fi

# Required files for the tarball; fail early if any are missing.
for f in LICENSE README.md THIRD-PARTY-LICENSES; do
  [ -f "$f" ] || { echo "ERROR: required file missing: $f" >&2; exit 1; }
done

log "qlot install"
qlot install

log "qlot bundle (excluding test system)"
make bundle

log "make build"
make build

log "smoke test"
./mallet --version

if [ "$OS" = "linux" ]; then
  log "ldd"
  ldd ./mallet || true
  log "verify libzstd is statically linked (no dynamic libzstd dependency)"
  if ldd ./mallet | grep -q libzstd; then
    echo "ERROR: binary still has dynamic libzstd dependency; libzstd should be statically linked into SBCL" >&2
    exit 1
  fi
  log "glibc symbol versions"
  if command -v objdump >/dev/null 2>&1; then
    objdump -T ./mallet | awk '/GLIBC_/ {print $5}' | sort -u
  fi
elif [ "$OS" = "darwin" ]; then
  log "otool -L"
  otool -L ./mallet || true
  log "verify no Homebrew dylib dependencies"
  if otool -L ./mallet | tail -n +2 | awk '{print $1}' | grep -E '^(/opt/homebrew|/usr/local/opt|/usr/local/Cellar)/' >&2; then
    echo "ERROR: binary references Homebrew paths; libzstd should be statically linked into SBCL" >&2
    exit 1
  fi
  log "LC_BUILD_VERSION / LC_VERSION_MIN_MACOSX"
  otool -l ./mallet | awk '/LC_BUILD_VERSION|LC_VERSION_MIN_MACOSX/{flag=1} flag{print; n++} n>=5{flag=0; n=0}'
  log "verify ad-hoc signature (SBCL linker-signs the runtime)"
  codesign --verify --verbose --no-strict ./mallet
fi

log "stage tarball at ${STAGE_DIR}"
rm -rf "${STAGE_DIR}"
mkdir -p "${STAGE_DIR}/bin"
cp ./mallet "${STAGE_DIR}/bin/mallet"
chmod +x "${STAGE_DIR}/bin/mallet"
cp LICENSE README.md THIRD-PARTY-LICENSES "${STAGE_DIR}/"

sbcl_version="$(sbcl --version 2>/dev/null || echo unknown)"
commit="$(git rev-parse --short HEAD 2>/dev/null || echo unknown)"
{
  echo "name: mallet"
  echo "version: ${VERSION}"
  echo "commit: ${commit}"
  echo "platform: ${OS}/${ARCH}"
  echo "sbcl: ${sbcl_version}"
  echo "built: $(date -u +%Y-%m-%dT%H:%M:%SZ)"
} > "${STAGE_DIR}/VERSION"

log "create tar.gz"
( cd "${DIST_DIR}" && tar -czf "${STAGE}.tar.gz" "${STAGE}" )

log "compute sha256"
( cd "${DIST_DIR}" && sha256 "${STAGE}.tar.gz" > "${STAGE}.tar.gz.sha256" )

log "done"
ls -l "${DIST_DIR}/${STAGE}.tar.gz" "${DIST_DIR}/${STAGE}.tar.gz.sha256"
