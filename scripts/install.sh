#!/bin/sh
# Install Mallet from a GitHub release tarball.
#
# Usage:
#     curl -fsSL https://raw.githubusercontent.com/fukamachi/mallet/main/scripts/install.sh | sh
#     curl -fsSL https://raw.githubusercontent.com/fukamachi/mallet/main/scripts/install.sh | MALLET_VERSION=v0.9.1 sh
#     curl -fsSL https://raw.githubusercontent.com/fukamachi/mallet/main/scripts/install.sh | MALLET_INSTALL_DIR=/usr/local/bin sh
#
# Environment variables:
#     MALLET_VERSION       Tag to install (e.g. v0.9.1). Defaults to the latest release.
#     MALLET_INSTALL_DIR   Where to drop the binary. Defaults to ~/.local/bin.
#     MALLET_REPO          GitHub repo slug. Defaults to fukamachi/mallet.

set -eu

REPO="${MALLET_REPO:-fukamachi/mallet}"
INSTALL_DIR="${MALLET_INSTALL_DIR:-${HOME}/.local/bin}"

log()  { printf '==> %s\n' "$*"; }
warn() { printf 'WARNING: %s\n' "$*" >&2; }
fail() { printf 'ERROR: %s\n' "$*" >&2; exit 1; }

need() {
  command -v "$1" >/dev/null 2>&1 || fail "missing required command: $1"
}

need uname
need tar
need mkdir
if command -v curl >/dev/null 2>&1; then
  fetch() { curl -fsSL "$1" -o "$2"; }
  fetch_stdout() { curl -fsSL "$1"; }
elif command -v wget >/dev/null 2>&1; then
  fetch() { wget -q -O "$2" "$1"; }
  fetch_stdout() { wget -q -O - "$1"; }
else
  fail "need either curl or wget"
fi

if command -v sha256sum >/dev/null 2>&1; then
  sha256() { sha256sum "$1" | awk '{print $1}'; }
elif command -v shasum >/dev/null 2>&1; then
  sha256() { shasum -a 256 "$1" | awk '{print $1}'; }
else
  warn "no sha256sum/shasum found; skipping checksum verification"
  sha256() { echo "skip"; }
fi

uname_s="$(uname -s)"
case "$uname_s" in
  Linux)  os=linux ;;
  Darwin) os=darwin ;;
  *)      fail "unsupported OS: $uname_s" ;;
esac

uname_m="$(uname -m)"
case "$uname_m" in
  x86_64|amd64)        arch=x86_64 ;;
  aarch64|arm64)       arch=aarch64 ;;
  *)                   fail "unsupported arch: $uname_m" ;;
esac

# Resolve target version. Tags are N.N.N (no leading "v") — strip it if provided.
if [ -n "${MALLET_VERSION:-}" ]; then
  tag="${MALLET_VERSION#v}"
else
  log "resolving latest release for ${REPO}"
  api_url="https://api.github.com/repos/${REPO}/releases/latest"
  tag="$(fetch_stdout "$api_url" | awk -F'"' '/"tag_name":/{print $4; exit}')"
  [ -n "$tag" ] || fail "could not resolve latest release tag from $api_url"
fi
log "installing mallet ${tag} for ${os}/${arch}"

asset="mallet-${tag}-${os}-${arch}.tar.gz"
base_url="https://github.com/${REPO}/releases/download/${tag}"
tarball_url="${base_url}/${asset}"
sums_url="${base_url}/SHA256SUMS"

tmp="$(mktemp -d 2>/dev/null || mktemp -d -t mallet-install)"
trap 'rm -rf "$tmp"' EXIT INT TERM

log "downloading ${asset}"
fetch "$tarball_url" "$tmp/$asset"

log "verifying checksum"
if fetch "$sums_url" "$tmp/SHA256SUMS" 2>/dev/null; then
  expected="$(awk -v name="$asset" '$2 == name || $2 == "*" name {print $1; exit}' "$tmp/SHA256SUMS")"
  if [ -z "$expected" ]; then
    warn "no checksum entry for ${asset} in SHA256SUMS"
  else
    actual="$(sha256 "$tmp/$asset")"
    if [ "$actual" = "skip" ]; then
      :
    elif [ "$actual" != "$expected" ]; then
      fail "checksum mismatch: expected $expected, got $actual"
    fi
  fi
else
  warn "SHA256SUMS not available, skipping verification"
fi

log "extracting"
tar -xzf "$tmp/$asset" -C "$tmp"

stage="${asset%.tar.gz}"
src_bin="${tmp}/${stage}/bin/mallet"
[ -x "$src_bin" ] || fail "binary missing in tarball: $src_bin"

mkdir -p "$INSTALL_DIR"
dest="${INSTALL_DIR}/mallet"
cp "$src_bin" "$dest"
chmod +x "$dest"

# Strip macOS quarantine attribute if present (best-effort).
if [ "$os" = "darwin" ] && command -v xattr >/dev/null 2>&1; then
  xattr -d com.apple.quarantine "$dest" 2>/dev/null || true
fi

log "installed: $dest"
case ":$PATH:" in
  *":${INSTALL_DIR}:"*) ;;
  *) warn "${INSTALL_DIR} is not in your PATH; add it to use 'mallet' directly" ;;
esac

"$dest" --version || true
