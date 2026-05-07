#!/usr/bin/env bash
# Run inside a manylinux2014 container (CentOS 7 base, glibc 2.17). Builds
# SBCL 2.6.0 from source with a statically linked libzstd, installs qlot,
# then defers to scripts/release-build.sh to produce the mallet tarball.
#
# Mount the project at /work and invoke this script with bash. The caller
# (release.yml or scripts/test-release-in-docker.sh) supplies these env vars:
#     MALLET_VERSION, MALLET_OS, MALLET_ARCH
#
# Output: /work/dist/mallet-${MALLET_VERSION}-${MALLET_OS}-${MALLET_ARCH}.tar.gz

set -euo pipefail

# --- yum deps -------------------------------------------------------------
yum install -y -q epel-release
yum install -y -q git make gcc curl libcurl-devel zlib-devel \
                  libzstd-devel sbcl openssl-devel

# CentOS 7 ships openssl 1.0.2 as libcrypto.so.10. cl+ssl probes for
# libcrypto.so.{3,1.1,1.0.0,unversioned}; expose 1.0.2 under the 1.0.0
# SO name (ABI-compatible) so dexador can load.
ln -sf /usr/lib64/libcrypto.so.10 /usr/lib64/libcrypto.so.1.0.0
ln -sf /usr/lib64/libssl.so.10    /usr/lib64/libssl.so.1.0.0

# --- libzstd 1.5.6 static archive ----------------------------------------
ZSTD_VERSION=1.5.6
cd /tmp
curl -fsSL -o zstd.tgz \
  "https://github.com/facebook/zstd/releases/download/v${ZSTD_VERSION}/zstd-${ZSTD_VERSION}.tar.gz"
tar xzf zstd.tgz
make -C "zstd-${ZSTD_VERSION}/lib" -j"$(nproc)" libzstd.a >/dev/null
install -m 644 "zstd-${ZSTD_VERSION}/lib/libzstd.a" /usr/local/lib/libzstd.a

# --- SBCL 2.6.0 from source ----------------------------------------------
# EPEL sbcl 1.4.0 is the bootstrap host. SBCL has historically tolerated
# multi-year host gaps, so 1.4.0 -> 2.6.0 chains directly.
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

export PATH="/usr/local/bin:$PATH"
hash -r

# --- Verify the source-built SBCL has no dynamic libzstd dependency ------
ldd "$(command -v sbcl)" || true
if ldd "$(command -v sbcl)" | grep -q libzstd; then
  echo "ERROR: SBCL still has dynamic libzstd dependency" >&2
  exit 1
fi
sbcl --version

# --- qlot from source -----------------------------------------------------
# Roswell sbcl-bin tarballs require glibc >= 2.31, so install qlot directly.
cd /tmp
git clone --depth 1 https://github.com/fukamachi/qlot.git
cd qlot
make install

# SBCL on this manylinux2014 + openssl combo consistently exits 255 *after*
# qlot has finished its work successfully (cl+ssl OpenSSL cleanup race).
# Wrap qlot so 255 is treated as success; any other non-zero exit is still
# surfaced.
mv /usr/local/bin/qlot /usr/local/bin/qlot-real
printf '%s\n' \
  '#!/bin/sh' \
  'qlot-real "$@"' \
  'rc=$?' \
  '[ "$rc" = "255" ] && rc=0' \
  'exit "$rc"' \
  > /usr/local/bin/qlot
chmod +x /usr/local/bin/qlot

# --- Build mallet ---------------------------------------------------------
cd /work
bash scripts/release-build.sh
