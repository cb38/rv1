# Dockerfile for building Linux kernel + BusyBox for RV1 (rv32imac, nommu)
# Usage:  docker build -t rv1-linux-builder .
#         docker run --rm -v $(pwd)/out:/out rv1-linux-builder
FROM ubuntu:22.04

ARG DEBIAN_FRONTEND=noninteractive

# Install build dependencies
RUN apt-get update && apt-get install -y \
    build-essential bc bison flex libssl-dev libelf-dev \
    git wget cpio python3 device-tree-compiler \
    gcc-riscv64-linux-gnu binutils-riscv64-linux-gnu \
    file rsync && \
    rm -rf /var/lib/apt/lists/*

# We use the riscv64 toolchain with -march=rv32imac -mabi=ilp32 flags
# since riscv32-linux-gnu is not in Ubuntu repos.
# The kernel builds fine without libc. For userspace init we use raw syscalls.

WORKDIR /build

# ── Clone Linux kernel (shallow, specific version known to support rv32 nommu) ──
ARG LINUX_VERSION=v6.6
RUN git clone --depth 1 --branch ${LINUX_VERSION} \
    https://git.kernel.org/pub/scm/linux/kernel/git/stable/linux.git /build/linux

# Copy build script
COPY docker-build.sh /build/docker-build.sh
RUN chmod +x /build/docker-build.sh

# Copy config fragments
COPY rv1_defconfig_fragment /build/rv1_defconfig_fragment
COPY rv1.dts /build/rv1.dts
COPY initramfs/ /build/initramfs/

CMD ["/build/docker-build.sh"]
