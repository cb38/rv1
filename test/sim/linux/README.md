# Linux nommu on RV1 — simulation boot procedure

This directory contains everything needed to boot Linux on the RV1 core (RV32IMAC, M+U mode, no MMU) using the cxxrtl testbench.

---

## Prerequisites

| Tool | Purpose |
|------|---------|
| Docker (Desktop or Engine) | Kernel build, RISC-V toolchain |
| `dtc` | Device tree compiler |
| `tb_rv` testbench | cxxrtl CPU simulation |

Install `dtc` on macOS:
```bash
brew install dtc
```

Build the testbench once from `test/sim/`:
```bash
cd test/sim && make tb_build
```

---

## Architecture

```
┌────────────────────────────────────────────────────────┐
│  Docker image rv1-linux-builder (Ubuntu 22.04)         │
│  ├─ riscv64-linux-gnu- toolchain (rv32)                │
│  └─ linux v6.6 (pre-cloned in the image)               │
└────────────────────┬───────────────────────────────────┘
                     │ docker run → docker-build.sh
                     ▼
              out/linux.bin
              (kernel + initramfs + DTB merged)
                     │
                     │ tb_rv --bin linux.bin …
                     ▼
              out/boot.log
```

### SoC memory map

| Address | Peripheral |
|---------|----------|
| `0x80000000` | RAM 16 MiB (kernel entry point) |
| `0x10000000` | UART_VIRT (earlycon, byte-spaced registers) |
| `0xC0000200` | UART 8250 (console, reg-shift=2, word-spaced) |
| `0x02000000` | CLINT (M-mode timer) |

---

## Workflow

### 1. Build the Docker image (once, ~5 min)

```bash
make docker
```

Clones Linux v6.6 and installs the RISC-V toolchain inside the image.

### 2. Build the kernel + initramfs

```bash
make linux
```

Runs `docker-build.sh` in 5 steps inside the container:

| Step | Action |
|------|--------|
| 1 | Compile minimal `/init` in RV32 assembly, no libc, linked as PIE |
| 2 | Create initramfs cpio from a file list (`gen_init_cpio`) including `/dev/console` |
| 3 | Install DTS + configure kernel (`rv32_nommu_virt_defconfig` + `rv1_defconfig_fragment`) |
| 4 | Build the kernel (`make ARCH=riscv ...`) |
| 5 | Merge Image + initramfs + DTB → `out/linux.bin`, compute `--dtb-addr` |

Files produced in `out/`:

```
out/linux.bin       — flat binary ready for --bin
out/Image           — kernel only
out/rv1.dtb         — compiled device tree
out/build_info.txt  — DTB_ADDR computed automatically
```

### 3. Run the simulation

```bash
make run
```

Invokes the testbench with the correct flags:

```
tb_rv --bin out/linux.bin --noshift --bootpc 0x80000000 \
      --dtb-addr <DTB_ADDR> --cycles 500000000 --logfile out/boot.log
```

- `--noshift` : load the binary flat, no address shifting
- `--bootpc 0x80000000` : initial PC = start of RAM
- `--dtb-addr` : read automatically from `out/build_info.txt`

Expected output at the end of `out/boot.log`:

```
=====================================
 RV1 nommu Linux — boot successful!
=====================================

[init] Hello from userspace on RV1!
[init] System halting.
[    x.xxxxxx] Kernel panic - not syncing: Attempted to kill init! ...
```

> **Note:** The final kernel panic is expected — `/init` calls `exit(0)`, and the kernel does not allow PID 1 to exit.

Increase the cycle count if needed:

```bash
make run MAX_CYCLES=2000000000
```

### 4. Clean

```bash
make clean
```

---

## Kernel configuration

The kernel is configured from `rv32_nommu_virt_defconfig` plus `rv1_defconfig_fragment`:

- **CONFIG_RISCV_M_MODE=y** — pure M-mode (no S-mode, no SBI)
- **CONFIG_MMU=n** — nommu, no paging
- **CONFIG_ARCH_RV32I=y** — RV32
- **CONFIG_BUILTIN_DTB=y** — DTB embedded in the kernel (no bootloader needed)
- **CONFIG_CMDLINE_FORCE=y** — forced kernel command line:
  ```
  earlycon=uart8250,mmio,0x10000000,115200n8 console=ttyS0 keep_bootcon loglevel=7 clocksource=jiffies
  ```
- **CONFIG_SMP=n**, **CONFIG_PREEMPT_NONE=y**

---

## Minimal init

`/init` is a RV32 assembly binary (~500 bytes) linked as PIE (required for nommu).  
It does two things:

1. **Write directly to the UART** (THR `0xC0000200`, LSR `0xC0000214`) with THRE polling.  
   Reason: the 8250 driver buffers output without a TX interrupt in simulation — messages would be lost if using `write(1, ...)` alone.
2. **`exit(0)` syscall** (syscall 93) to terminate cleanly.

---

## U-mode support in the CPU

Running user processes requires the RV1 CPU to implement:

- `priv` register (2 bits, init=3=M-mode)
- `ecall`: cause = 8 if `priv=U`, cause = 11 if `priv=M`
- All traps save `priv` into `mstatus.MPP` and switch to M-mode
- `mret` restores `priv` from `mstatus.MPP`
- `misa` bit 20 (U extension) set
- Privilege check on M-mode CSR access from U-mode

These changes are in `hw/spinal/rv/rv.scala`.

---

## Files in this directory

| File | Purpose |
|------|---------|
| `Dockerfile` | Docker image with RISC-V toolchain and Linux v6.6 sources |
| `docker-build.sh` | Build script (runs inside the container) |
| `rv1.dts` | SoC device tree source |
| `rv1_defconfig_fragment` | Kernel configuration fragment |
| `Makefile` | Orchestration (docker / linux / run / clean) |
| `out/` | Build artifacts (gitignored) |
