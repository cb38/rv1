#!/usr/bin/env python3
from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path

EXPECTED_MCAUSE_WRITE_0123 = "b00000000000000000000000100100011"
EXPECTED_MCAUSE_IRQ_EXTERNAL = "b10000000000000000000000000001011"
EXPECTED_MCAUSE_IRQ_TIMER = "b10000000000000000000000000000111"


def fail(message: str) -> None:
    print(f"[check_vcd] FAIL: {message}")
    raise SystemExit(1)


def ok(message: str) -> None:
    print(f"[check_vcd] PASS: {message}")


def extract_signal_id(lines: list[str], signal_name: str) -> str | None:
    pattern = re.compile(
        rf"^\$var\s+reg\s+32\s+(\S+)\s+{re.escape(signal_name)}\s+\$end$"
    )
    for line in lines:
        match = pattern.match(line)
        if match:
            return match.group(1)
    return None


def collect_signal_values(lines: list[str], signal_id: str) -> list[str]:
    signal_pattern = re.compile(rf"^(b[01xz]+)\s+{re.escape(signal_id)}$")
    values = []
    for line in lines:
        match = signal_pattern.match(line)
        if match:
            values.append(match.group(1))
    return values


def collect_signal_events(lines: list[str], signal_id: str) -> list[tuple[int, str]]:
    signal_pattern = re.compile(rf"^(b[01xz]+)\s+{re.escape(signal_id)}$")
    events: list[tuple[int, str]] = []
    current_ts = 0
    for line in lines:
        if line.startswith("#"):
            try:
                current_ts = int(line[1:])
            except ValueError:
                continue
            continue
        match = signal_pattern.match(line)
        if match:
            events.append((current_ts, match.group(1)))
    return events


def bit_value(vcd_binary: str, bit_idx: int) -> str | None:
    bits = vcd_binary[1:]
    if len(bits) < 32:
        bits = bits.rjust(32, "0")
    bit = bits[31 - bit_idx]
    if bit in ("0", "1"):
        return bit
    return None


def main() -> int:
    parser = argparse.ArgumentParser(description="Check irq_csr VCD content")
    parser.add_argument(
        "vcd_path",
        nargs="?",
        default="tmp/irq_csr_run.vcd",
        help="Path to VCD file (default: tmp/irq_csr_run.vcd)",
    )
    args = parser.parse_args()

    vcd_path = Path(args.vcd_path)
    if not vcd_path.is_file():
        fail(f"fichier introuvable: {vcd_path}")
    if vcd_path.stat().st_size == 0:
        fail(f"fichier vide: {vcd_path}")

    lines = vcd_path.read_text(encoding="utf-8", errors="replace").splitlines()
    line_count = len(lines)

    timestamps = [line for line in lines if line.startswith("#")]
    if not timestamps:
        fail("aucun timestamp (#...) trouvé")
    last_timestamp = timestamps[-1]

    mcause_id = extract_signal_id(lines, "CsrRegs_mcause")
    if not mcause_id:
        fail("signal CsrRegs_mcause introuvable")

    mcause_values = collect_signal_values(lines, mcause_id)

    if not mcause_values:
        fail("aucune valeur trouvée pour CsrRegs_mcause")

    if EXPECTED_MCAUSE_WRITE_0123 not in mcause_values:
        fail("valeur mcause 0x123 non observée")

    if EXPECTED_MCAUSE_IRQ_EXTERNAL not in mcause_values:
        fail("valeur mcause IRQ 0x8000000B non observée")

    if EXPECTED_MCAUSE_IRQ_TIMER not in mcause_values:
        fail("valeur mcause IRQ timer 0x80000007 non observée")

    final_mcause = mcause_values[-1]
    if final_mcause != EXPECTED_MCAUSE_IRQ_TIMER:
        fail(f"valeur finale mcause inattendue: {final_mcause}")

    mip_id = extract_signal_id(lines, "CsrRegs_mip")
    if not mip_id:
        fail("signal CsrRegs_mip introuvable")

    mip_events = collect_signal_events(lines, mip_id)
    mip_values = [value for _, value in mip_events]
    if not mip_values:
        fail("aucune valeur trouvée pour CsrRegs_mip")

    seen_mtip_set = False
    seen_mtip_clear_after_set = False
    mtip_set_ts = -1
    mtip_clear_ts = -1
    for ts, value in mip_events:
        mtip = bit_value(value, 7)
        if mtip == "1":
            if not seen_mtip_set:
                mtip_set_ts = ts
            seen_mtip_set = True
        elif seen_mtip_set and mtip == "0":
            if not seen_mtip_clear_after_set:
                mtip_clear_ts = ts
            seen_mtip_clear_after_set = True

    if not seen_mtip_set:
        fail("bit mip.MTIP (bit 7) jamais vu à 1")
    if not seen_mtip_clear_after_set:
        fail("bit mip.MTIP (bit 7) n'est jamais redescendu à 0 après être monté")

    ok(
        f"VCD valide ({vcd_path}, {line_count} lignes, dernier timestamp {last_timestamp}, "
        f"mcause final=0x80000007, mip.MTIP 1@#{mtip_set_ts} -> 0@#{mtip_clear_ts})"
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())
