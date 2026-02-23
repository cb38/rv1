#!/usr/bin/env python3
from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path

EXPECTED_MCAUSE_WRITE_0123 = "b00000000000000000000000100100011"
EXPECTED_MCAUSE_IRQ = "b10000000000000000000000000001011"


def fail(message: str) -> None:
    print(f"[check_vcd] FAIL: {message}")
    raise SystemExit(1)


def ok(message: str) -> None:
    print(f"[check_vcd] PASS: {message}")


def extract_mcause_id(lines: list[str]) -> str | None:
    pattern = re.compile(
        r"^\$var\s+reg\s+32\s+(\S+)\s+CsrRegs_mcause\s+\$end$"
    )
    for line in lines:
        match = pattern.match(line)
        if match:
            return match.group(1)
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

    mcause_id = extract_mcause_id(lines)
    if not mcause_id:
        fail("signal CsrRegs_mcause introuvable")

    mcause_pattern = re.compile(rf"^(b[01xz]+)\s+{re.escape(mcause_id)}$")
    mcause_values = []
    for line in lines:
        match = mcause_pattern.match(line)
        if match:
            mcause_values.append(match.group(1))

    if not mcause_values:
        fail("aucune valeur trouvée pour CsrRegs_mcause")

    if EXPECTED_MCAUSE_WRITE_0123 not in mcause_values:
        fail("valeur mcause 0x123 non observée")

    if EXPECTED_MCAUSE_IRQ not in mcause_values:
        fail("valeur mcause IRQ 0x8000000B non observée")

    final_mcause = mcause_values[-1]
    if final_mcause != EXPECTED_MCAUSE_IRQ:
        fail(f"valeur finale mcause inattendue: {final_mcause}")

    ok(
        f"VCD valide ({vcd_path}, {line_count} lignes, dernier timestamp {last_timestamp}, "
        f"mcause final=0x8000000B)"
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())
