#!/usr/bin/env python3
from __future__ import annotations

import argparse
import sys
from pathlib import Path


def main() -> int:
    parser = argparse.ArgumentParser(description="Basic VCD presence check for mul_hw")
    parser.add_argument("vcd_path", nargs="?", default="tmp/mul_hw_run.vcd")
    args = parser.parse_args()

    vcd_path = Path(args.vcd_path)
    if not vcd_path.is_file() or vcd_path.stat().st_size == 0:
        print(f"[check_vcd] FAIL: fichier VCD invalide: {vcd_path}")
        return 1

    print(f"[check_vcd] PASS: VCD présent ({vcd_path}, {vcd_path.stat().st_size} bytes)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
