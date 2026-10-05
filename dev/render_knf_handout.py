"""Backward-compatible launcher for the Markdown-first KNF handout renderer.

Edit dev/knf_steering_group_handout.md, then run either:
    Rscript dev/render_knf_handout.R
or:
    python dev/render_knf_handout.py
"""

from __future__ import annotations

import subprocess
from pathlib import Path


if __name__ == "__main__":
    script = Path(__file__).with_suffix(".R")
    subprocess.run(["Rscript", str(script)], check=True)
