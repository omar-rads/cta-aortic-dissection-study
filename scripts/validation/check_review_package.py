#!/usr/bin/env python3
"""Check the recorded review-package files and simple local navigation links."""

import argparse
import csv
import hashlib
import re
import sys
from pathlib import Path
from urllib.parse import unquote, urlsplit


LINK = re.compile(r"(?<!!)\[[^\]]*\]\(([^\s()]+)\)")
DOCUMENTS = ("README.md", "R/README.md", "docs/TEAM_CODE_REVIEW.md")


def check(root):
    errors = []
    manifest = root / "docs/FILE_PROVENANCE.csv"
    try:
        with manifest.open(newline="", encoding="utf-8") as stream:
            reader = csv.DictReader(stream)
            if not {"file", "bytes", "sha256"}.issubset(reader.fieldnames or []):
                raise ValueError("missing required columns")
            for row in reader:
                path = root / row["file"]
                if not path.is_file():
                    errors.append(f"missing: {row['file']}")
                    continue
                digest = hashlib.sha256()
                with path.open("rb") as source:
                    for chunk in iter(lambda: source.read(1024 * 1024), b""):
                        digest.update(chunk)
                if path.stat().st_size != int(row["bytes"]) or digest.hexdigest() != row["sha256"].lower():
                    errors.append(f"changed: {row['file']}")
    except (OSError, ValueError, KeyError, csv.Error) as exc:
        errors.append(f"manifest: {exc}")

    for name in DOCUMENTS:
        document = root / name
        try:
            content = document.read_text(encoding="utf-8")
        except OSError as exc:
            errors.append(f"document: {name}: {exc.strerror}")
            continue
        for target in LINK.findall(content):
            parts = urlsplit(target)
            if parts.scheme or parts.netloc or not parts.path:
                continue
            if not (document.parent / unquote(parts.path)).exists():
                errors.append(f"broken link: {name}: {target}")
    return errors


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=Path(__file__).resolve().parents[2])
    args = parser.parse_args()
    errors = check(args.root)
    if errors:
        print("\n".join(errors), file=sys.stderr)
        return 1
    print("Review package provenance and local links passed.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
