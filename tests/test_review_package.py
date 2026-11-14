import csv
import hashlib
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path


CHECKER = Path(__file__).resolve().parents[1] / "scripts/validation/check_review_package.py"


class ReviewPackageTest(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        (self.root / "docs").mkdir()
        (self.root / "R").mkdir()
        (self.root / "R/README.md").write_text("[item](../item.txt)", encoding="utf-8")
        (self.root / "item.txt").write_text("original", encoding="utf-8")
        (self.root / "README.md").write_text("[item](item.txt) [web](https://example.org) [section](#x)", encoding="utf-8")
        (self.root / "docs/TEAM_CODE_REVIEW.md").write_text("[item](../item.txt#section)", encoding="utf-8")
        with (self.root / "docs/FILE_PROVENANCE.csv").open("w", newline="", encoding="utf-8") as stream:
            writer = csv.writer(stream)
            writer.writerow(["file", "bytes", "sha256"])
            writer.writerow(["item.txt", 8, hashlib.sha256(b"original").hexdigest()])

    def run_check(self):
        return subprocess.run([sys.executable, "-O", str(CHECKER), "--root", str(self.root)],
                              capture_output=True, text=True, check=False)

    def test_matching_package(self):
        self.assertEqual(self.run_check().returncode, 0)

    def test_missing_file(self):
        (self.root / "item.txt").unlink()
        result = self.run_check()
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("missing: item.txt", result.stderr)

    def test_modified_file_same_size(self):
        (self.root / "item.txt").write_text("modified", encoding="utf-8")
        result = self.run_check()
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("changed: item.txt", result.stderr)

    def test_broken_local_link(self):
        (self.root / "README.md").write_text("[missing](absent.txt)", encoding="utf-8")
        result = self.run_check()
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("broken link: README.md: absent.txt", result.stderr)


if __name__ == "__main__":
    unittest.main()
