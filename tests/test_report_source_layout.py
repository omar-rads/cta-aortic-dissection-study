"""Synthetic checks for source freeze and renamed package inventory."""
import ast
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest
from unittest.mock import patch

ROOT = Path(__file__).resolve().parents[1]
SCRIPTS = ROOT / 'scripts/report_alt_dx'
sys.path.insert(0, str(SCRIPTS))
spec = importlib.util.spec_from_file_location('source_layout_classifier_v2', SCRIPTS / 'run_classifier_v2.py')
classifier = importlib.util.module_from_spec(spec)
spec.loader.exec_module(classifier)


class FreezeTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        self.root = Path(self.tmp.name)
        self.run = self.root / 'run'
        self.run.mkdir()
        self.files = {
            'R/17_report_alt_dx_calibration_ingest.R': b'ingest',
            'R/18_report_alt_dx_v2_finalize.R': b'finalize',
            'docs/report_alt_dx_v1_v2_change_log.md': b'change log',
        }
        for name, content in self.files.items():
            path = self.root / name
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_bytes(content)
        self.manifest = self.run / 'frozen_specification.json'
        self.manifest.write_text(json.dumps({'file_sha256': {
            name: hashlib.sha256(content).hexdigest() for name, content in self.files.items()
        }, 'historical_metadata': 'unchanged'}))
        self.original = self.manifest.read_bytes()
        self.root_patch = patch.object(classifier, 'ROOT', self.root)
        self.root_patch.start()
        self.addCleanup(self.root_patch.stop)

    def verify_unchanged(self):
        self.assertEqual(self.manifest.read_bytes(), self.original)

    def test_valid_new_layout(self):
        self.assertEqual(classifier.verify_freeze(self.run)['historical_metadata'], 'unchanged')
        self.verify_unchanged()

    def test_old_filename_does_not_match_new_layout(self):
        frozen = json.loads(self.manifest.read_text())
        hashes = frozen['file_sha256']
        hashes['R/20_report_alt_dx_calibration_ingest.R'] = hashes.pop('R/17_report_alt_dx_calibration_ingest.R')
        self.manifest.write_text(json.dumps(frozen))
        self.original = self.manifest.read_bytes()
        with self.assertRaisesRegex(RuntimeError, r'missing: R/20_.*9336d82'):
            classifier.verify_freeze(self.run)
        self.verify_unchanged()

    def test_missing_frozen_changelog_fails(self):
        (self.root / 'docs/report_alt_dx_v1_v2_change_log.md').unlink()
        with self.assertRaisesRegex(RuntimeError, 'missing: docs/report_alt_dx_v1_v2_change_log.md'):
            classifier.verify_freeze(self.run)
        self.verify_unchanged()

    def test_changed_content_fails(self):
        (self.root / 'R/17_report_alt_dx_calibration_ingest.R').write_bytes(b'changed')
        with self.assertRaisesRegex(RuntimeError, 'hash changed: R/17_'):
            classifier.verify_freeze(self.run)
        self.verify_unchanged()


class EntryPointTests(unittest.TestCase):
    def test_optimized_python_refused_before_data_access(self):
        for script in ('run_classifier_v2.py', 'finalize_v2_package.py'):
            for flag in ('-O', '-OO'):
                with self.subTest(script=script, flag=flag):
                    proc = subprocess.run([sys.executable, flag, str(SCRIPTS / script)],
                                          capture_output=True, text=True)
                    self.assertNotEqual(proc.returncode, 0)
                    self.assertIn('Optimized Python is not supported', proc.stderr)
            env = dict(os.environ, PYTHONOPTIMIZE='1')
            proc = subprocess.run([sys.executable, str(SCRIPTS / script)], env=env,
                                  capture_output=True, text=True)
            self.assertNotEqual(proc.returncode, 0)
            self.assertIn('Optimized Python is not supported', proc.stderr)

    def test_finalizer_inventory_r_paths_resolve(self):
        tree = ast.parse((SCRIPTS / 'finalize_v2_package.py').read_text())
        literals = {node.value for node in ast.walk(tree) if isinstance(node, ast.Constant)
                    and isinstance(node.value, str)}
        names = {'17_report_alt_dx_calibration_ingest.R', '18_report_alt_dx_v2_finalize.R',
                 '19_report_alt_dx_v2_handoff.R'}
        self.assertTrue(names <= literals)
        self.assertFalse({'20_report_alt_dx_calibration_ingest.R', '21_report_alt_dx_v2_finalize.R',
                          '22_report_alt_dx_v2_handoff.R'} & literals)
        for name in names:
            self.assertTrue((ROOT / 'R' / name).is_file(), name)


if __name__ == '__main__':
    unittest.main()
