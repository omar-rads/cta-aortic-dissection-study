#!/usr/bin/env python3
"""Offline checks for local path configuration and public inventory output."""
import ast
import csv
import hashlib
import importlib
import os
import shutil
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest
from unittest.mock import patch

ROOT = Path(__file__).resolve().parents[1]
SCRIPTS = ROOT / 'scripts/report_alt_dx'
sys.path.insert(0, str(SCRIPTS))
v1 = importlib.import_module('run_classifier')
v2 = importlib.import_module('run_classifier_v2')


class PortablePathsTest(unittest.TestCase):
    def test_help_needs_no_local_configuration(self):
        env = dict(os.environ, PYTHONDONTWRITEBYTECODE='1', PATH='')
        env.pop('RESEARCH_DATA_ROOT', None)
        env.pop('CODEX_EXECUTABLE', None)
        for script in ('run_classifier.py', 'run_classifier_v2.py'):
            result = subprocess.run([sys.executable, str(SCRIPTS / script), '--help'],
                                    env=env, capture_output=True, text=True)
            self.assertEqual(result.returncode, 0, result.stderr)

    def test_data_root_requires_existing_directory(self):
        with patch.dict(os.environ, {}, clear=True):
            with self.assertRaisesRegex(SystemExit, 'RESEARCH_DATA_ROOT'):
                v2.data_root()
        with tempfile.TemporaryDirectory() as tmp, patch.dict(os.environ, {'RESEARCH_DATA_ROOT': tmp}):
            with self.assertRaisesRegex(SystemExit, 'does not exist'):
                v2.data_root()
            expected = Path(tmp) / 'radiology/cta-aortic-dissection/derived/report_alt_dx'
            expected.mkdir(parents=True)
            self.assertEqual(v2.data_root(), expected)
            self.assertEqual(v2.v1_root().parent, expected)

    def test_executable_override_and_path(self):
        with tempfile.TemporaryDirectory() as tmp:
            folder = Path(tmp)
            choices = [folder / name for name in ('codex', 'override', 'cli')]
            for path in choices:
                path.write_text('#!/bin/sh\nexit 0\n')
                path.chmod(0o700)
            with patch.dict(os.environ, {'PATH': tmp, 'CODEX_EXECUTABLE': str(choices[1])}, clear=True):
                self.assertEqual(v1.resolve_codex(), choices[1])
                self.assertEqual(v1.resolve_codex(choices[2]), choices[2].resolve())
            with patch.dict(os.environ, {'PATH': tmp}, clear=True):
                self.assertEqual(v1.resolve_codex(), choices[0])
            with patch.dict(os.environ, {'PATH': ''}, clear=True):
                with self.assertRaisesRegex(SystemExit, 'CODEX_EXECUTABLE'):
                    v1.resolve_codex()
            previous = Path.cwd()
            try:
                os.chdir(folder)
                with patch.dict(os.environ, {'PATH': ''}, clear=True):
                    self.assertEqual(v1.resolve_codex(Path('./codex')), choices[0].resolve())
                    choices[2].chmod(0o600)
                    with self.assertRaisesRegex(SystemExit, 'Codex executable not found'):
                        v1.resolve_codex(Path('./cli'))
            finally:
                os.chdir(previous)

    def test_rscript_override_and_missing_tool(self):
        tree = ast.parse((SCRIPTS / 'finalize_v2_package.py').read_text())
        index = next(i for i, node in enumerate(tree.body)
                     if isinstance(node, ast.Assign) and any(
                         isinstance(target, ast.Name) and target.id == 'rscript'
                         for target in node.targets))
        code = compile(ast.fix_missing_locations(ast.Module(
            body=tree.body[index:index + 2], type_ignores=[])), '<rscript>', 'exec')
        with tempfile.TemporaryDirectory() as tmp:
            fake = Path(tmp) / 'fake-rscript'
            fake.write_text('#!/bin/sh\nexit 0\n')
            fake.chmod(0o700)
            with patch.dict(os.environ, {'RSCRIPT_EXECUTABLE': str(fake), 'PATH': ''}, clear=True):
                namespace = {'os': os, 'shutil': shutil}
                exec(code, namespace)
                self.assertEqual(namespace['rscript'], str(fake))
            with patch.dict(os.environ, {'PATH': ''}, clear=True):
                with self.assertRaisesRegex(SystemExit, 'RSCRIPT_EXECUTABLE'):
                    exec(code, {'os': os, 'shutil': shutil})

    def test_public_inventory_is_relative(self):
        # Execute only the inventory-writing block, never the finalization pipeline.
        tree = ast.parse((SCRIPTS / 'finalize_v2_package.py').read_text())
        block = next(node for node in tree.body if isinstance(node, ast.With)
                     and 'output_file_inventory.csv' in ast.unparse(node.items[0]))
        code = compile(ast.fix_missing_locations(ast.Module(body=[block], type_ignores=[])), '<inventory>', 'exec')
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            out = root / 'results/report_alt_dx_v2'
            out.mkdir(parents=True)
            item = root / 'sample.txt'
            item.write_text('example')
            sha = lambda path: hashlib.sha256(path.read_bytes()).hexdigest()
            exec(code, {'out': out, 'ROOT': root, 'public_files': [item], 'csv': csv, 'sha': sha})
            with (out / 'output_file_inventory.csv').open(newline='') as stream:
                rows = list(csv.DictReader(stream))
            self.assertEqual(list(rows[0]), ['repository_path', 'sha256', 'status'])
            self.assertEqual(rows[0], {'repository_path': 'sample.txt', 'sha256': sha(item), 'status': 'created'})
            self.assertTrue(all(not Path(row['repository_path']).is_absolute() for row in rows))
            self.assertEqual(rows[-1]['sha256'], 'self-manifest; verify with Git object')


if __name__ == '__main__':
    unittest.main()
