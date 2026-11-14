#!/usr/bin/env python3
"""Final metadata, inventory and privacy/preservation gates; no inference."""
from __future__ import annotations
import csv, hashlib, json, os, re, shutil, subprocess, sys
from datetime import datetime, timezone
from pathlib import Path
if sys.flags.optimize:
    raise SystemExit('Optimized Python is not supported: finalization gates require assertions.')
sys.dont_write_bytecode = True
sys.path.insert(0, str(Path(__file__).parent))
from run_classifier_v2 import ROOT, data_root, read, sha, verify_freeze, preservation, strip_json_fence

RUN_ID = 'report_alt_dx_v2_20260924_01'
run = data_root() / RUN_ID
rscript = shutil.which(os.environ.get('RSCRIPT_EXECUTABLE') or 'Rscript')
if not rscript:
    raise SystemExit('Rscript not found. Set RSCRIPT_EXECUTABLE or add Rscript to PATH.')
out = ROOT / 'results/report_alt_dx_v2'
branch = subprocess.run(['git','branch','--show-current'],cwd=ROOT,capture_output=True,text=True,check=True).stdout.strip()
assert branch == 'codex/r-rebuild-first-pass', 'Wrong branch; no finalization writes permitted'
frozen = verify_freeze(run)
assert (out / 'final_handoff_provenance.json').exists(), 'Run both final R stages first'
preservation(run)
input_hashes = read(run/'preservation/v1_file_hashes.json')
input_paths = [rel for rel in input_hashes if rel.startswith('inputs/') and rel.endswith('.json')]
assert len(input_paths) == 1111
for rel in input_paths + ['private_encounter_crosswalk.rds','private_report_evidence_map.rds']:
    assert sha(run/rel) == input_hashes[rel], 'v2 input or mapping differs from frozen v1 source'
tests = subprocess.run([rscript,'--no-init-file','--no-site-file',
    '--no-restore','--no-save','tests/test_report_alt_dx_v2.R'],cwd=ROOT,
    capture_output=True,text=True,check=True)
assert '542 encounters per pass' in tests.stdout
final_tests = subprocess.run([rscript,'--no-init-file','--no-site-file',
    '--no-restore','--no-save','tests/test_report_alt_dx_v2_final_results.R',RUN_ID],
    cwd=ROOT,capture_output=True,text=True,check=True)
assert 'Final aggregate reconciliation passed' in final_tests.stdout

runtime = []
for p in [1,2]:
    d = run / f'pass{p}'
    manifest = read(d / 'pass_manifest.json')
    assert frozen['frozen_utc'] < manifest['started_utc'] < manifest['completed_utc']
    metadata = [read(f) for f in sorted(d.glob('attempt_batch_*.metadata.json'))]
    assert len(manifest['batch_sha256']) == manifest['n_batches'] == 93
    for name,h in manifest['batch_sha256'].items():
        assert sha(d/name) == h
        obj = read(d/name)
        # Accepted output must originate verbatim in JSON meaning from a saved
        # response whose invocation had no prohibited tool events.
        number = int(name[6:10])
        accepted = []
        for f in d.glob(f'attempt_batch_{number:04d}_*.json'):
            if f.name.endswith('.metadata.json'): continue
            try: matches = json.loads(strip_json_fence(f.read_text())) == obj
            except json.JSONDecodeError: matches = False
            if matches:
                m = read(f.with_suffix('.metadata.json'))
                assert m['tool_events_detected'] == 0 and m['returncode'] == 0
                accepted.append(f)
        assert accepted, 'Accepted batch lacks verified response provenance'
    runtime.append({
        'pass':p,'analytic_role':'primary' if p==1 else 'reproducibility',
        'run_id':RUN_ID,'started_utc':manifest['started_utc'],
        'completed_utc':manifest['completed_utc'],'n_encounters':manifest['n_encounters'],
        'n_batches':manifest['n_batches'],'n_completed_attempts':len(metadata),
        'n_nonzero_exit_attempts':sum(m['returncode']!=0 for m in metadata),
        'n_attempts_with_prohibited_tool_events':sum(m['tool_events_detected']>0 for m in metadata),
        'accepted_responses_tool_events':0,
        'model_alias_requested':manifest['model_alias_requested'],
        'backend_identifiers_exposed':sorted({x for m in metadata for x in m['backend_identifiers_exposed']}),
        'backend_snapshot_limitation':'Exact immutable model snapshot unavailable unless explicitly exposed above',
        'runtime':manifest['runtime'],'reasoning_effort':manifest['reasoning_effort'],
        'temperature':manifest['temperature'],
        'pass_manifest_sha256':sha(d/'pass_manifest.json'),
        'frozen_specification_sha256':manifest['frozen_specification_sha256'],
        'independence':manifest['independence']})

def public_json(name,obj):
    (out/name).write_text(json.dumps(obj,indent=2,ensure_ascii=False)+'\n')

public_json('runtime_metadata.json',{'run_id':RUN_ID,'passes':runtime,
    'frozen_file_sha256':frozen['file_sha256'],
    'reproducibility_scope':'Saved inputs/prompts/responses and hashes reproduce the reported calculations; stochastic inference regeneration is not guaranteed to be identical.'})

# The repository outputs have aggregate-only schemas. This supplemental scan is
# intentionally conservative, not a claim of comprehensive clinical de-ID.
for f in out.iterdir():
    if f.suffix not in ['.csv','.json','.md']: continue
    assert not re.search(r'RDX-\d{4}|\b(?:MRN|EMPI)\s*[:=]\s*\w+',f.read_text()), 'Identifier pattern in aggregate output'
    if f.suffix == '.csv':
        with f.open(newline='') as stream:
            fields = next(csv.reader(stream))
        assert not set(fields) & {'report_audit_id','candidate_id','encounter','patient_id','report_text','clinical_indication','report_evidence_quote','indication_evidence_quote'}

public_json('closure_validation.json',{
    'run_id':RUN_ID,'branch':branch,'checked_utc':datetime.now(timezone.utc).isoformat(),
    'v1_preservation_passed':True,'primary_preservation_passed':True,
    'n_v1_files_verified':len(read(run/'preservation/v1_file_hashes.json')),
    'n_repository_baseline_files_verified':len(read(run/'preservation/repository_file_hashes.json')),
    'frozen_before_both_passes':True,'accepted_response_provenance_passed':True,
    'v2_inputs_and_private_maps_byte_identical_to_frozen_v1':True,
    'synthetic_tests_per_pass':542,'final_aggregate_reconciliation_passed':True,
    'aggregate_identifier_schema_scan_passed':True,
    'classifier_v3_created':False,'manual_cohort_record_patches':False,
    'outcome_based_tuning':False,'status':'Closed unless a hard technical defect is discovered',
    'script_sha256':sha(__file__)})

public_files = [ROOT/'.gitignore']
public_files += [ROOT/'R'/f for f in ['17_report_alt_dx_calibration_ingest.R','18_report_alt_dx_v2_finalize.R','19_report_alt_dx_v2_handoff.R']]
public_files += [ROOT/'config'/f for f in ['report_alt_dx_calibration_synthesis_schema.json','report_alt_dx_schema_v2.json','report_alt_dx_v2.json']]
public_files += sorted((ROOT/'prompts/report_alt_dx/v2').glob('*.md'))
public_files += sorted((ROOT/'docs').glob('report_alt_dx_v2_*.md'))
public_files += [ROOT/'docs/report_alt_dx_v1_v2_change_log.md',Path(__file__).resolve(),ROOT/'scripts/report_alt_dx/run_classifier_v2.py',ROOT/'tests/test_report_alt_dx_v2.R',ROOT/'tests/test_report_alt_dx_v2_final_results.R']
public_files += [f for f in sorted(out.iterdir()) if f.name!='output_file_inventory.csv']
assert all(f.is_file() for f in public_files)
with (out/'output_file_inventory.csv').open('w',newline='') as stream:
    w=csv.writer(stream,lineterminator='\n');w.writerow(['repository_path','sha256','status'])
    for f in sorted(set(public_files)):
        w.writerow([str(f.relative_to(ROOT)),sha(f),'modified' if f.name=='.gitignore' else 'created'])
    w.writerow(['results/report_alt_dx_v2/output_file_inventory.csv','self-manifest; verify with Git object','created'])

# Full private file inventory is retained outside Git, including patient-level
# packets, outputs, evidence and clinician-derived 25-encounter table.
with (run/'finalization_file_inventory.csv').open('w',newline='') as stream:
    w=csv.writer(stream,lineterminator='\n');w.writerow(['absolute_path','sha256'])
    for f in sorted(run.rglob('*')):
        if f.is_file() and f.name!='finalization_file_inventory.csv':w.writerow([str(f),sha(f)])
print('Final package gates passed: complete independent passes, original-response provenance, frozen rules, synthetic endpoint tests, aggregate-only outputs, v1 and primary preservation.')
