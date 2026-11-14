#!/usr/bin/env python3
"""Final v2 cycle. Private inputs/logs; frozen, resumable, independent passes."""
from __future__ import annotations
import argparse, concurrent.futures, hashlib, json, os, random, shutil, subprocess, tempfile
from datetime import datetime, timezone
from pathlib import Path
import sys
if sys.flags.optimize:
    raise SystemExit('Optimized Python is not supported: freeze and inference gates require assertions.')
sys.dont_write_bytecode = True
sys.path.insert(0, str(Path(__file__).parent))
from run_classifier import build_batches, validate_batch_result, strip_json_fence, resolve_codex

ROOT = Path(__file__).resolve().parents[2]
def data_root():
    root = os.environ.get('RESEARCH_DATA_ROOT')
    if not root:
        raise SystemExit('Set RESEARCH_DATA_ROOT to the external research-data root.')
    data = Path(root) / 'radiology/cta-aortic-dissection/derived/report_alt_dx'
    if not data.is_dir():
        raise SystemExit('The report alternative-diagnosis data directory does not exist under RESEARCH_DATA_ROOT.')
    return data

def v1_root():
    return data_root() / 'report_alt_dx_v1_20260910_01'
def now(): return datetime.now(timezone.utc).isoformat()
def sha(p): return hashlib.sha256(Path(p).read_bytes()).hexdigest()
def read(p): return json.loads(Path(p).read_text())
def write(p, obj):
    p=Path(p); p.parent.mkdir(parents=True,exist_ok=True)
    p.write_text(json.dumps(obj,indent=2,ensure_ascii=False)+'\n'); p.chmod(0o600)
def runtime():
    p=subprocess.run([str(resolve_codex()),'--version'],capture_output=True,text=True,check=True)
    return p.stdout.strip()

def validate_schema(obj, schema):
    """Validate every constraint used by this project's closed JSON schemas."""
    kind=schema.get('type')
    types={'object':dict,'array':list,'string':str,'integer':int,'boolean':bool}
    if kind and (not isinstance(obj,types[kind]) or (kind=='integer' and isinstance(obj,bool))):
        raise ValueError('Schema type validation failed')
    if 'enum' in schema and obj not in schema['enum']:raise ValueError('Schema enum validation failed')
    if kind=='object':
        props=schema.get('properties',{})
        if not set(schema.get('required',[]))<=set(obj):raise ValueError('Schema required field missing')
        if schema.get('additionalProperties') is False and not set(obj)<=set(props):raise ValueError('Schema extra field')
        for k,v in obj.items():
            if k in props:validate_schema(v,props[k])
    elif kind=='array':
        for item in obj:validate_schema(item,schema['items'])

def invoke(prompt, schema, output, metadata, model='gpt-6-astra', effort='high'):
    """Fresh session; request contains only the passed prompt and JSON payload."""
    if output.exists(): raise RuntimeError('Refusing to overwrite classifier response')
    start=now()
    with tempfile.TemporaryDirectory(prefix='report_alt_dx_v2_') as tmp:
        command=[str(resolve_codex()),'exec','--ephemeral','--ignore-user-config','--ignore-rules',
          '--skip-git-repo-check','--sandbox','read-only','--model',model,
          '-c',f'model_reasoning_effort="{effort}"','--json','--output-schema',str(schema),
          '--output-last-message',str(output),'--cd',tmp,'-']
        p=subprocess.run(command,input=prompt,text=True,capture_output=True,timeout=1800)
        logbase=output.with_suffix('')
        logbase.with_suffix('.stdout.private.log').write_text(p.stdout)
        logbase.with_suffix('.stderr.private.log').write_text(p.stderr)
        events=[]
        for line in p.stdout.splitlines():
            try: events.append(json.loads(line))
            except json.JSONDecodeError: pass
        tools=[e for e in events if e.get('item',{}).get('type') in
               ['command_execution','web_search','mcp_tool_call','file_change']]
        model_ids=sorted({str(e[k]) for e in events for k in ['model','model_id','backend_model'] if e.get(k)})
        write(metadata,{'started_utc':start,'completed_utc':now(),'returncode':p.returncode,
             'model_requested':model,'backend_identifiers_exposed':model_ids,
             'tool_events_detected':len(tools),'runtime':runtime(),
             'reasoning_effort':effort,'temperature':'not exposed/configured',
             'events_usage':[e.get('usage') for e in events if e.get('usage')]})
        if tools: raise RuntimeError('Classifier invoked a prohibited tool; response rejected')
        if p.returncode or not output.exists(): raise RuntimeError('Classifier call failed; details retained privately')
        obj=json.loads(strip_json_fence(output.read_text()))
        validate_schema(obj,read(schema))
        return obj

def synthesis(run):
    prompt_path=ROOT/'prompts/report_alt_dx/v2/calibration_synthesis_prompt.md'
    schema=ROOT/'config/report_alt_dx_calibration_synthesis_schema.json'
    inp=run/'calibration/private_comments_synthesis_input.json'
    output=run/'calibration/private_comments_synthesis.json'
    if output.exists():
        validate_schema(read(output),read(schema))
        print('Existing calibration synthesis schema validated; response preserved without rerun.',flush=True)
        return
    invoke(prompt_path.read_text()+'\nINPUT JSON:\n'+inp.read_text(),schema,output,
           run/'calibration/comments_synthesis_runtime.json')
    print('Calibration-comment synthesis complete; source comments and row references retained privately.',flush=True)

def freeze(run):
    V1 = v1_root()
    target=run/'frozen_specification.json'
    if target.exists(): raise RuntimeError('v2 is already frozen')
    config=read(ROOT/'config/report_alt_dx_v2.json')
    paths=[config['prompt_path'],config['schema_path'],'config/report_alt_dx_v2.json',
      'docs/report_alt_dx_v2_specification.md','docs/report_alt_dx_v1_v2_change_log.md',
      'R/17_report_alt_dx_calibration_ingest.R','R/18_report_alt_dx_v2_finalize.R',
      'R/11_report_alt_dx_validate.R',
      'scripts/report_alt_dx/run_classifier_v2.py']
    hashes={p:sha(ROOT/p) for p in paths}
    base_files=[p for p in V1.rglob('*') if p.is_file()]
    v1_hashes={str(p.relative_to(V1)):sha(p) for p in base_files}
    write(run/'preservation/v1_file_hashes.json',v1_hashes)
    tracked=subprocess.run(['git','ls-files'],cwd=ROOT,text=True,capture_output=True,check=True).stdout.splitlines()
    primary={p:sha(ROOT/p) for p in tracked if p.startswith(('R/0','results/','docs/report_alt_dx_','config/report_alt_dx_v1','config/report_alt_dx_schema_v1','prompts/report_alt_dx/v1','scripts/report_alt_dx/run_classifier.py')) and not p.startswith('results/report_alt_dx_v2/')}
    write(run/'preservation/repository_file_hashes.json',primary)
    for folder in ['inputs']:
        if (run/folder).exists():
            originals=sorted((V1/folder).glob('*.json'))
            assert {p.name for p in originals}=={p.name for p in (run/folder).glob('*.json')}
            assert all(sha(p)==sha(run/folder/p.name) for p in originals)
        else:shutil.copytree(V1/folder,run/folder,dirs_exist_ok=False)
    for filename in ['private_encounter_crosswalk.rds','private_report_evidence_map.rds']:
        shutil.copy2(V1/filename,run/filename)
    # Explicit allowlist check; no provider/site/outcomes or previous passes.
    packets=[read(p) for p in sorted((run/'inputs').glob('RDX-*.json'))]
    assert len(packets)==1111
    assert len({x['report_audit_id'] for x in packets})==1111
    for p in packets:
        assert set(p)=={'report_audit_id','reports','clinical_indications'}
        assert all(set(r)=={'source_report_key','component','text_source','report_text'} for r in p['reports'])
        indications=p['clinical_indications']
        # R's named list serializes exact-deduplicated indications as an object
        # keyed by text hash. Preserve those v1 bytes and check the values.
        if isinstance(indications,dict):indications=indications.values()
        assert all(set(i)=={'component','indication_text'} for i in indications)
    write(target,{'run_id':run.name,'frozen_utc':now(),'file_sha256':hashes,
        'input_origin':V1.name,'n_inputs':len(packets),'input_hash_manifest_sha256':sha(run/'preservation/v1_file_hashes.json'),
        'primary_pass':1,'reproducibility_pass':2,'endpoint_rule_version':'v1_unchanged',
        'calibration_workbook':read(run/'calibration/ingestion_manifest.json'),
        'model_alias':config['model_alias'],'runtime_at_freeze':runtime(),
        'v3_authorized':False,'cohort_results_viewed_at_freeze':False})
    public=read(target)
    public['calibration_workbook'].pop('workbook_path',None)
    write(ROOT/'results/report_alt_dx_v2/frozen_specification.json',public)
    print('v2 frozen before any cohort inference; 1,111 allowlisted inputs copied; v1/primary preservation hashes saved.',flush=True)

def verify_freeze(run):
    frozen=read(run/'frozen_specification.json')
    for p,h in frozen['file_sha256'].items():
        source = ROOT/p
        if not source.is_file():
            hint = '; check historical source snapshot 9336d82' if p.startswith(('R/20_', 'R/21_', 'R/22_')) else ''
            raise RuntimeError('Frozen v2 file missing: '+p+hint)
        if sha(source)!=h:
            raise RuntimeError('Frozen v2 file hash changed: '+p+'; use the matching frozen source snapshot')
    return frozen

def batch_run(number,batch,passdir,prompt,schema,config):
    expected=[x['report_audit_id'] for x in batch]
    dest=passdir/f'batch_{number:04d}.json'
    if dest.exists():
        validate_batch_result(read(dest),expected)
        return number
    for attempt in range(1,int(config['max_attempts'])+1):
        base=passdir/f'attempt_batch_{number:04d}_{attempt}'
        if base.with_suffix('.json').exists(): continue
        try:
            result=invoke(prompt+'\n\nINPUT JSON:\n'+json.dumps({'encounters':batch},ensure_ascii=False),
                schema,base.with_suffix('.json'),base.with_suffix('.metadata.json'),
                config['model_alias'],config['reasoning_effort'])
            validate_batch_result(result,expected)
            write(dest,result)
            return number
        except (RuntimeError,ValueError,subprocess.TimeoutExpired):
            if attempt==int(config['max_attempts']): raise RuntimeError(f'Batch {number} failed; inspect private logs')
    raise RuntimeError(f'Batch {number} exhausted saved attempts')

def run_pass(run,passnum):
    frozen=verify_freeze(run)
    config=read(ROOT/'config/report_alt_dx_v2.json')
    prompt=(ROOT/config['prompt_path']).read_text(); schema=ROOT/config['schema_path']
    packets=[read(p) for p in sorted((run/'inputs').glob('RDX-*.json'))]
    if passnum==2: random.Random(config['pass_2_shuffle_seed']).shuffle(packets)
    batches=build_batches(packets,config['batch_size'],config['max_batch_characters'])
    out=run/f'pass{passnum}';out.mkdir(exist_ok=True)
    if (out/'pass_manifest.json').exists():
        print(f'Pass {passnum} already complete; preserved.',flush=True);return
    start=out/'start_manifest.json'
    if not start.exists():write(start,{'started_utc':now(),'frozen_specification_sha256':sha(run/'frozen_specification.json'),'pass':passnum,'n_batches':len(batches)})
    print(f'Pass {passnum}: {len(packets)} encounters, {len(batches)} batches, {config["concurrent_batches"]} workers.',flush=True)
    with concurrent.futures.ThreadPoolExecutor(max_workers=config['concurrent_batches']) as pool:
        fs=[pool.submit(batch_run,i,b,out,prompt,schema,config) for i,b in enumerate(batches,1)]
        for n,f in enumerate(concurrent.futures.as_completed(fs),1):
            f.result()
            if n==1 or n%5==0 or n==len(fs): print(f'Pass {passnum}: {n}/{len(fs)} batches complete.',flush=True)
    objs=[read(out/f'batch_{i:04d}.json') for i in range(1,len(batches)+1)]
    combined={'classifications':[c for o in objs for c in o['classifications']]}
    validate_batch_result(combined,[x['report_audit_id'] for x in packets])
    verify_freeze(run)
    write(out/'pass_manifest.json',{'run_id':run.name,'pass':passnum,'started_utc':read(start)['started_utc'],
      'completed_utc':now(),'n_encounters':len(packets),'n_batches':len(batches),
      'model_alias_requested':config['model_alias'],'backend_model_identifier':'not emitted if metadata lists are empty',
      'runtime':runtime(),'reasoning_effort':config['reasoning_effort'],'temperature':'not exposed/configured',
      'frozen_specification_sha256':sha(run/'frozen_specification.json'),
      'batch_sha256':{f'batch_{i:04d}.json':sha(out/f'batch_{i:04d}.json') for i in range(1,len(batches)+1)},
      'independence':'Fresh ephemeral sessions and isolated working directories; shuffled order for pass 2; no prior answers or prohibited fields in input.'})
    print(f'Pass {passnum}: coverage, schema, information firewall, and frozen-file checks passed.',flush=True)

def preservation(run):
    V1 = v1_root()
    bad=[]
    for rel,h in read(run/'preservation/v1_file_hashes.json').items():
        if sha(V1/rel)!=h:bad.append('v1:'+rel)
    for rel,h in read(run/'preservation/repository_file_hashes.json').items():
        if sha(ROOT/rel)!=h:bad.append('repo:'+rel)
    if bad:raise RuntimeError('Preservation failure; '+str(len(bad))+' files changed')
    print('Preservation check: all baseline v1 and primary-analysis files unchanged.',flush=True)

if __name__=='__main__':
    ap=argparse.ArgumentParser();ap.add_argument('action',choices=['synthesize','freeze','pass1','pass2','verify']);ap.add_argument('--run-id',default='report_alt_dx_v2_20260924_01');a=ap.parse_args()
    run=data_root()/a.run_id
    if a.action=='synthesize':synthesis(run)
    elif a.action=='freeze':freeze(run)
    elif a.action=='verify':verify_freeze(run);preservation(run)
    else:run_pass(run,int(a.action[-1]))
