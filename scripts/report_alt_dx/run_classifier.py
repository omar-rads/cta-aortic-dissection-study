#!/usr/bin/env python3
"""Run independent schema-constrained report classifiers without terminal PHI."""

from __future__ import annotations

import argparse
import concurrent.futures
import hashlib
import json
import os
from pathlib import Path
import random
import re
import shutil
import subprocess
import tempfile
from datetime import datetime, timezone


REPO_ROOT = Path(__file__).resolve().parents[2]
def resolve_codex(override: Path | None = None) -> Path:
    """Resolve an explicit CLI choice, environment override, or PATH entry."""
    if override is not None:
        candidate = str(override.expanduser().resolve())
    else:
        candidate = os.environ.get("CODEX_EXECUTABLE") or "codex"
    resolved = shutil.which(candidate)
    if not resolved:
        raise SystemExit("Codex executable not found. Set CODEX_EXECUTABLE or pass --codex.")
    return Path(resolved)


def sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def load_json(path: Path):
    with path.open("r", encoding="utf-8") as handle:
        return json.load(handle)


def strip_json_fence(text: str) -> str:
    value = text.strip()
    match = re.fullmatch(r"```(?:json)?\s*(.*?)\s*```", value, flags=re.DOTALL)
    return match.group(1) if match else value


def build_batches(packets: list[dict], batch_size: int, max_chars: int) -> list[list[dict]]:
    batches: list[list[dict]] = []
    current: list[dict] = []
    current_chars = 0
    for packet in packets:
        packet_chars = len(json.dumps(packet, ensure_ascii=False))
        if current and (len(current) >= batch_size or current_chars + packet_chars > max_chars):
            batches.append(current)
            current = []
            current_chars = 0
        current.append(packet)
        current_chars += packet_chars
    if current:
        batches.append(current)
    return batches


def validate_batch_result(result: dict, expected_ids: list[str]) -> None:
    if not isinstance(result, dict) or set(result) != {"classifications"}:
        raise ValueError("Top-level classifier result does not match the frozen schema.")
    classifications = result["classifications"]
    if not isinstance(classifications, list):
        raise ValueError("Classifier result is not a classification list.")
    observed_ids = [item.get("report_audit_id") for item in classifications]
    if len(observed_ids) != len(set(observed_ids)):
        raise ValueError("Classifier result contains duplicate audit labels.")
    if set(observed_ids) != set(expected_ids):
        raise ValueError("Classifier result does not cover the requested audit-label set.")


def run_one_batch(
    batch_number: int,
    batch: list[dict],
    pass_dir: Path,
    prompt_text: str,
    schema_path: Path,
    config: dict,
    codex_path: Path,
) -> tuple[int, int]:
    expected_ids = [packet["report_audit_id"] for packet in batch]
    output_path = pass_dir / f"batch_{batch_number:04d}.json"
    if output_path.exists():
        raise RuntimeError("Refusing to overwrite an existing classifier batch.")

    input_payload = json.dumps({"encounters": batch}, ensure_ascii=False, separators=(",", ":"))
    complete_prompt = prompt_text + "\n\nINPUT JSON:\n" + input_payload

    attempts = int(config["max_attempts"])
    prior_attempt_numbers: list[int] = []
    attempt_pattern = re.compile(rf"(?:\\.|^)batch_{batch_number:04d}_attempt_([0-9]+)")
    for prior_path in pass_dir.glob(f"*batch_{batch_number:04d}_attempt_*"):
        match = attempt_pattern.search(prior_path.name)
        if match:
            prior_attempt_numbers.append(int(match.group(1)))
    attempt_offset = max(prior_attempt_numbers, default=0)
    for attempt in range(attempt_offset + 1, attempt_offset + attempts + 1):
        work_dir = Path(tempfile.mkdtemp(prefix="report_alt_dx_classifier_"))
        temporary_output = pass_dir / f".batch_{batch_number:04d}_attempt_{attempt}.json"
        command = [
            str(codex_path),
            "exec",
            "--ephemeral",
            "--ignore-user-config",
            "--ignore-rules",
            "--skip-git-repo-check",
            "--sandbox",
            "read-only",
            "--model",
            config["model_alias"],
            "-c",
            f'model_reasoning_effort="{config["reasoning_effort"]}"',
            "--output-schema",
            str(schema_path),
            "--output-last-message",
            str(temporary_output),
            "--cd",
            str(work_dir),
            "-",
        ]
        completed = None
        try:
            completed = subprocess.run(
                command,
                input=complete_prompt,
                text=True,
                capture_output=True,
                timeout=1800,
                check=False,
                env=os.environ.copy(),
            )
            (pass_dir / f"batch_{batch_number:04d}_attempt_{attempt}.stdout.private.log").write_text(
                completed.stdout, encoding="utf-8"
            )
            (pass_dir / f"batch_{batch_number:04d}_attempt_{attempt}.stderr.private.log").write_text(
                completed.stderr, encoding="utf-8"
            )
            if completed.returncode != 0 or not temporary_output.exists():
                continue
            result = json.loads(strip_json_fence(temporary_output.read_text(encoding="utf-8")))
            validate_batch_result(result, expected_ids)
            output_path.write_text(
                json.dumps(result, ensure_ascii=False, indent=2) + "\n", encoding="utf-8"
            )
            temporary_output.unlink(missing_ok=True)
            return batch_number, attempt
        except (subprocess.TimeoutExpired, json.JSONDecodeError, ValueError):
            pass
        finally:
            temporary_output.unlink(missing_ok=True)
            shutil.rmtree(work_dir, ignore_errors=True)

    raise RuntimeError(
        f"Classifier batch {batch_number} failed after {attempts} new attempts; private logs were retained."
    )


def classifier_version(codex_path: Path) -> str:
    completed = subprocess.run(
        [str(codex_path), "--version"], text=True, capture_output=True, check=False, timeout=30
    )
    value = (completed.stdout or completed.stderr).strip()
    return value if completed.returncode == 0 else "unavailable"


def run_synthetic_test(config: dict, prompt_text: str, schema_path: Path, codex_path: Path) -> None:
    synthetic = [
        {
            "report_audit_id": "RDX-SYNTHETIC",
            "reports": [
                {
                    "source_report_key": "SR-01",
                    "component": "chest",
                    "text_source": "impression",
                    "report_text": "No pulmonary embolism. Mild dependent atelectasis.",
                }
            ],
            "clinical_indications": [
                {"component": "chest", "indication_text": "Acute chest and back pain"}
            ],
        }
    ]
    with tempfile.TemporaryDirectory(prefix="report_alt_dx_synthetic_") as tmp:
        synthetic_dir = Path(tmp)
        run_one_batch(1, synthetic, synthetic_dir, prompt_text, schema_path, config, codex_path)
        result = load_json(synthetic_dir / "batch_0001.json")
        validate_batch_result(result, ["RDX-SYNTHETIC"])
    print("Synthetic schema-constrained classifier test: PASS")


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--run-dir", type=Path)
    parser.add_argument("--pass", dest="pass_number", type=int, choices=(1, 2))
    parser.add_argument("--synthetic-test", action="store_true")
    parser.add_argument("--codex", type=Path, help="Path to the Codex executable")
    parser.add_argument("--workers", type=int)
    args = parser.parse_args()

    config_path = REPO_ROOT / "config" / "report_alt_dx_v1.json"
    config = load_json(config_path)
    prompt_path = REPO_ROOT / config["prompt_path"]
    schema_path = REPO_ROOT / config["schema_path"]
    prompt_text = prompt_path.read_text(encoding="utf-8")

    args.codex = resolve_codex(args.codex)

    if args.synthetic_test:
        run_synthetic_test(config, prompt_text, schema_path, args.codex)
        return

    if args.run_dir is None or args.pass_number is None:
        raise SystemExit("--run-dir and --pass are required for a cohort run.")

    run_dir = args.run_dir.resolve()
    pass_dir = run_dir / f"pass{args.pass_number}"
    input_dir = run_dir / "inputs"
    pass_dir.mkdir(parents=True, exist_ok=True)

    input_paths = sorted(input_dir.glob("RDX-*.json"))
    packets = [load_json(path) for path in input_paths]
    if len(packets) != 1111:
        raise SystemExit("Prepared classifier input count is not the expected 1,111 encounters.")

    if args.pass_number == 2:
        random.Random(int(config["pass_2_shuffle_seed"])).shuffle(packets)

    batches = build_batches(
        packets,
        batch_size=int(config["batch_size"]),
        max_chars=int(config["max_batch_characters"]),
    )
    completed_before_resume: set[int] = set()
    for batch_number, batch in enumerate(batches, start=1):
        output_path = pass_dir / f"batch_{batch_number:04d}.json"
        if not output_path.exists():
            continue
        result = load_json(output_path)
        validate_batch_result(result, [packet["report_audit_id"] for packet in batch])
        completed_before_resume.add(batch_number)

    print(
        f"Classifier pass {args.pass_number}: {len(packets)} encounters in {len(batches)} batches; "
        f"{len(completed_before_resume)} validated batches already complete; no clinical text will be printed.",
        flush=True,
    )

    attempts_used: dict[int, int] = {}
    workers = args.workers if args.workers is not None else int(config["concurrent_batches"])
    if workers < 1 or workers > 8:
        raise SystemExit("--workers must be between 1 and 8.")
    with concurrent.futures.ThreadPoolExecutor(max_workers=workers) as executor:
        futures = {
            executor.submit(
                run_one_batch,
                batch_number,
                batch,
                pass_dir,
                prompt_text,
                schema_path,
                config,
                args.codex,
            ): batch_number
            for batch_number, batch in enumerate(batches, start=1)
            if batch_number not in completed_before_resume
        }
        completed_count = len(completed_before_resume)
        try:
            for future in concurrent.futures.as_completed(futures):
                batch_number, attempt = future.result()
                attempts_used[batch_number] = attempt
                completed_count += 1
                if completed_count == 1 or completed_count % 10 == 0 or completed_count == len(batches):
                    print(
                        f"Classifier pass {args.pass_number}: {completed_count}/{len(batches)} batches complete.",
                        flush=True,
                    )
        except Exception:
            for future in futures:
                future.cancel()
            raise

    combined_ids: list[str] = []
    for output_path in sorted(pass_dir.glob("batch_*.json")):
        result = load_json(output_path)
        combined_ids.extend(item["report_audit_id"] for item in result["classifications"])
    expected_ids = [packet["report_audit_id"] for packet in packets]
    if len(combined_ids) != len(set(combined_ids)) or set(combined_ids) != set(expected_ids):
        raise SystemExit("Completed pass failed exact coverage/uniqueness validation.")

    manifest = {
        "run_id": run_dir.name,
        "pass": args.pass_number,
        "started_and_completed_utc": datetime.now(timezone.utc).isoformat(),
        "classification_count": len(combined_ids),
        "batch_count": len(batches),
        "concurrent_batches_actual": workers,
        "validated_batches_resumed": len(completed_before_resume),
        "model_alias_requested": config["model_alias"],
        "backend_model_identifier_emitted": None,
        "reasoning_effort": config["reasoning_effort"],
        "temperature": "not exposed/configured by Codex CLI",
        "codex_cli_version": classifier_version(args.codex),
        "prompt_path": str(prompt_path.relative_to(REPO_ROOT)),
        "prompt_sha256": sha256_file(prompt_path),
        "schema_path": str(schema_path.relative_to(REPO_ROOT)),
        "schema_sha256": sha256_file(schema_path),
        "config_path": str(config_path.relative_to(REPO_ROOT)),
        "config_sha256": sha256_file(config_path),
        "attempts_used_by_batch": attempts_used,
        "failed_responses_retained": "private pass directory; excluded from analysis",
        "pass_independence": (
            "ephemeral sessions and isolated temporary working directories; pass 2 input order shuffled; "
            "pass 1 outputs, historical results, provider/site, incidental-field regex, and algorithm class absent from prompts"
        ),
    }
    (pass_dir / "pass_manifest.json").write_text(
        json.dumps(manifest, indent=2) + "\n", encoding="utf-8"
    )
    print(
        f"Classifier pass {args.pass_number}: exact coverage and uniqueness checks PASS.",
        flush=True,
    )


if __name__ == "__main__":
    main()
