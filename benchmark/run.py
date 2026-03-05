#!/usr/bin/env python3
import argparse
import json
import os
import shutil
import statistics
import subprocess
import sys
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, List


@dataclass(frozen=True)
class BenchmarkSpec:
    name: str
    description: str


BENCHMARKS: List[BenchmarkSpec] = [
    BenchmarkSpec("sum_loop", "Integer-heavy pseudo-random accumulation loop"),
    BenchmarkSpec("prime_count", "Prime counting via sieve"),
    BenchmarkSpec("matrix_mul", "Dense matrix multiplication (100x100)"),
]

LANGUAGES = ("apex", "c", "rust")


def run_cmd(cmd: List[str], cwd: Path, env: Dict[str, str] | None = None) -> subprocess.CompletedProcess:
    merged_env = os.environ.copy()
    if env:
        merged_env.update(env)
    return subprocess.run(
        cmd,
        cwd=cwd,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        env=merged_env,
        check=False,
    )


def ensure_tool(name: str) -> None:
    if shutil.which(name) is None:
        raise RuntimeError(f"Required tool not found in PATH: {name}")


def parse_checksum(output: str) -> int:
    line = output.strip().splitlines()[-1].strip()
    return int(line)


def compile_apex(root: Path, bench: str, out: Path, build_env: Dict[str, str]) -> None:
    compiler = root / "target" / "release" / "apex-compiler"
    if not compiler.exists():
        raise RuntimeError(
            f"Apex compiler missing at {compiler}. Build it first or run without --no-build."
        )

    src = root / "benchmark" / "apex" / f"{bench}.apex"
    cmd = [str(compiler), "compile", str(src), "-o", str(out)]
    proc = run_cmd(cmd, root, env=build_env)
    if proc.returncode != 0:
        raise RuntimeError(f"Failed to compile Apex benchmark {bench}:\n{proc.stderr}")


def compile_c(root: Path, bench: str, out: Path) -> None:
    src = root / "benchmark" / "c" / f"{bench}.c"
    cmd = ["gcc", "-O3", "-march=native", "-std=c11", str(src), "-o", str(out)]
    proc = run_cmd(cmd, root)
    if proc.returncode != 0:
        raise RuntimeError(f"Failed to compile C benchmark {bench}:\n{proc.stderr}")


def compile_rust(root: Path, bench: str, out: Path) -> None:
    src = root / "benchmark" / "rust" / f"{bench}.rs"
    cmd = ["rustc", "-C", "opt-level=3", "-C", "target-cpu=native", str(src), "-o", str(out)]
    proc = run_cmd(cmd, root)
    if proc.returncode != 0:
        raise RuntimeError(f"Failed to compile Rust benchmark {bench}:\n{proc.stderr}")


def timed_run(binary: Path, cwd: Path) -> (float, int):
    start = time.perf_counter()
    proc = run_cmd([str(binary)], cwd)
    elapsed = time.perf_counter() - start
    if proc.returncode != 0:
        raise RuntimeError(
            f"Benchmark execution failed: {binary}\nstdout:\n{proc.stdout}\nstderr:\n{proc.stderr}"
        )
    checksum = parse_checksum(proc.stdout)
    return elapsed, checksum


def compute_stats(values: List[float]) -> Dict[str, float]:
    return {
        "min_s": min(values),
        "mean_s": statistics.mean(values),
        "median_s": statistics.median(values),
        "max_s": max(values),
        "stddev_s": statistics.pstdev(values) if len(values) > 1 else 0.0,
    }


def format_seconds(x: float) -> str:
    return f"{x:.6f}"


def build_markdown(result: Dict) -> str:
    lines: List[str] = []
    lines.append("# Benchmark Report")
    lines.append("")
    lines.append(f"- Generated: `{result['generated_at']}`")
    lines.append(f"- Repeats: `{result['repeats']}`")
    lines.append(f"- Warmup runs: `{result['warmup']}`")
    lines.append("")

    for bench in result["benchmarks"]:
        lines.append(f"## {bench['name']}")
        lines.append("")
        lines.append(f"{bench['description']}")
        lines.append("")
        lines.append("| Language | Checksum | min (s) | mean (s) | median (s) | stddev (s) | max (s) |")
        lines.append("|---|---:|---:|---:|---:|---:|---:|")
        for lang in LANGUAGES:
            entry = bench["languages"][lang]
            stats = entry["stats"]
            lines.append(
                f"| {lang} | {entry['checksum']} | {format_seconds(stats['min_s'])} | "
                f"{format_seconds(stats['mean_s'])} | {format_seconds(stats['median_s'])} | "
                f"{format_seconds(stats['stddev_s'])} | {format_seconds(stats['max_s'])} |"
            )
        lines.append("")
        lines.append("| Relative to Apex (mean) | Value |")
        lines.append("|---|---:|")
        lines.append(f"| C speedup | {bench['speedup_vs_apex']['c']:.3f}x |")
        lines.append(f"| Rust speedup | {bench['speedup_vs_apex']['rust']:.3f}x |")
        lines.append("")

    return "\n".join(lines) + "\n"


def detect_llvm_prefix() -> str:
    from_env = os.environ.get("LLVM_SYS_211_PREFIX", "").strip()
    if from_env:
        return from_env

    llvm_config = shutil.which("llvm-config")
    if not llvm_config:
        raise RuntimeError(
            "LLVM prefix not found. Set LLVM_SYS_211_PREFIX or install llvm-config."
        )

    proc = run_cmd([llvm_config, "--prefix"], Path.cwd())
    if proc.returncode != 0:
        raise RuntimeError(
            f"Failed to detect LLVM prefix via llvm-config:\n{proc.stderr}"
        )
    prefix = proc.stdout.strip()
    if not prefix:
        raise RuntimeError("llvm-config --prefix returned empty output")
    return prefix


def main() -> int:
    parser = argparse.ArgumentParser(description="Run Apex vs C vs Rust benchmarks")
    parser.add_argument("--repeats", type=int, default=5, help="Timed runs per benchmark/language")
    parser.add_argument("--warmup", type=int, default=1, help="Warmup runs per benchmark/language")
    parser.add_argument(
        "--bench",
        choices=[b.name for b in BENCHMARKS],
        default=None,
        help="Run only one benchmark",
    )
    parser.add_argument(
        "--no-build",
        action="store_true",
        help="Skip building apex compiler with cargo build --release",
    )
    args = parser.parse_args()

    if args.repeats < 1:
        raise RuntimeError("--repeats must be >= 1")
    if args.warmup < 0:
        raise RuntimeError("--warmup must be >= 0")

    root = Path(__file__).resolve().parents[1]
    bench_dir = root / "benchmark"
    bin_dir = bench_dir / "bin"
    out_dir = bench_dir / "results"
    bin_dir.mkdir(parents=True, exist_ok=True)
    out_dir.mkdir(parents=True, exist_ok=True)

    ensure_tool("python3")
    ensure_tool("gcc")
    ensure_tool("rustc")
    ensure_tool("cargo")

    llvm_prefix = detect_llvm_prefix()
    build_env = {"LLVM_SYS_211_PREFIX": llvm_prefix}

    if not args.no_build:
        proc = run_cmd(["cargo", "build", "--release"], root, env=build_env)
        if proc.returncode != 0:
            raise RuntimeError(f"Failed to build Apex compiler:\n{proc.stderr}")

    selected = [b for b in BENCHMARKS if args.bench is None or b.name == args.bench]

    report = {
        "generated_at": time.strftime("%Y-%m-%d %H:%M:%S %Z"),
        "repeats": args.repeats,
        "warmup": args.warmup,
        "benchmarks": [],
    }

    for spec in selected:
        print(f"\n=== {spec.name} ===")
        binaries = {
            "apex": bin_dir / f"{spec.name}_apex",
            "c": bin_dir / f"{spec.name}_c",
            "rust": bin_dir / f"{spec.name}_rust",
        }

        compile_apex(root, spec.name, binaries["apex"], build_env)
        compile_c(root, spec.name, binaries["c"])
        compile_rust(root, spec.name, binaries["rust"])

        lang_data: Dict[str, Dict] = {}
        reference_checksum = None

        for lang in LANGUAGES:
            print(f"Running {lang}...")
            binary = binaries[lang]

            for _ in range(args.warmup):
                timed_run(binary, root)

            samples: List[float] = []
            checksums: List[int] = []
            for _ in range(args.repeats):
                elapsed, checksum = timed_run(binary, root)
                samples.append(elapsed)
                checksums.append(checksum)

            if len(set(checksums)) != 1:
                raise RuntimeError(f"Non-deterministic checksum in {lang}/{spec.name}: {checksums}")

            checksum = checksums[0]
            if reference_checksum is None:
                reference_checksum = checksum
            elif checksum != reference_checksum:
                raise RuntimeError(
                    f"Checksum mismatch for {spec.name}: {lang}={checksum}, expected={reference_checksum}"
                )

            stats = compute_stats(samples)
            lang_data[lang] = {
                "checksum": checksum,
                "samples_s": samples,
                "stats": stats,
            }

        apex_mean = lang_data["apex"]["stats"]["mean_s"]
        speedups = {
            "c": apex_mean / lang_data["c"]["stats"]["mean_s"],
            "rust": apex_mean / lang_data["rust"]["stats"]["mean_s"],
        }

        report["benchmarks"].append(
            {
                "name": spec.name,
                "description": spec.description,
                "languages": lang_data,
                "speedup_vs_apex": speedups,
            }
        )

    json_out = out_dir / "latest.json"
    md_out = out_dir / "latest.md"
    json_out.write_text(json.dumps(report, indent=2), encoding="utf-8")
    md_out.write_text(build_markdown(report), encoding="utf-8")

    print(f"\nWrote: {json_out}")
    print(f"Wrote: {md_out}")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except RuntimeError as exc:
        print(f"error: {exc}", file=sys.stderr)
        raise SystemExit(1)
