# Apex Production Benchmark Suite

This directory contains a structured benchmark suite that compares Apex against C and Rust on the same workloads.

## Goals

- Use identical algorithms across languages.
- Validate correctness (same checksum/result for each workload).
- Measure repeatable wall-clock runtime.
- Export machine-readable and human-readable reports.

## Workloads

- `sum_loop`: integer-heavy pseudo-random accumulation loop.
- `prime_count`: sieve-based prime counting.
- `matrix_mul`: dense integer matrix multiplication (flattened arrays).

## Directory Layout

```text
benchmark/
  apex/         # Apex implementations
  c/            # C implementations
  rust/         # Rust implementations
  bin/          # Compiled binaries (generated)
  results/      # Benchmark reports (generated)
  run.py        # Unified benchmark runner
```

## Requirements

- Linux/macOS shell environment.
- `python3`
- `clang` or `gcc` (for C; runner prefers `clang` when available)
- `rustc` (for Rust)
- Apex compiler binary available at `target/release/apex-compiler`

If the Apex compiler binary is missing, the runner will build it via:

```bash
cargo build --release
```

`LLVM_SYS_211_PREFIX` is auto-detected via `llvm-config --prefix` when available.
You can override it explicitly if needed:

```bash
LLVM_SYS_211_PREFIX=/usr/lib64/llvm21 python3 benchmark/run.py
```

## Run

From repository root:

```bash
python3 benchmark/run.py
```

Useful options:

```bash
python3 benchmark/run.py --repeats 7 --warmup 1
python3 benchmark/run.py --bench prime_count
python3 benchmark/run.py --no-build
python3 benchmark/run.py --apex-opt-level 3
python3 benchmark/run.py --apex-target x86_64-unknown-linux-gnu
```

## Output

- JSON report: `benchmark/results/latest.json`
- Markdown report: `benchmark/results/latest.md`

Both include:

- per-language timings
- summary stats (min/mean/median/stddev/max)
- speedups relative to Apex
- correctness checksums

## Notes

- This suite is CPU-focused and deterministic.
- Keep machine load stable for fair comparisons.
- For publication-quality results, pin CPU governor and run multiple sessions.
