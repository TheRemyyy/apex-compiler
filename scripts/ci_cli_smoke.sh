#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
COMPILER="${REPO_ROOT}/target/release/apex-compiler"

cargo build --release >/dev/null

TMP_DIR="$(mktemp -d)"
trap 'rm -rf "${TMP_DIR}"' EXIT

PROJECT_DIR="${TMP_DIR}/sample_project"
UGLY_FILE="${TMP_DIR}/ugly.apex"
TEST_FILE="${TMP_DIR}/sample_test.apex"
HEADER_FILE="${TMP_DIR}/sample.h"
BINDINGS_FILE="${TMP_DIR}/bindings.apex"
OUT_FILE="${TMP_DIR}/ugly_bin"

"${COMPILER}" new sample_project --path "${PROJECT_DIR}" >/dev/null

pushd "${PROJECT_DIR}" >/dev/null
"${COMPILER}" info >/dev/null
"${COMPILER}" check >/dev/null
"${COMPILER}" run >/dev/null
popd >/dev/null

cat <<'EOF_FILE' > "${UGLY_FILE}"
// lead comment
import std.io.*;
function main(): None {println("ok");return None;}
EOF_FILE

if "${COMPILER}" fmt --check "${UGLY_FILE}" >/dev/null 2>&1; then
  echo "fmt --check unexpectedly passed on unformatted source" >&2
  exit 1
fi

"${COMPILER}" fmt "${UGLY_FILE}" >/dev/null
"${COMPILER}" fmt --check "${UGLY_FILE}" >/dev/null
"${COMPILER}" lex "${UGLY_FILE}" >/dev/null
"${COMPILER}" parse "${UGLY_FILE}" >/dev/null
"${COMPILER}" compile "${UGLY_FILE}" -o "${OUT_FILE}" >/dev/null
"${OUT_FILE}" >/dev/null
"${COMPILER}" run "${UGLY_FILE}" >/dev/null

cat <<'EOF_TEST' > "${TEST_FILE}"
@Test
function sampleTest(): None {
    assert_eq(1, 1);
    return None;
}
EOF_TEST

"${COMPILER}" test --list --path "${TEST_FILE}" >/dev/null
"${COMPILER}" test --path "${TEST_FILE}" >/dev/null

cat <<'EOF_HEADER' > "${HEADER_FILE}"
int add(int a, int b);
int puts(const char* msg);
EOF_HEADER

"${COMPILER}" bindgen "${HEADER_FILE}" --output "${BINDINGS_FILE}" >/dev/null
grep -q 'extern(c) function add(a: Integer, b: Integer): Integer;' "${BINDINGS_FILE}"
grep -q 'extern(c) function puts(msg: String): Integer;' "${BINDINGS_FILE}"
