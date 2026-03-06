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
LINT_FILE="${TMP_DIR}/linty.apex"
TEST_FILE="${TMP_DIR}/sample_test.apex"
HEADER_FILE="${TMP_DIR}/sample.h"
BINDINGS_FILE="${TMP_DIR}/bindings.apex"
OUT_FILE="${TMP_DIR}/ugly_bin"
SHARED_PROJECT="${TMP_DIR}/shared_project"
STATIC_PROJECT="${TMP_DIR}/static_project"

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

cat <<'EOF_LINT' > "${LINT_FILE}"
import std.string.*;
import std.io.*;
import std.io.*;

function main(): None {
    println("ok");
    return None;
}
EOF_LINT

if "${COMPILER}" fmt --check "${UGLY_FILE}" >/dev/null 2>&1; then
  echo "fmt --check unexpectedly passed on unformatted source" >&2
  exit 1
fi

"${COMPILER}" lint "${LINT_FILE}" >/dev/null 2>&1 && {
  echo "lint unexpectedly passed on linty source" >&2
  exit 1
}
"${COMPILER}" fix "${LINT_FILE}" >/dev/null
"${COMPILER}" lint "${LINT_FILE}" >/dev/null
"${COMPILER}" fmt "${UGLY_FILE}" >/dev/null
"${COMPILER}" fmt --check "${UGLY_FILE}" >/dev/null
"${COMPILER}" lex "${UGLY_FILE}" >/dev/null
"${COMPILER}" parse "${UGLY_FILE}" >/dev/null
"${COMPILER}" compile "${UGLY_FILE}" -o "${OUT_FILE}" >/dev/null
"${OUT_FILE}" >/dev/null
"${COMPILER}" run "${UGLY_FILE}" >/dev/null
"${COMPILER}" bench "${UGLY_FILE}" --iterations 2 >/dev/null
"${COMPILER}" profile "${UGLY_FILE}" >/dev/null

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

"${COMPILER}" new shared_project --path "${SHARED_PROJECT}" >/dev/null
python3 - <<'PY' "${SHARED_PROJECT}/apex.toml"
from pathlib import Path
import sys

path = Path(sys.argv[1])
content = path.read_text()
content += '\noutput_kind = "shared"\n'
path.write_text(content)
PY
pushd "${SHARED_PROJECT}" >/dev/null
"${COMPILER}" build >/dev/null
test -f "${SHARED_PROJECT}/shared_project"
popd >/dev/null

"${COMPILER}" new static_project --path "${STATIC_PROJECT}" >/dev/null
python3 - <<'PY' "${STATIC_PROJECT}/apex.toml"
from pathlib import Path
import sys

path = Path(sys.argv[1])
content = path.read_text()
content += '\noutput_kind = "static"\n'
path.write_text(content)
PY
pushd "${STATIC_PROJECT}" >/dev/null
"${COMPILER}" build >/dev/null
test -f "${STATIC_PROJECT}/static_project"
popd >/dev/null
