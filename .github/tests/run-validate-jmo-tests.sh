#!/usr/bin/env bash
# run-validate-jmo-tests.sh — adversarial regression harness for
# .github/scripts/validate-jmo.sh and .github/scripts/manifest-entries.js.
#
# Runs the ACTUAL production helper (strict mode) against fixture archives.
#
# LIMITATIONS (read before trusting results):
#   * 7z is faked: a stub on PATH serves a fixture listing for `7z l` and
#     copies fixture files for `7z e`. The real 7z binary's listing format
#     and extraction behaviour are exercised only on a GitHub runner. The
#     fake faithfully models the flag surface the helper uses (l; e -y -o).
#   * The bundled js-yaml parser is taken from the jmvtools tarball whose
#     SHA-256 is asserted against the workflow pin first (no new dependency
#     is installed). Provide the tarball via JMVTOOLS_TARBALL (default
#     /tmp/pr3-regression/jmvtools.tar.gz); download it with the workflow's
#     pinned URL if absent.
#   * If 7z and a real miso .jmo are both available (MISO_REAL_JMO points at
#     it), an additional real-archive test runs; otherwise it is skipped and
#     disclosed below.
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WF="$ROOT/.github/workflows/build-jmo.yml"
HELPER="$ROOT/.github/scripts/validate-jmo.sh"
WORK="$(mktemp -d "${TMPDIR:-/tmp}/pr3-validate-tests.XXXXXX")"
trap 'rm -rf "$WORK"' EXIT

pass=0; fail=0; skip=0
ok()  { printf 'PASS %s\n' "$1"; pass=$((pass+1)); }
bad() { printf 'FAIL %s\n' "$1"; fail=$((fail+1)); }
skipd() { printf 'SKIP %s\n' "$1"; skip=$((skip+1)); }

# --- fixtures -----------------------------------------------------------------
TARBALL="${JMVTOOLS_TARBALL:-/tmp/pr3-regression/jmvtools.tar.gz}"
PIN_JMVTOOLS=$(awk -F"'" '/^[[:space:]]*JMVTOOLS_SHA256:/ {print $2}' "$WF")
# the jamovi pin now lives in the matrix rows of the consolidated workflow;
# extract the win-x64 row's hash (T15d verifies the Windows zip against it)
PIN_JAMOVI=$(awk '/platform: win-x64/,/jamovi_sha256:/' "$WF" | awk -F"'" '/jamovi_sha256:/ {print $2}')

[ -f "$TARBALL" ] || { echo "FATAL: jmvtools tarball not found at $TARBALL (set JMVTOOLS_TARBALL)"; exit 2; }
ACTUAL_JMVTOOLS=$(sha256sum "$TARBALL" | awk '{print $1}')
if [ "$ACTUAL_JMVTOOLS" != "$PIN_JMVTOOLS" ]; then
  # hard gate: a mismatched tarball must never be extracted or loaded, so
  # abort the whole harness here rather than counting a failure and
  # continuing into `tar xzf` with an unverified parser
  printf 'FAIL T00a jmvtools tarball hash mismatch - refusing to extract or load the unverified parser\n'
  printf '      expected (workflow pin): %s\n      actual:                   %s\n' "$PIN_JMVTOOLS" "$ACTUAL_JMVTOOLS"
  exit 1
fi
ok "T00a tarball hash equals workflow pin (parser provenance)"

PARSER="$WORK/jmvtools/inst/node_modules/jamovi-compiler/node_modules/js-yaml/index.js"
tar xzf "$TARBALL" -C "$WORK" "jmvtools/inst/node_modules/jamovi-compiler/node_modules/js-yaml"

# fake 7z: `l` cats <stem>.listing; `e -y -oDIR <jmo> <members...>` copies
# <stem>.extracted/<member> to DIR/<basename>. Fails like real 7z when a
# requested member is absent from the fixture.
mkdir -p "$WORK/bin"
cat > "$WORK/bin/7z" <<'STUB'
#!/usr/bin/env bash
set -euo pipefail
case "$1" in
  l) cat "${2}.listing" ;;
  e) shift; out=""; pos=()
     while [ $# -gt 0 ]; do
       case "$1" in
         -y) ;;
         -o*) out="${1#-o}" ;;
         *) pos+=("$1") ;;
       esac; shift
     done
     archive="${pos[0]}"
     for m in "${pos[@]:1}"; do cp "${archive}.extracted/${m}" "${out}/$(basename "${m}")"; done ;;
  *) echo "fake7z: unsupported invocation: $*" >&2; exit 9 ;;
esac
STUB
chmod +x "$WORK/bin/7z"
export PATH="$WORK/bin:$PATH"

ANALYSES7="permanova anosim permdisp nmds pcoa cluster simper"
FIX_ITEMS="miso/jamovi.yaml miso/R/miso/DESCRIPTION miso/R/vegan/DESCRIPTION miso/R/permute/DESCRIPTION"

# listing-item files (paths may contain spaces, so items travel by file)
UI_FILE="$WORK/ui-items.txt"
for a in $ANALYSES7; do echo "miso/ui/${a}.js"; done > "$UI_FILE"
GOOD_FILE="$WORK/good-items.txt"
{ cat "$UI_FILE"; echo "miso/data/dune_meadows.csv"; } > "$GOOD_FILE"

# mkfix <dir> <manifest> <listing-items-file>: builds a >1MB fake jmo whose
# fake-7z listing is the fixed items plus the file's lines, and whose
# extracted metadata fixtures are well-formed
mkfix() {
  local dir="$1" manifest="$2" items="$3"
  mkdir -p "$dir/miso_1.0.0.jmo.extracted/miso/R/miso"
  dd if=/dev/zero of="$dir/miso_1.0.0.jmo" bs=1024 count=1200 status=none
  { echo "$FIX_ITEMS"; cat "$items"; } > "$dir/miso_1.0.0.jmo.listing"
  printf 'name: miso\nversion: 1.0.0\n' > "$dir/miso_1.0.0.jmo.extracted/miso/jamovi.yaml"
  printf 'Package: miso\nVersion: 1.0.0\n' > "$dir/miso_1.0.0.jmo.extracted/miso/R/miso/DESCRIPTION"
  cp "$manifest" "$dir/manifest.yaml"
}

run_helper() { # <fixdir> [extra args...]: runs the real helper with fake 7z
  local dir="$1"; shift
  ( cd "$dir" && bash "$HELPER" \
      --jmo "miso_1.0.0.jmo" \
      --manifest "$dir/manifest.yaml" \
      --parser "$PARSER" \
      --version 1.0.0 \
      --module miso \
      --suffix win-x64 \
      --tempdir "$dir/tmp" \
      --github-env "$dir/github-env" "$@" )
}

expect_err() { # <label> <exact-substring> <fixdir>
  local label="$1" want="$2" dir="$3" out rc=0
  out=$(run_helper "$dir" 2>&1) || rc=$?
  if [ "$rc" -ne 0 ] && printf '%s' "$out" | grep -Fq "$want"; then
    ok "$label"
  else
    bad "$label (rc=$rc; wanted substring: $want; got: $(printf '%s' "$out" | tail -2 | tr '\n' '|'))"
  fi
}

# === T00b: provenance failure must abort BEFORE extraction/parser load =======
# Builds a tampered copy of the (already verified) tarball whose js-yaml
# entrypoint writes a sentinel file if it is ever LOADED, then re-runs this
# harness against it. The guard must: exit nonzero, print the refusal, never
# reach any test or the RESULT line, and never load the tampered parser.
# Skipped in the child invocation to bound recursion.
if [ -z "${PR3_HARNESS_CHILD:-}" ]; then
  TAMPER="$WORK/tamper"; mkdir -p "$TAMPER"
  tar xzf "$TARBALL" -C "$TAMPER"
  printf '%s\n' \
    "require('fs').writeFileSync(process.env.PR3_SENTINEL_PATH, 'tampered parser was loaded');" \
    "module.exports = { load: () => ({ analyses: [], datasets: [] }) };" \
    > "$TAMPER/jmvtools/inst/node_modules/jamovi-compiler/node_modules/js-yaml/index.js"
  tar czf "$WORK/tampered.tgz" -C "$TAMPER" jmvtools
  SENTINEL="$WORK/parser-load-sentinel"; rm -f "$SENTINEL"
  rc=0; child_out=$(PR3_HARNESS_CHILD=1 PR3_SENTINEL_PATH="$SENTINEL" JMVTOOLS_TARBALL="$WORK/tampered.tgz" bash "$0" 2>&1) || rc=$?
  if [ "$rc" -ne 0 ] \
     && printf '%s' "$child_out" | grep -Fq "refusing to extract or load the unverified parser" \
     && ! printf '%s' "$child_out" | grep -q "^RESULT:" \
     && ! printf '%s' "$child_out" | grep -q "^PASS T0 " \
     && [ ! -f "$SENTINEL" ]; then
    ok "T00b mismatched tarball aborts before extraction; parser never loaded (sentinel absent, no tests ran)"
  else
    bad "T00b provenance guard leaked (rc=$rc; sentinel=$([ -f "$SENTINEL" ] && echo present || echo absent); child tail: $(printf '%s' "$child_out" | tail -1))"
  fi
fi

# === T0: process-substitution hazard (documents the design constraint) =======
hazard=$(bash -c 'set -euo pipefail; out=$(cat <(node -e "process.stdout.write(\"first-entry\"); process.exit(1)")); echo "SURVIVED:${out}"' 2>/dev/null || true)
[ "$hazard" = "SURVIVED:first-entry" ] \
  && ok "T0 set -e does NOT abort on process-substitution producer failure (why validate-jmo.sh uses checked temp files)" \
  || bad "T0 hazard demo unexpectedly failed"

# === T1: all-present success, full production path ============================
D="$WORK/t1"; mkfix "$D" "$ROOT/jamovi/0000.yaml" "$GOOD_FILE"
out=$(run_helper "$D")
[ -f "$D/miso-1.0.0-win-x64.jmo" ] && [ ! -f "$D/miso_1.0.0.jmo" ] && ok "T1a success: artifact renamed to platform-suffixed name" || bad "T1a rename failed"
[ "$(cat "$D/github-env")" = "JMO_OUT=miso-1.0.0-win-x64.jmo" ] && ok "T1b success: JMO_OUT exported exactly" || bad "T1b env file: $(cat "$D/github-env" 2>/dev/null)"
printf '%s' "$out" | grep -Fxq "checked analyses (7): $ANALYSES7" && ok "T1c success: all seven analyses visited, manifest order" || bad "T1c enumeration missing: $out"
printf '%s' "$out" | grep -Fxq "checked datasets (1): dune_meadows.csv" && ok "T1d success: dataset visited" || bad "T1d dataset enumeration missing"

# === T2: absent SECOND / LAST entries produce exact errors ====================
for case in "second anosim" "last simper"; do
  set -- $case; label=$1; victim=$2
  grep -v "ui/${victim}.js" "$GOOD_FILE" > "$WORK/items-$label.txt"
  D="$WORK/t2-$label"; mkfix "$D" "$ROOT/jamovi/0000.yaml" "$WORK/items-$label.txt"
  expect_err "T2-$label absent $label analysis fails with exact error" "::error::missing from miso_1.0.0.jmo: miso/ui/${victim}.js" "$D"
done

# absent only/last dataset
D="$WORK/t2-dslast"; mkfix "$D" "$ROOT/jamovi/0000.yaml" "$UI_FILE"
expect_err "T2-dslast absent (only/last) dataset fails with exact error" "::error::missing from miso_1.0.0.jmo: miso/data/dune_meadows.csv" "$D"

# two datasets with extra.csv appended as the SECOND entry, after the real
# first (inserted at the end of the datasets block, before the next
# top-level key) - inserted-first would mask second-position behaviour
sed '/^\.\.\.$/d' "$ROOT/jamovi/0000.yaml" | awk -v e='  - name: extra\n    path: extra.csv' '
  /^datasets:/ { inds=1; print; next }
  inds && /^[^[:space:]]/ { print e; inds=0 }
  { print }' > "$WORK/manifest-2ds.yaml"
# ordered-fixture assertion via the verified parser itself
node "$(dirname "$HELPER")/manifest-entries.js" "$PARSER" "$WORK/manifest-2ds.yaml" "$WORK/2ds-a.txt" "$WORK/2ds-d.txt"
DS_ORDER=$(tr '\n' ' ' < "$WORK/2ds-d.txt" | sed 's/ $//')
[ "$DS_ORDER" = "dune_meadows.csv extra.csv" ] && ok "T2-dssecond-order fixture order is [dune_meadows.csv, extra.csv]" || bad "T2-dssecond-order fixture order wrong: '$DS_ORDER'"
# first present, second absent: exact diagnostic for the SECOND entry
D="$WORK/t2-dssecond"; mkfix "$D" "$WORK/manifest-2ds.yaml" "$GOOD_FILE"
expect_err "T2-dssecond absent second dataset fails with exact error" "::error::missing from miso_1.0.0.jmo: miso/data/extra.csv" "$D"
# positive: both present, helper enumerates both in order
{ cat "$GOOD_FILE"; echo "miso/data/extra.csv"; } > "$WORK/items-2ds.txt"
D="$WORK/t2-dssecond-ok"; mkfix "$D" "$WORK/manifest-2ds.yaml" "$WORK/items-2ds.txt"
out=$(run_helper "$D")
printf '%s' "$out" | grep -Fxq "checked datasets (2): dune_meadows.csv extra.csv" && ok "T2-dssecond-ok both present: helper checks both datasets in manifest order" || bad "T2-dssecond-ok enumeration wrong: $out"

# === T3: seven analyses all visited (visitation proof via enumeration) ========
D="$WORK/t3"; mkfix "$D" "$ROOT/jamovi/0000.yaml" "$GOOD_FILE"
out=$(run_helper "$D")
printf '%s' "$out" | grep -Fxq "checked analyses (7): $ANALYSES7" \
  && ok "T3 enumeration proves each of the 7 analyses reached its archive check (loop-built list)" \
  || bad "T3 enumeration wrong: $out"

# === T4: key ordering is irrelevant under the real parser =====================
cat > "$WORK/manifest-order.yaml" <<'YAML'
title: Multivariate Inference, Similarity and Ordination (MISO)
name: miso
version: 1.0.0
datasets:
  - path: dune_meadows.csv
    name: dune_meadows
analyses:
  - name: permanova
    title: PERMANOVA
  - title: ANOSIM
    name: anosim
  - name: permdisp
    title: PERMDISP
  - title: nMDS
    name: nmds
  - name: pcoa
    title: PCoA
  - title: Cluster analysis
    name: cluster
  - name: simper
    title: SIMPER
YAML
D="$WORK/t4"; mkfix "$D" "$WORK/manifest-order.yaml" "$GOOD_FILE"
out=$(run_helper "$D")
printf '%s' "$out" | grep -Fxq "checked analyses (7): $ANALYSES7" && ok "T4 mixed key order (name-first and name-last) parses; order preserved" || bad "T4 ordering case failed: $out"

# === T5: quoted path containing spaces ========================================
cat > "$WORK/manifest-spaced.yaml" <<'YAML'
title: t
name: miso
version: 1.0.0
datasets:
  - name: teaching
    path: "teaching data.csv"
analyses:
  - name: permanova
YAML
printf 'miso/ui/permanova.js\nmiso/data/teaching data.csv\n' > "$WORK/items-spaced.txt"
D="$WORK/t5-ok"; mkfix "$D" "$WORK/manifest-spaced.yaml" "$WORK/items-spaced.txt"
run_helper "$D" >/dev/null && ok "T5a quoted spaced path checks the exact unsplit archive path" || bad "T5a spaced-path success case failed"
printf 'miso/ui/permanova.js\n' > "$WORK/items-spaced-miss.txt"
D="$WORK/t5-miss"; mkfix "$D" "$WORK/manifest-spaced.yaml" "$WORK/items-spaced-miss.txt"
expect_err "T5b absent spaced path fails with the exact unsplit path" "::error::missing from miso_1.0.0.jmo: miso/data/teaching data.csv" "$D"

# === T6-T8: field validity, through the full helper ===========================
mkvar() { # inserts entry lines at the END of the analyses block, immediately
# before the next top-level key (the manifest has keys after analyses:, so
# EOF appends are invalid YAML; end-of-block insertion makes the new entry
# the LATER entry, exercising late-position failure paths)
  sed '/^\.\.\.$/d' "$ROOT/jamovi/0000.yaml" | awk -v e="$2" '
    /^analyses:/ { inan=1; print; next }
    inan && /^[^[:space:]]/ { print e; inan=0 }
    { print }' > "$WORK/$1"; }
mkvar manifest-noname.yaml '  - title: Ghost'
D="$WORK/t6"; mkfix "$D" "$WORK/manifest-noname.yaml" "$GOOD_FILE"
expect_err "T6 missing name field fails with exact indexed error" "manifest-entries: analyses[7]: 'name' is required" "$D"

mkvar manifest-numname.yaml '  - name: 42'
D="$WORK/t7"; mkfix "$D" "$WORK/manifest-numname.yaml" "$GOOD_FILE"
expect_err "T7 non-string name fails with exact error" "manifest-entries: analyses[7]: 'name' must be a non-empty string" "$D"

printf '  - name: rda\n  - name: rda\n' >> /dev/null # (two identical later entries)
sed '/^\.\.\.$/d' "$ROOT/jamovi/0000.yaml" | awk -v e='  - name: rda\n  - name: rda' '
    /^analyses:/ { inan=1; print; next }
    inan && /^[^[:space:]]/ { print e; inan=0 }
    { print }' > "$WORK/manifest-dup.yaml"
D="$WORK/t8"; mkfix "$D" "$WORK/manifest-dup.yaml" "$GOOD_FILE" miso/ui/rda.js
expect_err "T8 later duplicate name fails with exact indexed error" "manifest-entries: analyses[8]: duplicate analyses name: rda" "$D"

sed '/^\.\.\.$/d' "$ROOT/jamovi/0000.yaml" | awk -v e='  - name: ghost\n    name: ghast' '
    /^analyses:/ { inan=1; print; next }
    inan && /^[^[:space:]]/ { print e; inan=0 }
    { print }' > "$WORK/manifest-dupkey.yaml"
D="$WORK/t8b"; mkfix "$D" "$WORK/manifest-dupkey.yaml" "$GOOD_FILE"
expect_err "T8b duplicate key within one entry fails with js-yaml duplicate-key error" "duplicated mapping key" "$D"

# === T9: malformed YAML after valid entries — no partial acceptance ==========
sed '/^\.\.\.$/d' "$ROOT/jamovi/0000.yaml" | awk -v e='  - name: rda\n   bad: [' '
    /^analyses:/ { inan=1; print; next }
    inan && /^[^[:space:]]/ { print e; inan=0 }
    { print }' > "$WORK/manifest-malformed.yaml"
D="$WORK/t9"; mkfix "$D" "$WORK/manifest-malformed.yaml" "$GOOD_FILE"
rc=0; out=$(run_helper "$D" 2>&1) || rc=$?
[ "$rc" -ne 0 ] && printf '%s' "$out" | grep -Fq "bad indentation" && ok "T9a malformed-after-valid entries fails with the actual parse error" || bad "T9a (rc=$rc): $(printf '%s' "$out" | tail -1)"
[ ! -f "$D/tmp/entries/analyses.txt" ] && ok "T9b no entries file written on failure (no partial acceptance)" || bad "T9b entries file exists despite failure"
# decisive partial-acceptance proof: listing is missing permanova.js, yet the
# failure is the YAML error — proving full validation precedes any archive check
grep -v "ui/permanova.js" "$GOOD_FILE" > "$WORK/items-noperm.txt"
D="$WORK/t9c"; mkfix "$D" "$WORK/manifest-malformed.yaml" "$WORK/items-noperm.txt"
expect_err "T9c validation completes before any archive check (missing permanova NOT the error)" "bad indentation" "$D"

# === T10-T11: structural manifest failures =====================================
python3 - "$ROOT/jamovi/0000.yaml" "$WORK/manifest-nods.yaml" <<'PY'
import sys, re
src, dst = sys.argv[1], sys.argv[2]
text = open(src).read()
text = re.sub(r'^datasets:.*?(?=^analyses:)', '', text, flags=re.S | re.M)
open(dst, 'w').write(text)
PY
D="$WORK/t10"; mkfix "$D" "$WORK/manifest-nods.yaml" "$GOOD_FILE"
expect_err "T10 missing datasets block fails with exact error" "manifest-entries: datasets: required, must be a non-empty list" "$D"

printf -- '- a\n- b\n' > "$WORK/manifest-rootlist.yaml"
D="$WORK/t11"; mkfix "$D" "$WORK/manifest-rootlist.yaml" "$GOOD_FILE"
expect_err "T11 non-mapping root fails with exact error" "manifest-entries: manifest root is not a mapping (got a list)" "$D"

# === T12: strict mode — missing required option ================================
out=$(bash "$HELPER" --manifest x --parser y --version 1 --module m --suffix s --tempdir "$WORK" --github-env "$WORK/env" 2>&1) || rc=$? || true
rc=0; out=$(bash "$HELPER" --manifest x --parser y --version 1 --module m --suffix s --tempdir "$WORK" --github-env "$WORK/env" 2>&1) || rc=$?
[ "$rc" -ne 0 ] && printf '%s' "$out" | grep -Fq "validate-jmo: missing required option --jmo" && ok "T12 strict mode: unset required input fails with exact message" || bad "T12 rc=$rc out=$out"

# === T12b: Windows 7z backslash listing normalisation ==========================
# Real 7z on Windows emits entry paths with backslash separators; the helper
# must normalise them before the forward-slash fixed-item checks.
D="$WORK/t12b"; mkfix "$D" "$ROOT/jamovi/0000.yaml" "$GOOD_FILE"
sed -i.bak 's|/|\\|g' "$D/miso_1.0.0.jmo.listing" && rm -f "$D/miso_1.0.0.jmo.listing.bak"
out=$(run_helper "$D") && rc=0 || rc=$?
if [ "$rc" -eq 0 ] && [ -f "$D/miso-1.0.0-win-x64.jmo" ]; then
  ok "T12b backslash (Windows 7z) listing normalised; helper succeeds"
else
  bad "T12b backslash listing failed (rc=$rc; tail: $(printf '%s' "$out" | tail -2 | tr '\n' '|'))"
fi

# === T13: size floor ============================================================
D="$WORK/t13"; mkfix "$D" "$ROOT/jamovi/0000.yaml" "$GOOD_FILE"
dd if=/dev/zero of="$D/miso_1.0.0.jmo" bs=1024 count=1 status=none
expect_err "T13 undersized artifact fails with exact message" "validate-jmo: miso_1.0.0.jmo is unexpectedly small (1024 bytes); dependency bundling probably failed" "$D"

# === T14: metadata mismatches ===================================================
D="$WORK/t14a"; mkfix "$D" "$ROOT/jamovi/0000.yaml" "$GOOD_FILE"
printf 'name: miso\nversion: 9.9.9\n' > "$D/miso_1.0.0.jmo.extracted/miso/jamovi.yaml"
expect_err "T14a wrong compiled version fails exactly" "::error::extracted jamovi.yaml version does not match 1.0.0" "$D"
D="$WORK/t14b"; mkfix "$D" "$ROOT/jamovi/0000.yaml" "$GOOD_FILE"
printf 'Package: other\nVersion: 1.0.0\n' > "$D/miso_1.0.0.jmo.extracted/miso/R/miso/DESCRIPTION"
expect_err "T14b wrong package name fails exactly" "::error::bundled miso package does not declare 'Package: miso' exactly" "$D"

# === T15: sha256 verification, exact workflow command form =====================
printf 'intact content\n' > "$WORK/hashgood.txt"
KH=$(sha256sum "$WORK/hashgood.txt" | awk '{print $1}')
echo "$KH *$WORK/hashgood.txt" | sha256sum -c - >/dev/null 2>&1 && ok "T15a intact file passes the workflow's exact 'echo PIN *file | sha256sum -c -' form" || bad "T15a"
printf 'tampered\n' > "$WORK/hashbad.txt"
echo "$KH *$WORK/hashbad.txt" | sha256sum -c - >/dev/null 2>&1 && bad "T15b tampered file wrongly accepted" || ok "T15b tampered copy rejected by the same command"
echo "$PIN_JMVTOOLS *$TARBALL" | sha256sum -c - >/dev/null 2>&1 && ok "T15c jmvtools tarball verifies against workflow pin via actual command" || bad "T15c"
JAMOVI_ZIP="${JAMOVI_ZIP:-/tmp/pr3-regression/jamovi-win-x64.zip}"
if [ -f "$JAMOVI_ZIP" ]; then
  echo "$PIN_JAMOVI *$JAMOVI_ZIP" | sha256sum -c - >/dev/null 2>&1 && ok "T15d jamovi zip verifies against workflow pin via actual command" || bad "T15d"
else
  skipd "T15d jamovi zip not present locally (${JAMOVI_ZIP}); pin equality was proven in the prior round and re-provable on the runner"
fi

# === T16: real 7z + real .jmo (when available) =================================
if PATH="${PATH#"$WORK/bin:"}" command -v 7z >/dev/null 2>&1 && [ -n "${MISO_REAL_JMO:-}" ] && [ -f "${MISO_REAL_JMO:-}" ]; then
  D="$WORK/t16"; mkdir -p "$D"
  cp "$MISO_REAL_JMO" "$D/miso_1.0.0.jmo"
  cp "$ROOT/jamovi/0000.yaml" "$D/manifest.yaml"
  out=$( cd "$D" && PATH="${PATH#"$WORK/bin:"}" bash "$HELPER" --jmo miso_1.0.0.jmo --manifest "$D/manifest.yaml" --parser "$PARSER" --version 1.0.0 --module miso --suffix win-x64 --tempdir "$D/tmp" --github-env "$D/github-env" 2>&1 )
  printf '%s' "$out" | grep -Fxq "checked analyses (7): $ANALYSES7" && ok "T16 real 7z + real .jmo validate end-to-end" || bad "T16: $out"
else
  skipd "T16 real-archive test: 7z binary and/or MISO_REAL_JMO not available on this host (fake-7z fixtures above cover helper logic; real binary behaviour is runner-only)"
fi

printf '\nRESULT: pass=%d fail=%d skip=%d\n' "$pass" "$fail" "$skip"
[ "$fail" -eq 0 ]
