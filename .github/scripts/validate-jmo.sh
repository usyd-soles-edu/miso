#!/usr/bin/env bash
# validate-jmo.sh — full artifact validation for .github/workflows/build-jmo.yml.
#
# CI-only helper. Performs, in order:
#   1. existence and size-floor check on the built .jmo
#   2. structural checks against the `7z l` archive listing (fixed items plus
#      every dataset and analysis declared in jamovi/0000.yaml)
#   3. content checks on the metadata extracted with `7z e`
#      (exact `name:`/`version:` and `Package:`/`Version:` lines)
#   4. rename to the platform-suffixed artifact name and JMO_OUT export
#
# Manifest entries are derived by .github/scripts/manifest-entries.js using the
# js-yaml bundled inside the checksum-verified jmvtools tarball. The validator
# writes its output files only after the ENTIRE manifest validates, so a
# partially-valid manifest can never yield a partially-checked archive — bash
# cannot observe process-substitution producer failures, which is why the
# handoff is checked temp files rather than a streamed producer.
#
# Usage (all options required):
#   validate-jmo.sh --jmo <file> --manifest <file> --parser <js-yaml-index.js>
#                   --version <x.y.z> --module <name> --suffix <platform>
#                   --tempdir <dir> --github-env <file>
set -euo pipefail

fail() { echo "validate-jmo: $*" >&2; exit 1; }
usage() { fail "usage: validate-jmo.sh --jmo F --manifest F --parser F --version V --module M --suffix S --tempdir D --github-env F"; }

JMO='' MANIFEST='' PARSER='' VERSION='' MODULE='' SUFFIX='' TEMPDIR='' GITHUB_ENV_ARG=''
while [ $# -gt 0 ]; do
  case "$1" in
    --jmo) JMO="${2-}"; shift 2 ;;
    --manifest) MANIFEST="${2-}"; shift 2 ;;
    --parser) PARSER="${2-}"; shift 2 ;;
    --version) VERSION="${2-}"; shift 2 ;;
    --module) MODULE="${2-}"; shift 2 ;;
    --suffix) SUFFIX="${2-}"; shift 2 ;;
    --tempdir) TEMPDIR="${2-}"; shift 2 ;;
    --github-env) GITHUB_ENV_ARG="${2-}"; shift 2 ;;
    *) usage ;;
  esac
done
[ -n "${JMO}" ] || fail "missing required option --jmo"
[ -n "${MANIFEST}" ] || fail "missing required option --manifest"
[ -n "${PARSER}" ] || fail "missing required option --parser"
[ -n "${VERSION}" ] || fail "missing required option --version"
[ -n "${MODULE}" ] || fail "missing required option --module"
[ -n "${SUFFIX}" ] || fail "missing required option --suffix"
[ -n "${TEMPDIR}" ] || fail "missing required option --tempdir"
[ -n "${GITHUB_ENV_ARG}" ] || fail "missing required option --github-env"

mkdir -p "${TEMPDIR}"
test -f "${JMO}" || fail "${JMO} not found"
test -f "${MANIFEST}" || fail "${MANIFEST} not found"
test -f "${PARSER}" || fail "YAML parser not found at ${PARSER}"
command -v node >/dev/null 2>&1 || fail "node is required on PATH"
command -v 7z >/dev/null 2>&1 || fail "7z is required on PATH"

# --- 1. size floor: a real .jmo bundles compiled R packages -----------------
MIN_BYTES=1000000
SIZE=$(wc -c < "${JMO}" | tr -d ' ')
echo "${JMO} is ${SIZE} bytes"
[ "${SIZE}" -gt "${MIN_BYTES}" ] || fail "${JMO} is unexpectedly small (${SIZE} bytes); dependency bundling probably failed"

# --- 2. structural checks against the archive listing -----------------------
LISTING="${TEMPDIR}/jmo-listing.txt"
7z l "${JMO}" > "${LISTING}"
# 7-Zip on Windows lists entry paths with backslash separators (miso\x.yaml);
# normalise to forward slashes so the fixed-item checks below match either way
sed -i.bak 's|\\|/|g' "${LISTING}" && rm -f "${LISTING}.bak"
check_in_jmo() {
  grep -Fq "$1" "${LISTING}" || { echo "::error::missing from ${JMO}: $1" >&2; exit 1; }
}
# fixed items: module manifest, the miso R package, and the external
# dependencies jamovi does not bundle itself (ggplot2/R6/jmvcore ship with
# jamovi)
check_in_jmo "${MODULE}/jamovi.yaml"
check_in_jmo "${MODULE}/R/${MODULE}/DESCRIPTION"
check_in_jmo "${MODULE}/R/vegan/DESCRIPTION"
check_in_jmo "${MODULE}/R/permute/DESCRIPTION"

# declared datasets and analyses, derived by full-manifest YAML validation
ENTRIES_DIR="${TEMPDIR}/entries"
mkdir -p "${ENTRIES_DIR}"
ANALYSES_FILE="${ENTRIES_DIR}/analyses.txt"
DATASETS_FILE="${ENTRIES_DIR}/datasets.txt"
# command substitution DOES propagate the exit status under set -e; the node
# validator additionally writes its outputs only after the whole manifest has
# validated, so no partial list can be consumed even on failure paths
node "$(dirname "$0")/manifest-entries.js" "${PARSER}" "${MANIFEST}" "${ANALYSES_FILE}" "${DATASETS_FILE}"

dataset_count=0
checked_datasets=''
while IFS= read -r dataset_path; do
  [ -n "${dataset_path}" ] || continue
  dataset_count=$((dataset_count + 1))
  check_in_jmo "${MODULE}/data/${dataset_path}"
  checked_datasets="${checked_datasets} ${dataset_path}"
done < "${DATASETS_FILE}"
[ "${dataset_count}" -gt 0 ] || { echo "::error::${JMO}: no datasets were validated" >&2; exit 1; }

analysis_count=0
checked_analyses=''
while IFS= read -r analysis; do
  [ -n "${analysis}" ] || continue
  analysis_count=$((analysis_count + 1))
  check_in_jmo "${MODULE}/ui/${analysis}.js"
  checked_analyses="${checked_analyses} ${analysis}"
done < "${ANALYSES_FILE}"
[ "${analysis_count}" -gt 0 ] || { echo "::error::${JMO}: no analyses were validated" >&2; exit 1; }

echo "checked analyses (${analysis_count}):${checked_analyses}"
echo "checked datasets (${dataset_count}):${checked_datasets}"

# --- 3. content checks on extracted metadata --------------------------------
VALDIR="${TEMPDIR}/validate"
mkdir -p "${VALDIR}"
7z e -y -o"${VALDIR}" "${JMO}" "${MODULE}/jamovi.yaml" "${MODULE}/R/${MODULE}/DESCRIPTION" > /dev/null \
  || { echo "::error::failed to extract metadata from ${JMO}" >&2; exit 1; }
tr -d '\r' < "${VALDIR}/jamovi.yaml" > "${VALDIR}/jamovi.yaml.n"
tr -d '\r' < "${VALDIR}/DESCRIPTION" > "${VALDIR}/DESCRIPTION.n"
grep -Fxq "name: ${MODULE}" "${VALDIR}/jamovi.yaml.n" \
  || { echo "::error::extracted jamovi.yaml does not declare 'name: ${MODULE}' exactly" >&2; exit 1; }
grep -Fxq "version: ${VERSION}" "${VALDIR}/jamovi.yaml.n" \
  || { echo "::error::extracted jamovi.yaml version does not match ${VERSION}" >&2; exit 1; }
grep -Fxq "Package: ${MODULE}" "${VALDIR}/DESCRIPTION.n" \
  || { echo "::error::bundled ${MODULE} package does not declare 'Package: ${MODULE}' exactly" >&2; exit 1; }
grep -Fxq "Version: ${VERSION}" "${VALDIR}/DESCRIPTION.n" \
  || { echo "::error::bundled ${MODULE} package version does not match ${VERSION}" >&2; exit 1; }

# --- 4. platform-suffixed artifact name and JMO_OUT export ------------------
OUT_NAME="${MODULE}-${VERSION}-${SUFFIX}.jmo"
OUT_PATH="$(dirname "${JMO}")/${OUT_NAME}"
mv "${JMO}" "${OUT_PATH}"
echo "JMO_OUT=${OUT_NAME}" >> "${GITHUB_ENV_ARG}"
echo "validate-jmo: ok: ${OUT_NAME}"
