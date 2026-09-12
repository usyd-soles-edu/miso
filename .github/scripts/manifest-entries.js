#!/usr/bin/env node
/*
 * manifest-entries.js — parse and validate jamovi/0000.yaml with a real YAML
 * parser, then emit the validated entries as line-based lists.
 *
 * CI-only helper for .github/workflows/build-jmo.yml. The parser is the
 * js-yaml bundled inside the checksum-verified jmvtools tarball that the
 * workflow already downloads and verifies — no new dependency is installed.
 *
 * Output contract (important): the output files are written ONLY after the
 * entire manifest has parsed and validated. A manifest that is valid up to
 * its tenth entry and then malformed produces NO output files at all, so the
 * consuming shell can never accept a partial entry list. This is deliberate:
 * bash `set -e`/`pipefail` cannot observe the exit status of a process
 * substitution producer, so a streaming producer that emitted a valid entry
 * and then failed could let a `while read < <(producer)` loop falsely
 * succeed. Validate-everything-then-write defeats that class of bug.
 *
 * Usage:
 *   node manifest-entries.js <js-yaml-index.js> <manifest.yaml> <analyses-out> <datasets-out>
 *
 * On success: writes one analysis name per line to <analyses-out>, one
 * dataset path per line to <datasets-out> (manifest order preserved), and
 * prints a one-line summary to stdout. On any failure: prints
 * `manifest-entries: <reason>` to stderr, exits 1, and writes nothing.
 */
'use strict';

const fs = require('fs');
const path = require('path');

function fail(reason) {
  process.stderr.write(`manifest-entries: ${reason}\n`);
  process.exit(1);
}

const argv = process.argv.slice(2);
if (argv.length !== 4) {
  fail(`usage: manifest-entries.js <js-yaml-index.js> <manifest.yaml> <analyses-out> <datasets-out>`);
}
const [parserArg, manifestArg, analysesOutArg, datasetsOutArg] = argv;

let loadYaml;
try {
  loadYaml = require(path.resolve(parserArg)).load;
} catch (e) {
  fail(`cannot load YAML parser from ${parserArg}: ${e.message}`);
}
if (typeof loadYaml !== 'function') {
  fail(`parser at ${parserArg} does not expose a load() function`);
}

let text;
try {
  text = fs.readFileSync(path.resolve(manifestArg), 'utf8');
} catch (e) {
  fail(`cannot read manifest ${manifestArg}: ${e.message}`);
}

let manifest;
try {
  manifest = loadYaml(text);
} catch (e) {
  fail(`YAML parse error in ${manifestArg}: ${String(e.message).split('\n')[0]}`);
}

if (manifest === null || typeof manifest !== 'object' || Array.isArray(manifest)) {
  fail(`manifest root is not a mapping (got ${manifest === null ? 'null' : Array.isArray(manifest) ? 'a list' : typeof manifest})`);
}

function checkEntryList(block, label, key) {
  if (!Array.isArray(block) || block.length === 0) {
    fail(`${label}: required, must be a non-empty list`);
  }
  const values = [];
  const seen = new Set();
  block.forEach((entry, i) => {
    if (entry === null || typeof entry !== 'object' || Array.isArray(entry)) {
      fail(`${label}[${i}]: must be a mapping`);
    }
    const value = entry[key];
    if (value === undefined) {
      fail(`${label}[${i}]: '${key}' is required`);
    }
    if (typeof value !== 'string' || value.trim().length === 0) {
      fail(`${label}[${i}]: '${key}' must be a non-empty string`);
    }
    if (/[\r\n]/.test(value)) {
      fail(`${label}[${i}]: '${key}' must be a single line: ${JSON.stringify(value)}`);
    }
    if (seen.has(value)) {
      fail(`${label}[${i}]: duplicate ${label} ${key}: ${value}`);
    }
    seen.add(value);
    values.push(value);
  });
  return values;
}

const analyses = checkEntryList(manifest.analyses, 'analyses', 'name');
const datasetPaths = checkEntryList(manifest.datasets, 'datasets', 'path');

datasetPaths.forEach((p, i) => {
  if (p.startsWith('/') || p.startsWith('\\') || /^[A-Za-z]:[\\/]/.test(p)) {
    fail(`datasets[${i}]: 'path' must be a relative filename, got ${JSON.stringify(p)}`);
  }
  if (p.split(/[\\/]/).includes('..')) {
    fail(`datasets[${i}]: 'path' must not contain '..' segments: ${JSON.stringify(p)}`);
  }
});

// Full validation has succeeded for every entry — only now touch the outputs.
const writeList = (file, list) => fs.writeFileSync(path.resolve(file), list.join('\n') + '\n');
writeList(analysesOutArg, analyses);
writeList(datasetsOutArg, datasetPaths);
process.stdout.write(`manifest-entries: ok: ${analyses.length} analyses, ${datasetPaths.length} datasets\n`);
