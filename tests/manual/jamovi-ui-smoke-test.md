# jamovi UI smoke-test runbook

This runbook records the Computer Use procedures that successfully launched jamovi from a fully quit state and exercised the module analyses on the saved baseline workbooks and clean CSV fixtures. It is intended for future Codex sessions and is deliberately separate from the user-facing `jamovi-functionality-guide.md`.

## Current automated scope

The validated baseline-workbook workflow covers:

- launching jamovi from a quit state;
- normalizing the window;
- opening the small and large saved workbooks;
- forcing the saved PERMANOVA, ANOSIM, PERMDISP, SIMPER, and nMDS analyses to recalculate, and creating Cluster analysis from each clean CSV;
- reading their controls, tables, plot descriptions, guidance, and settings through the accessibility tree;
- checking expected values, conditional outputs, stale-output clearing, control dependencies, and absence of `NaN` or `Inf`;
- checking SIMPER and nMDS at 200% jamovi zoom;
- checking that guidance, warnings, and interpretation text remain within a readable report width, including when the results pane is narrowed;
- checking nMDS output-only invariance, optional-layer independence, and basic keyboard focus recovery; and
- confirming that the workbooks and Git worktree were not modified.

The PERMANOVA redesign regression also covers a new analysis, incomplete inputs, conditional result sections, permutation/block truthfulness, control dependencies, valid → invalid → valid clearing, collapsed-section accessibility, and keyboard operation. Run those checks from a clean CSV as described below before claiming the redesigned UI passed. The ANOSIM, PERMDISP, SIMPER, nMDS, Cluster analysis, and shared invalid-input procedures below were successfully exercised and are now repeatable UI workflows.

Do not claim live coverage for VoiceOver speech, 400% macOS magnification, Windows NVDA, nMDS five-group rendering, or hidden legacy Binary/3D controls; those checks were not physically run.

## Plotting-overhaul verification

**Status: core small- and large-data states validated in jamovi on 22 July 2026; the edge cases explicitly listed below remain unverified.** The checks in this section are the Task 10 acceptance procedure for the new `ggplot2` outputs and PCoA. Do not infer coverage for a detailed option or fixture merely because the core analysis ran successfully.

Run every analysis from both clean CSVs so cached workbook output cannot satisfy an assertion. For the small file assign `feature_01`–`feature_08`; for the large file assign `feature_01`–`feature_48`. Where grouping is required or requested, assign `group`. After each control change, fetch a fresh accessibility state and poll for a named changed result. For every image assert its exposed title, its adjacent description, and the named table alternative below. Also confirm its declared dimensions, absence of report-level horizontal scrolling, and absence of `NaN` or `Inf`.

### Pending PERMANOVA plot procedure

1. Create PERMANOVA from each clean CSV with the baseline settings in `jamovi-functionality-guide.md`. Record the complete **PERMANOVA Table**.
2. Confirm **Companion PCoA** is off and that **PERMANOVA Companion PCoA**, **PERMANOVA Companion PCoA**, coordinates, and centroids are absent.
3. Select **Companion PCoA**. For the simple model, assert `group` is the effective display factor and that the 600 × 500 **PERMANOVA Companion PCoA**, **PERMANOVA Companion PCoA**, and **Companion PCoA Site Coordinates** appear.
4. Select **Group Centroids** and **Connect Sites to Centroids** independently. Assert the image and description report the effective layers and the full-data coordinate and centroid tables remain available.
5. Add `treatment` under **Study Design and Model → Additional Factors**. Before enabling **Companion PCoA**, assign `treatment` under **Model Factor** and confirm the assignment remains. Enable the plot and verify the styling/table grouping follows the requested factor. The PERMANOVA test remains the inferential result; the companion PCoA is descriptive.
6. With the plot enabled, try an ineligible covariate, a factor removed by filtering, and a deleted/unavailable factor. Confirm each assignment remains in the target and the companion description explicitly names the reason and the eligible retained factors; the PERMANOVA table and any calculable pairwise output remain visible.
7. Clear **Companion PCoA**. Assert all companion results disappear and the recorded PERMANOVA table is byte-for-byte unchanged by every plot toggle.

### Pending ANOSIM plot procedure

1. Create ANOSIM from each clean CSV and force a baseline calculation with seed `123` (999 small; 199 large).
2. Assert **Ranked-dissimilarity diagnostic** is on by default. Verify the 600 × 480 **Ranked dissimilarities by pair category**, adjacent **Plot details**, and **Ranked-dissimilarity summary** with category, pair count, median, Q1, and Q3.
3. On the small file, verify all finite pair ranks inform the summary. On the large file, verify the description discloses a maximum of 600 displayed marks while summary counts still cover every finite pair.
4. Clear the diagnostic. Assert the image and description disappear, the summary table remains, and Global R, permutation p, and optional pairwise results are unchanged. Restore it and verify deterministic point positions after another recalculation.

### Pending PERMDISP plot procedure

1. Create PERMDISP from each clean CSV. Assert **Distance-to-Centre Diagnostic** is on and **Ordination with group centres** is off by default.
2. Verify the 600 × 460 **Distance to group centre**, adjacent **Plot details**, and full-data **Distance-to-centre summary**. The description must state n, centre choice, and any point limit; recalculation must reproduce point positions.
3. Select **Ordination with group centres**. Verify the 600 × 500 image, **Ordination details**, full-data **Ordination coordinates**, and **Plot key**. Assert equal axis scaling, site-to-centre segments, and no ellipse.
4. Toggle each plot independently. The optional image, its adjacent description, and its table alternative should appear and disappear together. The always-relevant **Distance-to-centre summary**, F, and permutation p must remain invariant.

### Pending SIMPER plot procedure

1. On the small three-group fixture, verify **Contribution plots by contrast** contains three separately titled **Contrast contribution** groups—not one tall composite—with one 580 × 430 image, **Plot details**, and **Values shown in this plot** per contrast.
2. Compare each per-contrast value table with **Descriptive feature contributions**. Confirm complete detailed tables retain omitted features when requested and that the text describes contributions without causal language.
3. For a five-group fixture, assert ten separate bounded contrast groups. If the current fixture cannot provide five groups, record this case as unverified rather than inferring it from three groups.
4. Select **Contrast overview heatmap**. Verify the 600 × 500 image, **Heatmap details**, and **Values shown in the heatmap**; clear it and assert all three disappear without changing contrast values.
5. Repeat on the large file. Assert no image tower, wide composite, clipped heading, or report-level horizontal scrolling.

### Pending nMDS plot procedure

1. Create nMDS from each clean CSV using the documented seed and settings. Verify the 580 × 450 **nMDS Ordination**, equal physical axis scaling, adjacent **Ordination description**, and complete **Site Scores**.
2. Toggle group styling, hulls, 1-SD ellipses, spiders, feature scores, and environmental vectors one at a time. Assert the description names only effective layers and **Site Scores** remain invariant. Ellipses must be described as descriptive, not confidence regions.
3. Select **Shepard Diagram**. Verify the separate 580 × 450 **Shepard Diagram**, **Shepard Diagram**, and **Shepard Diagram Values** containing dissimilarity, ordination distance, and monotone fitted distance.
4. On the large file, assert any Shepard-pair cap is disclosed while the fitted nMDS result remains based on the full data. Clear Shepard and assert stress, site coordinates, and the ordination remain unchanged.

### Pending Cluster plot procedure

1. Create Cluster analysis from each clean CSV. With **Define clusters** off, verify the uncut 600 × 500 **Group-average cluster dendrogram**, **Plot details**, and complete **Dendrogram structure**; **Cluster membership** must be absent.
2. Select **Define clusters**, choose **Number of clusters**, and enter `3`. Verify the cut marking and membership table exactly agree with `stats::cutree(hc, k = 3)` using the same transformed data and distance.
3. Choose **Dissimilarity height** and enter `0.5`. Verify membership against `stats::cutree(hc, h = 0.5)`, a visible cut line when representable, and an explicit tied-height disclosure when a unique line cannot represent the effective cut.
4. Clear **Define clusters**. The membership and cut must disappear while dendrogram branches and structure values remain unchanged.
5. On the large file, verify Automatic labels omit crowded leaf text with a disclosure; Show and Hide must not alter branches or joins. Long labels must be shortened only in the image and preserved in tables.

### Pending PCoA procedure

1. Create **Analyses → Multivariate Inference, Similarity and Ordination (MISO) → PCoA — Visualise distance structure** from each clean CSV. Confirm no-variable guidance and no blank result, then assign the feature range. Use None, Bray-Curtis, Binary off, Square-root distances off, and Additive correction None.
2. Verify the 600 × 500 **Principal coordinates ordination**, equal physical axis scaling, adjacent **Ordination description**, complete **Site Coordinates**, **Eigenvalues**, the relevant result notes, and the relevant table notes. Compare small and large checkpoints with `reference-results.csv` and direct `vegan::wcmdscale`; compare coordinate magnitudes because whole axes may reflect.
3. Add `group`. Verify colour-plus-shape encoding and complete group values in the coordinate table. Select **Group Centroids**, then **Connect Sites to Centroids**; verify **Group Centroids**, effective-layer text, and invariant coordinates/eigenvalues.
4. Run None, Lingoes, and Cailliez corrections. Verify the selected correction and finite constant in the relevant table note and direct `vegan::wcmdscale(add = ...)` parity. Test Square-root distances separately.
5. Create a five-row proportional two-feature case and select Euclidean. Verify the one-positive-axis state retains coordinate/eigenvalue tables and description but hides the two-dimensional image rather than leaving blank space.
6. Remove all required features after a valid result. Verify every stale image/table clears. Restore them, then run the large case and confirm any 1,000-point display cap is disclosed while all site rows remain in the table.

### Pending cross-analysis accessibility and narrow-layout procedure

Run these checks for every analysis above on at least the small result, and repeat the layout checks on the large result:

1. Use Tab and Shift-Tab from the first target box through every visible control. Assert a logical focus order, disabled controls are skipped, focus stays visible, and recalculation does not move focus into the report.
2. Read the accessibility tree from the analysis heading through each image, its immediately adjacent description, and its table alternative. Assert that reading order matches visual order and that shortened marks never replace full identities in tables.
3. Narrow the results pane with the workspace splitter to approximately 320 CSS pixels. Assert narrative text wraps, figures scale within their cards, table columns retain semantics, and the report itself gains no horizontal scrollbar.
4. Use deliberately long feature, site, and group labels. Assert options labels wrap without overlap; figures shorten labels collision-safely; descriptions disclose shortening where applicable; tables preserve full text.
5. Repeat at 200% jamovi zoom and restore 100%. Physically run 400% magnification before marking it passed; otherwise record it as unverified.
6. Physically navigate with VoiceOver and record the spoken analysis heading, image title/description, table title, column headings, and row context. Record VoiceOver speech as unverified until this is done. Windows NVDA remains unverified until the same procedure is run on Windows.

### Task 10 evidence and promotion rule

For each assertion, record: build identifier, operating system, jamovi and module versions, dataset, analysis, option state, expected text/value, observed text/value, pass/fail, and screenshot path for failures. Save accessibility snapshots or compact extracted evidence for named sections and dimensions; do not save recalculated `.omv` files. Record small and large results separately.

### Live plotting-overhaul evidence — 22 July 2026

Environment: superseded pre-rename module build (0.2.0), freshly built and installed into jamovi 2.7.36 on macOS Tahoe 26.5.2; re-record with Multivariate Inference, Similarity and Ordination (MISO) 1.0.0 at the next smoke run. Every analysis was created from `miso-small.csv` (24 rows, 8 feature variables) and `miso-large.csv` (360 rows, 48 feature variables); no cached workbook output was used.

- PCoA: the ordination, Site Coordinates, Eigenvalues, interpretation, and relevant table notes appeared on both datasets with all site rows retained. A live defect exposed `NaN` for explained percentages on non-positive axes; the raw cells were changed to blanks, covered by a regression test, rebuilt, reinstalled, and confirmed finite in jamovi.
- PERMANOVA: the inferential table appeared on both datasets. Enabling the companion PCoA, group centroids, and spiders produced the named images and full coordinate/centroid tables without changing the PERMANOVA table.
- ANOSIM: the ranked-dissimilarity image and complete summary appeared on both datasets. The large description disclosed the 600-mark cap. With seed 123 on the small dataset, toggling the diagnostic left Global R (`-0.0195`) and permutation p (`0.676`) unchanged.
- PERMDISP: the distance diagnostic and summary appeared on both datasets. Enabling the ordination produced the image, site-to-centre segments, Plot key, and complete coordinate table without changing the Dispersion Test. Clearing the optional ordination correctly hid its image, description, and coordinate table together while the always-relevant distance summary remained.
- SIMPER: three separate bounded contrast plots and three plot-value tables appeared on both datasets. The optional heatmap and its value table also appeared. A live defect exposed `NaN` in structurally omitted heatmap cells; those cells were changed to blanks and the plot layer was separated into a grey structural grid plus finite contributions, then rebuilt, reinstalled, and confirmed finite in jamovi.
- nMDS: the two-dimensional ordination, Site Scores, Shepard Diagram, and pair table appeared on both datasets. The large description disclosed that 1,000 of 64,620 diagnostic pairs were displayed while the fit used all finite pairs. With seed 123 on the small dataset, toggling Shepard left stress and site coordinates unchanged.
- Cluster analysis: uncut, three-cluster number-cut, and height-0.5 states were exercised on the small dataset; branches and the Dendrogram structure stayed invariant while membership followed the cut toggle. The large dataset automatically hid labels above 40 samples and disclosed that decision. A live defect exposed `NaN` in structurally absent leaf/merge cells; those cells were changed to blanks, regression-tested, rebuilt, reinstalled, and confirmed finite in jamovi.
- Scale and accessibility: the accessibility tree preserved image → adjacent description → table-alternative order in the exercised results. Forward and reverse keyboard traversal reached the nMDS target boxes, analysis choices, plot controls, and optional sections without leaving the analysis pane. SIMPER and nMDS remained readable and reachable through the exact 100 → 110 → 120 → 133 → 150 → 170 → 200% zoom sequence and were restored to 100%.
- Narrow layout: all seven analyses remained contained when the application was reduced until the results pane was approximately 320 px wide. Narrative, plots, and tables did not paint across the pane boundary, but jamovi retained a fixed-width report page and exposed a horizontal scrollbar rather than satisfying the stricter reflow assertion in the procedure above. Treat this assertion as not passed; do not describe it as module text overflow without first separating host-page behaviour from avoidable module text width.

Still unverified: full keyboard traversal and recalculation-focus recovery across all seven analyses, five-group SIMPER, the one-positive-axis PCoA fixture, live PCoA correction variants, deliberate long-label fixtures, every optional nMDS overlay, Cluster Show/Hide label invariance, 400% macOS magnification, VoiceOver speech, Windows NVDA, nMDS five-group rendering, and hidden legacy Binary/3D controls.

Only after the corresponding procedure is physically exercised may its item move into **Current automated scope**. Update that section with the exact date, environment, datasets, and states tested. Pending, skipped, and platform-unavailable checks must remain explicitly unverified.

## Integer-coded feature assignment matrix

For each analysis, repeat the clean-small-CSV assignment with the same feature columns set to integer storage and the jamovi measure type set in turn to **Continuous**, **Nominal**, and **Ordinal**. The Nominal and Ordinal rows must calculate the same retained sample count, tables, and finite values as Continuous; a text-valued feature assignment must show the analysis-level error while the titled result shells remain visible and blank.

| Analysis | Integer Nominal feature assignment | Integer Ordinal feature assignment |
|---|---|---|
| PERMANOVA | `feature_01`–`feature_08` as integer Nominal | `feature_01`–`feature_08` as integer Ordinal |
| ANOSIM | `feature_01`–`feature_08` as integer Nominal | `feature_01`–`feature_08` as integer Ordinal |
| PERMDISP | `feature_01`–`feature_08` as integer Nominal | `feature_01`–`feature_08` as integer Ordinal |
| nMDS | `feature_01`–`feature_08` as integer Nominal | `feature_01`–`feature_08` as integer Ordinal |
| PCoA | `feature_01`–`feature_08` as integer Nominal | `feature_01`–`feature_08` as integer Ordinal |
| Cluster analysis | `feature_01`–`feature_08` as integer Nominal | `feature_01`–`feature_08` as integer Ordinal |
| SIMPER | `feature_01`–`feature_08` as integer Nominal | `feature_01`–`feature_08` as integer Ordinal |

Record the assignment, retained rows, and the analysis error/shell state in the smoke evidence; do not save recalculated workbooks.

## Test assets

- Small: `tests/manual/workbooks/miso-small-baselines.omv`
- Large: `tests/manual/workbooks/miso-large-baselines.omv`
- Clean small CSV: `tests/manual/miso-small.csv`
- Clean large CSV: `tests/manual/miso-large.csv`
- Full-precision references: `tests/manual/reference-results.csv`

Install the Multivariate Inference, Similarity and Ordination (MISO) build under test before starting. The `.omv` files contain cached results, so opening a workbook is not itself evidence that the current build works.

### Install safeguard

Delete the existing `miso_1.0.0.jmo` before running `jmvtools::install(pkg=".")`. The current compiler can report process exit code 0 after a YAML compilation error and leave the old archive in place. Treat the install as successful only when all of the following are true:

- the output does not contain `Unable to compile` or `Could not install module`;
- a freshly dated `miso_1.0.0.jmo` exists;
- the output contains both `Installing miso_1.0.0.jmo` and `Module installed successfully`; and
- `~/Library/Application Support/jamovi/modules/miso/jamovi.yaml` has a fresh modification time.

For the current PERMANOVA redesign, also inspect the installed `ui/permanova.js` for `Feature Variables`, the Free default, the `studyVariables` supplier, and four real `update_control_states` event handlers. A compiled `execute: function(ui) { }` means the unsupported `changed:` event alias was used instead of the working `change:` spelling. For nMDS, inspect installed `ui/nmds.js` for the title-case variable labels and the two nearby tips. For SIMPER, inspect installed `ui/simper.js` for `Detailed Statistics`, `Contribution variability`, and `Group means`.

## Automation principles

1. Use the Computer Use skill and `node_repl` for all jamovi interactions.
2. Target the app as `jamovi`; `get_app_state({ app: "jamovi" })` launches it when necessary.
3. Normalize the window using the window button's exposed `zoom the window` secondary action.
4. Prefer accessibility labels and fresh `element_index` values. Fetch a new app state after every action that may change the interface.
5. Never hard-code an accessibility index between states. Indices are ephemeral.
6. Use relative coordinates only as a fallback for controls without usable accessibility elements.
7. Wait for a visible state change, such as `Seed: 124`, rather than assuming that a delay means recalculation finished.
8. Do not save the workbook after recalculation.
9. Capture the current app state and screenshot when a check fails.
10. For a native select menu that does not open through its accessibility element, use one relative click to open the visible control, fetch a fresh state for the native menu, then select the named menu item by its new accessibility index.
11. Keep at most one test-data document window open. Reuse that document for every analysis in the current dataset phase.
12. Close each dataset document with Command-W, choose `Don't Save`, and verify its title has disappeared before opening the next fixture.
13. Treat cleanup as part of the test, including process verification after quitting. A failed cleanup is a failed test run.
14. Poll for a named result, option value, or document title. Use fixed delays only for short UI animation settling when no observable state exists.
15. Capture screenshots for failures and new visual acceptance evidence, not for every passing checkpoint.

## Useful helpers

After bootstrapping Computer Use as directed by its skill, this helper locates an accessibility element from the latest state:

```js
var axIndex = function(text, pattern) {
  var line = text.split("\n").find(l => pattern.test(l));
  if (!line)
    throw new Error("AX element not found: " + pattern);
  var match = line.match(/^\s*(\d+)\s/);
  if (!match)
    throw new Error("No AX index: " + line);
  return Number(match[1]);
};
```

Use polling for recalculation rather than a blind sleep:

```js
var waitForText = async function(pattern, timeoutMs = 30000) {
  var deadline = Date.now() + timeoutMs;
  while (Date.now() < deadline) {
    var state = await sky.get_app_state({ app: "jamovi", disableDiff: true });
    if (pattern.test(state.text))
      return state;
    await new Promise(resolve => setTimeout(resolve, 500));
  }
  throw new Error("Timed out waiting for " + pattern);
};
```

Close and discard the current document before moving to the next dataset:

```js
var closeCurrentDocument = async function(oldTitle) {
  await sky.press_key({ app: "jamovi", key: "CMD+W" });
  var state = await waitForText(/Don't Save|Discard|Save/);
  var discard = axIndex(state.text, /button (Don't Save|Discard)/);
  await sky.click({ app: "jamovi", element_index: discard });

  var deadline = Date.now() + 10000;
  while (Date.now() < deadline) {
    state = await sky.get_app_state({ app: "jamovi", disableDiff: true });
    if (!state.text.includes(oldTitle))
      return state;
    await new Promise(resolve => setTimeout(resolve, 250));
  }
  throw new Error("Document did not close: " + oldTitle);
};
```

If Computer Use loses its connection during cleanup, stop sending UI actions. Check the jamovi process and visible-window state independently; do not infer that the document or application closed.

## Procedure

### Fast default workflow

Run the smoke test in three dataset phases. The numbered analysis sections below are assertion references; they are not instructions to reopen the same fixture for every analysis.

1. Launch and normalize jamovi once.
2. Open one small-data document. Create or activate every analysis needed for PERMANOVA, ANOSIM, PERMDISP, SIMPER, nMDS, and Cluster, then run the full small-data option and accessibility matrices. Use a saved baseline only for an exact checkpoint that the clean CSV cannot reproduce reliably.
3. Close the small document, choose `Don't Save`, and verify that its title is absent.
4. Open one large-data document. Run every analysis once and check the large-data scale, row-count, plot-containment, and no-stale-output assertions. Do not repeat the full small-data option matrix.
5. Close the large document, choose `Don't Save`, and verify that its title is absent.
6. Open the invalid-input fixture as a separate document because its transitions are destructive. Run the shared invalid-input matrix, then close and discard it.
7. Restore 100% zoom and normal window geometry, quit jamovi, verify that no jamovi process remains, check fixture integrity, and run `git diff --check`.

This ordering minimizes launches, file dialogs, recalculations, and leftover windows while keeping the small and large datasets independent validation layers. The 400% magnification, VoiceOver, and Windows NVDA procedures remain deferred until they are physically exercised; do not include them in routine smoke-test timing or claim them as passed.

### 1. Launch and normalize jamovi

1. Call `sky.get_app_state({ app: "jamovi", disableDiff: true })`.
2. In the returned state, locate the window's `full screen button` whose secondary action is `zoom the window`.
3. Invoke `sky.perform_secondary_action()` with the exact exposed action `zoom the window`.
4. Fetch a fresh full state and confirm that the Multivariate Inference, Similarity and Ordination (MISO) module is listed in the Analyses ribbon.

This provides consistent geometry. The remaining workflow should still use accessibility elements rather than coordinates.

### 2. Open the small workbook

1. Locate and click the `File` pop-up button from the latest state.
2. Select `miso-small-baselines.omv` from Recent files when present.
3. If it is not recent, use **Open** and browse to `tests/manual/workbooks/miso-small-baselines.omv`.
4. Wait until the window title is `miso-small-baselines` and the data status reports 24 rows.

### 3. Recalculate small PERMANOVA

1. Locate and click `container PERMANOVA- Results`.
2. Fetch a fresh full state.
3. Locate `text field ... Random seed, Value: 123`.
4. Set it to `124` and press Return.
5. Poll until the PERMANOVA the relevant table notes table displays `Random seed` followed by `124`.
6. From the new state, locate the seed field with value `124`.
7. Set it back to `123` and press Return.
8. Poll until the PERMANOVA the relevant table notes table again displays `Random seed` followed by `123`.

The intermediate seed value is essential: it proves that jamovi committed the changed option and recalculated instead of merely redisplaying cached results.

### 4. Verify small PERMANOVA

Within `container PERMANOVA- Results`, isolate `table PERMANOVA Table` and stop before `table Pairwise PERMANOVA`.

Assert these rendered rows:

| Row | Expected accessible cell text |
|---|---|
| `group` | `group`, `2`, `0.554`, `0.378`, `6.37`, `.001` |
| `Residual` | `Residual`, `21`, `0.913`, `0.622` |
| `Total` | `Total`, `23`, `1.467`, `1.000` |

The shortened Residual and Total rows are intentional. Their missing Pseudo-F and Permutation p entries must be plain blank cells without superscripts, cell footnotes, or a table note. Fail if `NaN` appears anywhere in the PERMANOVA table.

### 5. Open and recalculate the large workbook

1. Open the File menu from the latest state.
2. Select `miso-large-baselines.omv`.
3. Wait until the window title is `miso-large-baselines` and the data status reports 360 rows.
4. Activate `container PERMANOVA- Results`.
5. Confirm that Random seed is `123` and Permutations is `199`.
6. Repeat the committed seed sequence `123` to `124` to `123`, pressing Return and polling for each corresponding the relevant table notes value.

Do not save the small workbook when switching. If jamovi presents a discard-changes confirmation, follow the current Computer Use confirmation policy before acting.

### 6. Verify large PERMANOVA

Assert these rendered rows:

| Row | Expected accessible cell text |
|---|---|
| `group` | `group`, `2`, `9.47`, `0.394`, `116`, `.005` |
| `Residual` | `Residual`, `357`, `14.56`, `0.606` |
| `Total` | `Total`, `359`, `24.04`, `1.000` |

Again, Residual and Total must have blank F and p cells, and `NaN` must not appear.

### 7. Run the PERMANOVA state matrix from a clean CSV

Open `tests/manual/miso-small.csv`, create a new **Analyses → Multivariate Inference, Similarity and Ordination (MISO) → PERMANOVA — Test group differences**, and fetch a fresh accessibility state after every change.

1. With no variables assigned, assert that **Getting Started** contains both required steps. Assert that the empty result tables and table notes are absent from the accessibility tree.
2. Assign `feature_01`–`feature_08` to **Feature Variables** using keyboard selection and the transfer arrow. Assert that **Action needed** asks for a categorical Grouping variable and that result tables remain absent.
3. Assign `group` to **Grouping Variable**. Wait for `table PERMANOVA Table`, then assert that `table Pairwise PERMANOVA` and `Data handling warnings` are absent.
4. Select **Pairwise comparisons**. Wait for a populated `table Pairwise PERMANOVA`. Clear Pairwise and assert that the table is removed from the accessibility tree.
5. Expand **Study Design and Model**, assign `block` to **Blocking Variable**, and leave **Permutation restrictions** at **Free**. Assert that **Data handling warnings** says `Blocking variable 'block'` is assigned but not used, and the PERMANOVA table note reports `Block used: No`.
6. Remove `block`, select **Within blocks — requires a Blocking variable**, and assert that the actionable correction appears with no inferential rows. Reassign `block` and assert that the table returns, the warning disappears, and the PERMANOVA table note reports `Block used: Yes` and names `block`.
7. Restore **Free**, remove `group`, and assert that all previous rows and headings disappear. Restore `group` and assert that a fresh standard result appears.

Do not save the CSV as a workbook.

### 8. Check dependencies and collapsed-section accessibility

1. With Pairwise cleared, confirm **P-value adjustment** exposes a disabled state and cannot receive keyboard focus.
2. With no Additional factor, confirm **Model Interactions** is disabled.
3. Assign `treatment` to **Additional Factors** and confirm **Model Interactions** becomes available.
4. Select interactions and confirm an unselected **Pairwise comparisons** becomes unavailable. Clear interactions, select Pairwise, and confirm an unselected **Model Interactions** becomes unavailable.
5. Select **Omnibus** and confirm an unselected Pairwise control is unavailable. A saved legacy conflict must keep its already-selected conflicting control operable so it can be cleared.
6. Collapse **Study Design and Model**. Confirm its descendants are absent from the accessibility tree and cannot receive focus. Expand it and confirm `aria-expanded`/expanded state, accessible region name, and logical focus order.
7. Reopen an analysis containing a block or non-default Test type and confirm the study-design section auto-expands. Reopen one containing a non-default seed or permutation count and confirm **Reproducibility and technical settings** auto-expands.
8. Confirm result updates do not move keyboard focus into the report.

### 9. Check magnification and table semantics

1. At 200% display or text scaling, confirm all controls remain readable, reachable, and unclipped.
2. At 400% macOS magnification, confirm keyboard focus remains visible and every control/result remains reachable; do not claim browser-style reflow.
3. With VoiceOver, navigate the PERMANOVA table and confirm it announces the table title, column heading, Source-row context, and the blank Residual/Total cells without adding superscript markers.
4. Record Windows NVDA as unverified unless the same checks are physically run on Windows.

### 10. Run the SIMPER workflow

Use the saved baselines so all variables are already assigned. Opening cached results is not a pass; force recalculation through a visible option.

#### Small baseline

1. Open `miso-small-baselines.omv`, activate `container SIMPER- Results`, and fetch a fresh full state.
2. Change **Top N features** from `10` to `9`. Wait for the relevant table notes to display `9`.
3. Assert that the visible **Descriptive feature contributions** table has only `Contrast`, `Feature`, `Contribution (%)`, and `Cumulative (%)`. The legacy wide internal table must not appear.
4. Confirm three contrasts and the expected first rows: A vs B `feature_04` at about `21.2`, A vs C `feature_01` at about `26.3`, and B vs C `feature_04` at about `24.4`.
5. Select **Detailed Statistics**. Assert a five-column **Contribution Variability** table and a four-column **Group Means** table, both contained within the report width. Clear the option and assert that both tables disappear.
6. With **Assess contributions with permutations** clear, confirm its permutation count, adjustment, and seed controls are unavailable. Select it, enter `19`, retain Holm, and enter seed `123`. Assert a populated four-column **Exploratory permutation assessment** and truthful settings. Clear the assessment.
7. Remove the required grouping variable. Assert that all prior SIMPER tables, plot, and assessment disappear and that guidance requests a grouping variable. Restore `group` and wait for all three contrasts to return without stale rows.
8. At 200% jamovi zoom, confirm the option tips, four-column main table, and optional detail tables remain readable and reachable. The zoom sequence is `100 → 110 → 120 → 133 → 150 → 170 → 200`; restore it in reverse.
9. Fail if `NaN` or `Inf` appears in the SIMPER result block.

#### Large baseline

1. Open `miso-large-baselines.omv`, activate SIMPER, and change Top N `10 → 9`.
2. Assert the relevant result tables reports 360 analysed samples, 48 features, three groups, and three contrasts.
3. Assert exactly 9 contribution rows per contrast, 27 data rows total. The A vs B first row is `feature_47`, contribution `6.54`, cumulative `6.54`.
4. Confirm the compact main table, plot, and settings appear without horizontal overflow or stale small-data rows.

### 11. Run the nMDS workflow

#### Small baseline

1. Open `miso-small-baselines.omv`, activate `container nMDS- Results`, and confirm eight required features, `group`, `temperature` plus `pH`, None/Bray-Curtis, Shepard on, feature scores off, environmental permutations `99`, seed `123`, starts `20`, and iterations `200`.
2. Change seed `123 → 124 → 123`. Wait for the relevant table notes after each change and do not read the environmental rows until the requested seed appears there. At seed 123, expect stress about `0.1636`, 24 **Site Scores**, temperature r² about `.292` with p `.060`, and pH r² about `.238` with p `.100`.
3. Remove `group`. The ordination, Site Scores, and Environmental Fit must remain; group styling controls become unavailable and the plot description reports no grouping. Restore `group` and confirm stress/configuration are unchanged.
4. Remove `pH`, then all environmental variables. Environmental Fit and vectors must follow the requested variables while the base ordination remains. Restore `temperature` and `pH`.
5. Clear **Shepard Diagram** and select **Feature Scores**. Assert only the requested output changes and Feature Scores contains 8 rows. Restore Shepard on and Feature Scores off; Site Scores must remain identical.
6. Change environmental permutations `99 → 19`. Expect temperature p `.050` and pH p `.250`; restore `99`.
7. Select Standardize with Bray-Curtis. Assert an actionable compatibility message and no stale ordination, scores, or environmental fit. Select Euclidean to recover, then restore None/Bray-Curtis.
8. Remove all required features. Assert the nMDS result block contains only getting-started guidance asking for at least two numeric features. Restore all eight features and wait for stress `0.1636` and the complete result set.
9. At 200% zoom, confirm **Grouping Variable** and **Environmental Variables** are not cropped, the compact tips in **Group display** and **Environmental fit** are visible, and the report remains reachable. Restore 100%.
10. Focus Random seed, press Tab and Shift-Tab, and confirm logical movement among the three reproducibility fields. Change the seed with `set_value`; after recalculation, the accessibility tree should still identify the seed field as focused rather than moving focus into the report. Restore seed `123`.
11. Confirm the accessibility tree exposes the nMDS heading; named tables and images; the compact ordination description; `p (unadjusted)` headers; and the descriptive, non-inferential overlay warning.

#### Large baseline

1. Open `miso-large-baselines.omv`, activate nMDS, and change seed `123 → 124 → 123`.
2. At seed 123, expect stress about `.1844`, 360 Site Scores, temperature r² about `.385` with p `.010`, and pH r² about `.529` with p `.010`.
3. Select Feature Scores, hulls, the 1-SD ellipse, and spiders. Assert 48 Feature Score rows, unchanged Site Scores, all requested layers in the plot description, two environmental vectors, and truthful feature-label omission text.
4. Clear the optional layers and confirm the base Site Scores remain unchanged.

### 12. Run the ANOSIM workflow

#### Small baseline

1. Open `miso-small-baselines.omv`, activate ANOSIM, and change seed `123 → 124 → 123` to force recalculation.
2. At seed 123, assert Global R is about `.495` and permutation p is `.001`.
3. Select **Pairwise ANOSIM comparisons** with Holm adjustment. Assert the A vs B row has R about `.678` and adjusted p `.003`.
4. Clear Pairwise. Assert its table disappears and **P-value adjustment** becomes unavailable.

#### Large baseline

1. Open `miso-large-baselines.omv`, activate ANOSIM, and change seed `123 → 124 → 123`.
2. At seed 123, assert Global R is about `.829` and permutation p is `.005`.
3. Select Pairwise with Holm adjustment. Assert the A vs B row has R about `.985` and adjusted p `.015`.
4. Clear Pairwise and assert that its table disappears without stale rows.

### 13. Run the PERMDISP workflow

#### Small baseline

1. Open `miso-small-baselines.omv`, activate PERMDISP, and change seed `123 → 124 → 123`.
2. At seed 123 with the median centre, assert F is about `1.69`, permutation p is about `.219`, and mean distances for A/B/C are about `.160 / .163 / .221`.
3. Assert the Residuals-row F and p cells are blank rather than `NaN`.
4. Select **Pairwise dispersion comparisons** with Holm adjustment. Assert the table appears and the A vs B row has t about `−0.0926` and p about `.920`.
5. Clear Pairwise and assert its table disappears.
6. Select **Centroid** and **Bias adjustment**. Assert F is about `2.16`, p about `.148`, and mean distances are about `.172 / .178 / .237`. Restore Median and clear Bias adjustment.

#### Large baseline

1. Open `miso-large-baselines.omv`, activate PERMDISP, and change seed `123 → 124 → 123`.
2. At seed 123, assert F is about `372.5`, permutation p is `.005`, and mean distances for A/B/C are about `.158 / .182 / .247`.
3. Select Pairwise with Holm adjustment. Assert the table is populated and adjusted p values are `.015`; clear Pairwise and assert its table disappears.

### 14. Run the shared invalid-input matrix

Open `tests/manual/miso-invalid.csv`, create a new PERMANOVA, and return to a clean analysis before each case.

1. Assign only `group`. Assert **Getting started** shows both required steps and no inferential table appears.
2. Assign `valid_01` and `valid_02` without a group. Assert the guidance requests a Grouping variable and no inferential table appears.
3. Try to move `text_feature` to **Feature Variables**. Assert jamovi refuses it and announces **Incorrect measure or data type**.
4. Assign `valid_01` plus `negative_feature`, with group `group`. Assert the guidance names `negative_feature` and no inferential table appears.
5. Assign `valid_01` plus `missing_feature`, with group `group`. Assert one row is excluded, seven samples are analysed, and the inferential table is populated.
6. Before the all-zero-feature case, open **Variables → Edit** for `all_zero_feature` and set **Measure type: Continuous**. Constant columns are otherwise imported as Nominal. Assign it with `valid_01` and group `group`; assert one all-zero feature is excluded and the inferential table is populated.
7. Assign `zero_case_01` and `zero_case_02`, with group `group`. Assert one all-zero sample is excluded, seven samples are analysed, and the inferential table is populated.
8. Assign `valid_01` and `valid_02`, with group `single_group`. Assert the guidance reports fewer than two groups and no inferential table appears.

### 15. Run the Cluster analysis workflow

Use the clean CSV fixtures because the saved baseline workbooks predate Cluster analysis.

#### Small dataset

1. Open `tests/manual/miso-small.csv` and create **Analyses → Multivariate Inference, Similarity and Ordination (MISO) → Cluster analysis — Visualise sample similarity**.
2. Assign `feature_01`–`feature_08` to **Feature Variables**.
3. Assert the relevant result tables reports 24 samples, 8 features, and zero exclusions.
4. Confirm the group-average dendrogram is visibly drawn, uses row numbers when **Sample Labels** is empty, and the relevant table notes reports None, Bray-Curtis, Group average (UPGMA), Data row numbers, and labels shown.
5. Confirm the 540 × 360 plot and its surrounding interpretation fit within the report card without report-level horizontal scrolling.
6. Fail if `NaN` or `Inf` appears in the Cluster result block.

#### Large dataset

1. Open `tests/manual/miso-large.csv`, create Cluster analysis, and assign `feature_01`–`feature_48`.
2. Assert the relevant result tables reports 360 samples, 48 features, and zero exclusions.
3. With **Show sample labels** selected, assert that the crowding warning appears and the dendrogram remains visible.
4. Clear **Show sample labels**. Assert that the crowding warning disappears, the dendrogram remains visible, and relevant table notes disclose labels not shown.
5. Confirm the plot remains contained without report-level horizontal scrolling. Fail if `NaN` or `Inf` appears.

### 16. Check result text width across analyses

1. In a clean analysis for each of PERMANOVA, ANOSIM, PERMDISP, nMDS, and SIMPER, inspect the getting-started guidance in the report.
2. For PERMANOVA, assign one numeric Feature variable without a Grouping variable and inspect the waiting-for-group guidance.
3. Narrow the results pane with the workspace splitter. Confirm guidance, warnings, tips, and interpretation paragraphs wrap within the white report card instead of extending horizontally or forcing report-level horizontal scrolling.
4. Confirm normal result tables retain their column structure. Treat a genuinely wide data table separately from avoidable narrative-text overflow.

### 17. Finish safely

1. Leave the random seed at its documented value of `123`.
2. Restore 100% zoom and normal window geometry.
3. Close the current test document with Command-W, choose `Don't Save`, and verify that its title disappears. Do not press Save or Save As.
4. Quit jamovi. Outside Computer Use, verify that no jamovi process remains; if one remains, close it and record cleanup as failed.
5. Run `git status --short` and `git diff --check` outside Computer Use.
6. Confirm that the two `.omv` files and all CSV fixtures are unchanged and that no unexpected files were created.
7. Report small and large assertions separately, including any mismatch and captured evidence. Report deferred accessibility checks as unverified rather than passed.

## Validated result

PERMANOVA passed on 16 July 2026 with jamovi 2.7.36 from a fully quit application state, using accessibility elements after normalizing the window.

ANOSIM, PERMDISP, SIMPER, and nMDS passed their small- and large-baseline workflows on 19 July 2026 with jamovi 2.7.36. The shared invalid-input matrix also passed, including current jamovi type handling for the constant all-zero feature. Guidance and other narrative text for the analyses checked that day remained contained when the results pane was narrowed. Accessibility elements were preferred throughout; relative coordinates were used only to select target-list rows whose accessibility click was inert. Both workbooks remained unmodified. The unverified assistive-technology and legacy-control checks listed above remain explicit gaps rather than pass claims.

Cluster analysis passed its clean small- and large-CSV workflows on 20 July 2026 with jamovi 2.7.36. The live result displayed the 24-sample and 360-sample dendrograms, the large-data label-crowding warning followed the label toggle, and the 540 × 360 image stayed within the report width. Neither CSV nor either saved workbook was modified.
