# jamovi UI smoke-test runbook

This runbook records the Computer Use procedure that successfully launched jamovi from a fully quit state, recalculated PERMANOVA on both baseline workbooks, and verified the rendered tables. It is intended for future Codex sessions and is deliberately separate from the user-facing functionality guide in `README.md`.

## Current automated scope

The validated baseline-workbook workflow covers:

- launching jamovi from a quit state;
- normalizing the window;
- opening the small and large saved workbooks;
- forcing PERMANOVA to recalculate;
- reading the rendered PERMANOVA table through the accessibility tree;
- checking expected values, structural blank cells, and absence of `NaN`; and
- confirming that the workbooks and Git worktree were not modified.

The PERMANOVA redesign regression also covers a new analysis, incomplete inputs, conditional result sections, permutation/block truthfulness, control dependencies, valid → invalid → valid clearing, collapsed-section accessibility, and keyboard operation. Run those checks from a clean CSV as described below before claiming the redesigned UI passed.

ANOSIM, PERMDISP, nMDS, SIMPER, and invalid-input UI checks remain manual until equivalent automation has been exercised and added here.

## Test assets

- Small: `tests/manual/workbooks/tofu-small-baselines.omv`
- Large: `tests/manual/workbooks/tofu-large-baselines.omv`
- Full-precision references: `tests/manual/reference-results.csv`

Install the tofu build under test before starting. The `.omv` files contain cached results, so opening a workbook is not itself evidence that the current build works.

### Install safeguard

Delete the existing `tofu_0.2.0.jmo` before running `jmvtools::install(pkg=".")`. The current compiler can report process exit code 0 after a YAML compilation error and leave the old archive in place. Treat the install as successful only when all of the following are true:

- the output does not contain `Unable to compile` or `Could not install module`;
- a freshly dated `tofu_0.2.0.jmo` exists;
- the output contains both `Installing tofu_0.2.0.jmo` and `Module installed successfully`; and
- `~/Library/Application Support/jamovi/modules/tofu/jamovi.yaml` has a fresh modification time.

For the current PERMANOVA redesign, also inspect the installed `ui/permanova.js` for `Required: Feature variables`, the Free default, the `studyVariables` supplier, and four real `update_control_states` event handlers. A compiled `execute: function(ui) { }` means the unsupported `changed:` event alias was used instead of the working `change:` spelling.

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

## Procedure

### 1. Launch and normalize jamovi

1. Call `sky.get_app_state({ app: "jamovi", disableDiff: true })`.
2. In the returned state, locate the window's `full screen button` whose secondary action is `zoom the window`.
3. Invoke `sky.perform_secondary_action()` with the exact exposed action `zoom the window`.
4. Fetch a fresh full state and confirm that the tofu module is listed in the Analyses ribbon.

This provides consistent geometry. The remaining workflow should still use accessibility elements rather than coordinates.

### 2. Open the small workbook

1. Locate and click the `File` pop-up button from the latest state.
2. Select `tofu-small-baselines.omv` from Recent files when present.
3. If it is not recent, use **Open** and browse to `tests/manual/workbooks/tofu-small-baselines.omv`.
4. Wait until the window title is `tofu-small-baselines` and the data status reports 24 rows.

### 3. Recalculate small PERMANOVA

1. Locate and click `container PERMANOVA- Results`.
2. Fetch a fresh full state.
3. Locate `text field ... Random seed, Value: 123`.
4. Set it to `124` and press Return.
5. Poll until the PERMANOVA **Analysis settings** table displays `Random seed` followed by `124`.
6. From the new state, locate the seed field with value `124`.
7. Set it back to `123` and press Return.
8. Poll until the PERMANOVA **Analysis settings** table again displays `Random seed` followed by `123`.

The intermediate seed value is essential: it proves that jamovi committed the changed option and recalculated instead of merely redisplaying cached results.

### 4. Verify small PERMANOVA

Within `container PERMANOVA- Results`, isolate `table PERMANOVA Table` and stop before `table Pairwise PERMANOVA`.

Assert these rendered rows:

| Row | Expected accessible cell text |
|---|---|
| `group` | `group`, `2`, `0.554`, `0.378`, `6.37`, `.001` |
| `Residual` | `Residual`, `21`, `0.913`, `0.622` |
| `Total` | `Total`, `23`, `1.467`, `1.000` |

The shortened Residual and Total rows are intentional. Their missing Pseudo-F and Permutation p entries must be blank, and the table note or cell footnote must identify them as not applicable. Fail if `NaN` appears anywhere in the PERMANOVA table.

### 5. Open and recalculate the large workbook

1. Open the File menu from the latest state.
2. Select `tofu-large-baselines.omv`.
3. Wait until the window title is `tofu-large-baselines` and the data status reports 360 rows.
4. Activate `container PERMANOVA- Results`.
5. Confirm that Random seed is `123` and Permutations is `199`.
6. Repeat the committed seed sequence `123` to `124` to `123`, pressing Return and polling for each corresponding **Analysis settings** value.

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

Open `tests/manual/tofu-small.csv`, create a new **Analyses → tofu → PERMANOVA — Test group differences**, and fetch a fresh accessibility state after every change.

1. With no variables assigned, assert that **Getting started** contains both required steps. Assert that `Data Summary`, `PERMANOVA Table`, `Pairwise PERMANOVA`, `Interpretation`, and `Analysis settings` are absent from the accessibility tree.
2. Assign `feature_01`–`feature_08` to **Required: Feature variables** using keyboard selection and the transfer arrow. Assert that **Action needed** asks for a categorical Grouping variable and that result tables remain absent.
3. Assign `group` to **Required: Grouping variable**. Wait for `table PERMANOVA Table`, then assert that `table Pairwise PERMANOVA` and `Data handling warnings` are absent.
4. Select **Pairwise comparisons**. Wait for a populated `table Pairwise PERMANOVA`. Clear Pairwise and assert that the table is removed from the accessibility tree.
5. Expand **Study design and model — use when part of your study design**, assign `block` to **Blocking variable**, and leave **Permutation restrictions** at **Free**. Assert that **Data handling warnings** says the block is not used and **Analysis settings** reports `Block used` followed by `No`.
6. Remove `block`, select **Within blocks — requires a Blocking variable**, and assert that only the correction appears; the inferential tables must be absent. Reassign `block` and assert that the table returns and settings report `Block used` followed by `Yes`.
7. Restore **Free**, remove `group`, and assert that all previous rows and headings disappear. Restore `group` and assert that a fresh standard result appears.

Do not save the CSV as a workbook.

### 8. Check dependencies and collapsed-section accessibility

1. With Pairwise cleared, confirm **P-value adjustment** exposes a disabled state and cannot receive keyboard focus.
2. With no Additional factor, confirm **Include interactions** is disabled.
3. Assign `treatment` to **Additional factors** and confirm **Include interactions** becomes available.
4. Select interactions and confirm an unselected **Pairwise comparisons** becomes unavailable. Clear interactions, select Pairwise, and confirm an unselected **Include interactions** becomes unavailable.
5. Select **Omnibus** and confirm an unselected Pairwise control is unavailable. A saved legacy conflict must keep its already-selected conflicting control operable so it can be cleared.
6. Collapse **Study design and model — use when part of your study design**. Confirm its descendants are absent from the accessibility tree and cannot receive focus. Expand it and confirm `aria-expanded`/expanded state, accessible region name, and logical focus order.
7. Reopen an analysis containing a block or non-default Test type and confirm the study-design section auto-expands. Reopen one containing a non-default seed or permutation count and confirm **Reproducibility and technical settings** auto-expands.
8. Confirm result updates do not move keyboard focus into the report.

### 9. Check magnification and table semantics

1. At 200% display or text scaling, confirm all controls remain readable, reachable, and unclipped.
2. At 400% macOS magnification, confirm keyboard focus remains visible and every control/result remains reachable; do not claim browser-style reflow.
3. With VoiceOver, navigate the PERMANOVA table and confirm it announces the table title, column heading, Source-row context, and the not-applicable meaning of Residual/Total structural cells.
4. Record Windows NVDA as unverified unless the same checks are physically run on Windows.

### 10. Finish safely

1. Leave the random seed at its documented value of `123`.
2. Do not press Save or Save As.
3. Run `git status --short` outside Computer Use.
4. Confirm that the two `.omv` files are unchanged and that no unexpected files were created.
5. Report small and large assertions separately, including any mismatch and captured evidence.

## Validated result

On 16 July 2026 with jamovi 2.7.36, the full procedure passed from a quit application state. The interface was controlled through accessibility elements after normalizing the window; no coordinate clicks were required.
