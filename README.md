# tofu

`tofu` brings the community-ecology methods from the R [`vegan`](https://cran.r-project.org/package=vegan) package into [jamovi](https://www.jamovi.org). It covers PERMANOVA, PERMDISP, ANOSIM, SIMPER, and nMDS, so you can run them on abundance-style data without leaving the spreadsheet.

Not all features are included as we have deliberately ported only the most commonly used techniques for teaching purposes. If you need additional functionality for teaching, let us know. Otherwise, please use `vegan` directly.

`tofu` supports the following analyses:

| Analysis | vegan function | What it does |
|---|---|---|
| PERMANOVA | `vegan::adonis2` | Compare groups with permutation tests |
| ANOSIM | `vegan::anosim` | Rank-based group comparison |
| PERMDISP | `vegan::betadisper` | Check multivariate dispersion, with a distance-to-centre plot |
| nMDS | `vegan::metaMDS` | Ordinate samples, with stress and Shepard plots |
| SIMPER | `vegan::simper` | See which features drive Bray-Curtis dissimilarity |

## Installation

Install `tofu` from jamovi's module library, or build it from source:

```r
# install.packages("jmvtools")
jmvtools::install()
```

We have plans to submit `tofu` to the jamovi library in the near future.

## Manual testing in jamovi

This guide tests the module you see and use in jamovi. It is organised by analysis, so if you changed PERMANOVA you can go directly to **Test PERMANOVA**. The tests check that controls are wired correctly, results render, warnings are useful, and key values agree with direct calls to `vegan`.

A test passes when its output matches the stated reference. It does **not** pass or fail because a p-value happens to be significant. These manual checks complement the automated R tests; they do not establish whether an analysis is appropriate for a research question.

### Rebuild and verify the current checkout

Do this before trusting any jamovi result:

1. If tofu is already installed, open jamovi, select **Modules** in the top-right, open the installed-modules view, and remove **tofu**.
2. Fully quit jamovi.
3. In a terminal, change to the tofu repository and confirm the path:

   ```bash
   pwd
   ```

4. Build and install that checkout:

   ```bash
   R -q -e 'jmvtools::install(pkg = ".")'
   ```

5. Relaunch jamovi. Open **Analyses → tofu** and confirm that the five analyses below are present.
6. Open PERMANOVA and confirm that it includes **Covariates (continuous)**. Open nMDS and confirm that it includes **Environmental variables**.

Record the operating system plus the jamovi and tofu versions with your test notes. The version alone is not proof that the new build loaded: an older build may also say `0.2.0`. If the menu or controls do not match this guide, remove tofu, fully quit jamovi, rebuild from the confirmed path, and relaunch before investigating the analysis.

### Choose a test scope

| Scope | Use it when | Run |
|---|---|---|
| Quick card | You changed one analysis | That analysis with `tofu-small.csv` |
| Functionality regression | You changed its controls or results | Its small-data quick card, advanced checks, and 199-permutation large-data check |
| Full release regression | Shared preprocessing changed or a release is being prepared | All quick cards, advanced and negative checks, then the 999-permutation large-data robustness pass |

The 199- and 999-permutation large-data checks are alternative scopes. You do not need to run both consecutively.

### Open and prepare the test data

The manual-test files are:

- [`tests/manual/tofu-small.csv`](tests/manual/tofu-small.csv): 24 samples and 8 abundance features; use this for readable correctness checks.
- [`tests/manual/tofu-large.csv`](tests/manual/tofu-large.csv): 360 samples and 48 independently generated features; use this as a second correctness and robustness check.
- [`tests/manual/tofu-invalid.csv`](tests/manual/tofu-invalid.csv): dedicated bad-input cases; use it only for the negative checks.
- [`tests/manual/reference-results.csv`](tests/manual/reference-results.csv): full-precision audit values behind the rounded checkpoints below.
- [`tests/manual/workbooks/tofu-small-baselines.omv`](tests/manual/workbooks/tofu-small-baselines.omv): the small dataset with all five baseline analyses already configured.
- [`tests/manual/workbooks/tofu-large-baselines.omv`](tests/manual/workbooks/tofu-large-baselines.omv): the large dataset with all five baseline analyses already configured.

To open a file, select **File (☰) → Open → This PC → Browse**, navigate to `tests/manual`, and select the CSV.

For a faster repeat run, open one of the `.omv` workbooks instead. Install the current tofu build first, then force each saved analysis to recalculate by changing its random seed and changing it back to the documented value. Do not judge a new build from cached workbook results. These repository-only workbooks are excluded from the installed module.

Before analysing either clean dataset, open each column's **Setup** and confirm:

| Columns | Data type | Measurement level |
|---|---|---|
| `sample_id` | Text | ID |
| `group`, `treatment`, `block` | Text | Nominal |
| `temperature`, `pH` | Decimal | Continuous |
| `feature_01` through the final feature | Decimal | Continuous |

To assign one variable, drag it into the named target box, or select it and use the right-arrow. To assign all abundance columns, select `feature_01`, Shift-click the final feature, then transfer the selected range into **Feature variables**. Click a panel's chevron to expand **Resemblance** or the analysis-specific options.

jamovi recalculates automatically; there is no **Run** button. Wait until the spinner or progress message disappears and all named result sections have appeared. If you edit test data, reopen the clean CSV before starting another card.

### What tofu currently supports

This table describes current computation, including controls that are visible but limited or ignored.

| Capability | PERMANOVA | ANOSIM | PERMDISP | nMDS | SIMPER |
|---|---|---|---|---|---|
| Transformation | Yes | Yes | Yes | Yes | Yes |
| Selectable dissimilarity | Yes | Yes | Yes | Yes | No—calculation stays Bray-Curtis |
| Binary distance | Yes | Yes | Yes | Control currently has no effect | Not used by fixed Bray-Curtis SIMPER |
| Blocking factor | Yes | Global test only | No blocking target | No | No |
| Permutation scheme | Yes | Global test only | Free and Series; Stratified is currently a no-op | No | No |
| Parallel option | Yes | Global test only | Yes | No | No |
| Pairwise output | Optional | Always, using free serial permutations | Optional | No | Group contrasts |
| Main plots | No | No | Distances to centre | Ordination and Shepard | Contributions |

Enabling **Parallel processing** can show that the option does not alter results or cause an error. It cannot prove that worker processes ran, because tofu may silently fall back to serial execution.

### Test PERMANOVA

**What this validates:** multivariate group comparison through `vegan::adonis2`, including the PERMANOVA table and shared preprocessing.

1. Open `tofu-small.csv`.
2. Select **Analyses → tofu → Compare groups → PERMANOVA**.
3. Move `feature_01`–`feature_08` to **Feature variables**.
4. Move `group` to **Grouping variable**. Leave **Additional factors**, **Strata / blocking factor**, and **Covariates (continuous)** empty.
5. Expand **Resemblance** and select:
   - **Transformation:** None
   - **Dissimilarity index:** Bray-Curtis
   - **Binary (presence/absence):** cleared
   - **Square-root distances:** cleared
   - **Additive constant:** None
   - **Random seed:** 123
6. Expand **PERMANOVA** and select:
   - **Permutations:** 999
   - **Permutation scheme:** Free
   - **Parallel processing:** cleared
   - **Test type:** Sequential terms
   - **Pairwise comparisons:** cleared

Expected **PERMANOVA Table** values for `group`:

| Pseudo-F | R² | p |
|---:|---:|---:|
| approximately 6.3747 | approximately 0.3778 | .001 |

The **F** and **p** cells for the `Residual` and `Total` rows should be blank because those statistics are not applicable to those rows.

Pass when **Data Summary**, **PERMANOVA Table**, and **Notes** appear, the values match above to the displayed precision, the inapplicable cells are blank, and **Notes and Warnings** is blank.

<details>
<summary>PERMANOVA functionality regression checks</summary>

- **Pairwise:** select **Pairwise comparisons** and leave **P-value adjustment** at **Holm**. A **Pairwise PERMANOVA** table should appear. Keep this as a single-factor model.
- **Additional factor:** move `treatment` to **Additional factors**. Confirm both model terms appear. Then select **Include group × additional factor interactions** and confirm the interaction row appears.
- **Covariates:** move `temperature` and `pH` to **Covariates (continuous)** and confirm both appear as model terms.
- **Blocked permutations:** move `block` to **Strata / blocking factor** and select **Stratified (within blocks)**.
- **Hellinger + Euclidean:** select **Transformation: Hellinger** and **Dissimilarity: Euclidean**. The group pseudo-F should be approximately 7.4512.
- **Presence/absence + Jaccard:** select **Transformation: Presence/absence**, **Dissimilarity: Jaccard**, and check **Binary**. The group pseudo-F should be approximately 1.0218 and p approximately .494.
- **Parallel-toggle invariance:** return to the baseline, record the table, select **Parallel processing**, and confirm the displayed statistics and p-value do not change.

</details>

For the large-data confirmation, open `tofu-large.csv`, assign `feature_01`–`feature_48`, repeat the baseline with **199 permutations**, and expect pseudo-F approximately **116.1245**, R² approximately **0.3941**, and p **.005**.

If the analysis fails, first confirm the feature columns are numeric, `group` is nominal, **Free** was selected, and the newly built module is loaded.

### Test ANOSIM

**What this validates:** the global rank-based group comparison and the separately calculated pairwise contrasts.

1. Open `tofu-small.csv`.
2. Select **Analyses → tofu → Compare groups → ANOSIM**.
3. Move `feature_01`–`feature_08` to **Feature variables** and `group` to **Grouping variable**. Leave **Strata / blocking factor** empty.
4. Under **Resemblance**, select **None**, **Bray-Curtis**, clear **Binary**, and enter seed **123**.
5. Under **ANOSIM**, enter **999** permutations, select **Free**, clear **Parallel processing**, and select **P-value adjustment: Holm**.

Expected results:

| Result | Value |
|---|---:|
| Global R | approximately 0.4950 |
| Global p | .001 |
| A–B R / adjusted p | approximately 0.6780 / .003 |
| A–C R / adjusted p | approximately 0.4314 / .003 |
| B–C R / adjusted p | approximately 0.3970 / .003 |

Pass when **Data Summary**, **Global Test**, **Pairwise ANOSIM**, and **Notes** appear, the values match, and **Notes and Warnings** is blank.

<details>
<summary>ANOSIM functionality regression checks</summary>

- Move `block` to **Strata / blocking factor** and select **Stratified (within blocks)**. The restriction applies to **Global Test** only.
- Pairwise ANOSIM currently uses free serial permutations even when the global test is blocked or parallel. Confirm only that Holm-adjusted pairwise output appears and matches its reference.
- For parallel-toggle invariance, compare **Global Test** with **Parallel processing** cleared and selected. Do not use pairwise output to judge the parallel control.

</details>

For `tofu-large.csv`, assign all 48 features, use **199 permutations**, and expect Global R approximately **0.8289**, global p **.005**, and adjusted pairwise p-values **.015**.

### Test PERMDISP

**What this validates:** distances to group centres, the dispersion permutation test, and its distance-to-centre plot.

1. Open `tofu-small.csv`.
2. Select **Analyses → tofu → Check dispersion → PERMDISP**.
3. Move `feature_01`–`feature_08` to **Feature variables** and `group` to **Grouping variable**.
4. Under **Resemblance**, select **None**, **Bray-Curtis**, clear **Binary** and **Square-root distances**, select **Additive constant: None**, and enter seed **123**.
5. Under **PERMDISP**, enter **999** permutations, select **Free**, clear **Parallel processing**, select **Dispersion centre: Median**, clear **Bias adjustment**, and clear **Pairwise comparisons**.

Expected results:

| Result | Value |
|---|---:|
| Dispersion F | approximately 1.6908 |
| p | approximately .219 |
| Mean distance A | approximately 0.1599 |
| Mean distance B | approximately 0.1631 |
| Mean distance C | approximately 0.2206 |

The **F** and **p** cells for the `Residuals` row should be blank because those statistics are not applicable to that row.

Pass when **Data Summary**, **Group Distances to Centre**, **Dispersion Test**, **Distances to Centre**, and **Notes** appear, the plot contains all three groups, the inapplicable cells are blank, and **Notes and Warnings** is blank.

<details>
<summary>PERMDISP functionality regression checks</summary>

- Select **Pairwise comparisons** with **P-value adjustment: Holm**. **Pairwise Dispersion** should appear.
- Change the centre to **Centroid** and select **Bias adjustment**. The dispersion F should be approximately **2.1561**.
- **Stratified** is currently a no-op because PERMDISP has no blocking target. With the same seed it should reproduce the Free baseline; record this as a current limitation, not restricted-permutation support.
- **Series** uses the current row order. Do not sort the fixture before an optional Series smoke check.
- Check **Square-root distances** and select **Cailliez** to confirm the distance-correction path completes.
- For parallel-toggle invariance, compare the baseline table before and after selecting **Parallel processing**.

</details>

For `tofu-large.csv`, use all 48 features and **199 permutations**. Expect F approximately **372.4867**, p **.005**, and mean distances A/B/C approximately **0.1583 / 0.1822 / 0.2472**.

### Test nMDS

**What this validates:** ordination, stress reporting, environmental fitting, group overlays, and the Shepard diagram.

1. Open `tofu-small.csv`.
2. Select **Analyses → tofu → Ordinate samples → nMDS**.
3. Move `feature_01`–`feature_08` to **Feature variables**, `group` to **Grouping variable**, and `temperature` plus `pH` to **Environmental variables**.
4. Under **Resemblance**, select **None**, **Bray-Curtis**, clear **Binary**, and enter seed **123**.
5. Under **nMDS**, enter **Dimensions: 2**, **Random starts: 20**, and **Max iterations per run: 200**. Select **Show Shepard diagram** and **Colour by group**. Clear **Show species scores**, **Group hulls**, **Group ellipses**, and **Group spiders**.

Expected results:

| Result | Value |
|---|---:|
| Stress | approximately 0.1636 |
| Temperature r² / p | approximately 0.2921 / .060 |
| pH r² / p | approximately 0.2376 / .100 |

Pass when **Data Summary**, **Ordination Plot**, **Environmental Fit**, **Stress and Convergence**, **Shepard Diagram**, and **Notes** appear. The ordination should contain 24 site points coloured by group. Axes and environmental vectors may rotate or reflect; coordinate signs are not pass criteria.

<details>
<summary>nMDS functionality regression checks</summary>

- Remove `group`, clear **Colour by group**, and confirm **Data Summary** says the grouping variable was not selected while the ordination still renders.
- Select **Show species scores**, **Group hulls**, **Group ellipses**, and **Group spiders** one at a time and confirm each requested ornament appears without an error.
- The **Binary** checkbox is currently ignored by the `metaMDS` calculation. With the same seed, selecting it should leave stress at approximately **0.1636**. Record this as a current limitation.
- Environmental-fit p-values use 99 permutations after the nMDS random starts; compare the displayed references rather than resetting or reproducing only the `envfit` seed separately.

</details>

For `tofu-large.csv`, use all 48 features with the same nMDS settings. Expect stress approximately **0.1844**, temperature r² approximately **0.3852**, and pH r² approximately **0.5291**. Confirm 360 site points and both plots appear; do not compare coordinate signs.

### Test SIMPER

**What this validates:** Bray-Curtis feature contributions, top-N and cumulative filtering, the contribution plot, and notes explaining current behaviour.

1. Open `tofu-small.csv`.
2. Select **Analyses → tofu → Explore feature contributions → SIMPER**.
3. Move `feature_01`–`feature_08` to **Feature variables** and `group` to **Grouping variable**.
4. Under **Resemblance**, select **None**, **Bray-Curtis**, clear **Binary**, and enter seed **123**.
5. Under **SIMPER**, enter **999** permutations, **Top N features: 10**, and **Cumulative contribution threshold (%): 70**.

Expected first rows and displayed row counts:

| Contrast | First feature | First contribution | Rows shown |
|---|---|---:|---:|
| A_B | `feature_04` | approximately 21.1956% | 4 |
| A_C | `feature_01` | approximately 26.3230% | 4 |
| B_C | `feature_04` | approximately 24.4285% | 4 |

Pass when **Data Summary**, **Feature Contributions**, **Contribution Plot**, and **Notes** appear. The note must say SIMPER uses Bray-Curtis and report 999 permutations.

<details>
<summary>SIMPER functionality regression checks</summary>

- Select **Square root** transformation. Contributions should change; for A_B the first contribution becomes approximately **18.9077%**.
- Select **Euclidean** and check **Binary**. Contributions should remain identical to the Bray-Curtis baseline, and **Notes** should explain that the selected dissimilarity is not used. This tests current behaviour rather than claiming Euclidean support.
- Change permutations to **19**. The contribution table remains unchanged because tofu does not display permutation p-values; only the permutation count in **Notes** changes.
- Change **Top N** and the cumulative threshold and confirm the displayed prefix changes. The current implementation may omit the feature that first crosses the threshold; compare it with the frozen current output rather than assuming the intended prefix.

</details>

For `tofu-large.csv`, use all 48 features and **199 permutations**. The first features for A_B, A_C, and B_C should be `feature_47`, `feature_45`, and `feature_45`, with first contributions approximately **6.5440%**, **6.8497%**, and **7.1003%**. Ten rows should appear for each contrast.

### Shared negative-input checks

Open `tofu-invalid.csv` and use PERMANOVA unless stated otherwise. Reopen the file before each check.

| Check | What to assign | Expected current behaviour |
|---|---|---|
| No features | `group` only | **Notes and Warnings:** “Select one or more feature variables.” |
| No group | `valid_01`, `valid_02`; no grouping variable | **Notes and Warnings:** “Select a primary grouping factor.” |
| Text feature | Try to move `text_feature` to **Feature variables** | jamovi refuses the transfer because the target permits numeric variables only |
| Negative abundance | `valid_01`, `negative_feature`; group `group` | warning identifies `negative_feature` |
| Missing abundance | `valid_01`, `missing_feature`; group `group` | warning reports one excluded row and the summary reports seven samples |
| All-zero feature | `valid_01`, `all_zero_feature`; group `group` | warning reports that the zero feature was removed |
| All-zero sample | `zero_case_01`, `zero_case_02`; group `group` | warning reports that one all-zero sample was removed |
| One group | `valid_01`, `valid_02`; group `single_group` | warning says the primary factor has fewer than two groups |

### Full large-data robustness pass

After the 199-permutation functionality checks pass, a release tester may repeat PERMANOVA, ANOSIM, and PERMDISP with **999 permutations** and the parallel option selected. Run nMDS and SIMPER with their documented large-data settings. Pass when every analysis completes, all required result sections render, expected current warnings are the only warnings, and jamovi remains usable. Record elapsed time only as diagnostic context; there is no timing threshold.

### Maintaining this guide

When tofu changes, use these sources together:

- menu paths: `jamovi/0000.yaml`
- option labels, choices, and defaults: `jamovi/*.a.yaml`
- visible panels and target boxes: `jamovi/*.u.yaml`
- result titles: `jamovi/*.r.yaml`
- wrapper regressions: `tests/testthat/test-workflows.R`
- independent scenarios and values: `tests/manual/scenarios.csv` and `tests/manual/reference-results.csv`
- reusable jamovi setups: `tests/manual/workbooks/*.omv`

Regenerate and verify the manual assets with:

```bash
Rscript tests/manual/generate-datasets.R --write
Rscript tests/manual/generate-reference-results.R --write
Rscript tests/manual/verify-fixtures.R
```

The generators call `vegan` directly and do not source tofu implementation files. See `tests/manual/reference-session-info.txt` for the recorded R, vegan, permute, RNG, and platform details.


## License

`tofu` is free software, released under the **GNU General Public License v2 or later** ([GPL-2.0-or-later](https://spdx.org/licenses/GPL-2.0-or-later.html)). The full terms are in [`LICENSE`](LICENSE).



## Acknowledgements

Two projects do the heavy lifting behind tofu:

- **[`vegan`](https://cran.r-project.org/package=vegan)**, the community-ecology methods, by Jari Oksanen and the `vegan` development team. Every number tofu reports comes out of `vegan`. If you use tofu in published work, please cite `vegan`; run `citation("vegan")` in R for the reference.
- **[jamovi](https://www.jamovi.org)**, the statistical platform tofu runs on.

`tofu` is developed at the University of Sydney.
