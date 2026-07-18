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
6. Open PERMANOVA, expand **Study design and model — use when part of your study design**, and confirm that it includes **Continuous covariates**. Open nMDS and confirm that it includes **Environmental variables**.

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

To assign one variable, drag it into the named target box, or select it and use the right-arrow. To assign all abundance columns, select `feature_01`, Shift-click the final feature, then transfer the selected range into **Feature variables**. Click a panel's chevron to expand a collapsed section. In PERMANOVA and ANOSIM, the two required targets stay visible; study-design and reproducibility controls are grouped separately.

jamovi recalculates automatically; there is no **Run** button. Wait until the spinner or progress message disappears and all named result sections have appeared. If you edit test data, reopen the clean CSV before starting another card.

### What tofu currently supports

This table describes current computation, including controls that are visible but limited or ignored.

| Capability | PERMANOVA | ANOSIM | PERMDISP | nMDS | SIMPER |
|---|---|---|---|---|---|
| Transformation | Yes | Yes | Yes | Yes | Yes |
| Selectable dissimilarity | Yes | Yes | Yes | Yes | No—calculation stays Bray-Curtis |
| Binary distance | Yes | Yes | Yes | Control currently has no effect | Not used by fixed Bray-Curtis SIMPER |
| Blocking factor | Yes | Yes; required for Within blocks | No blocking target | No | No |
| Permutation scheme | Yes | Free, Within blocks, or Series for global and pairwise tests | Free and Series; legacy no-block Stratified values migrate to Free with disclosure | No | No |
| Parallel option | Yes | Yes, for global and pairwise tests | Yes | No | No |
| Pairwise output | Optional | Optional | Optional | No | Group contrasts |
| Main plots | No | No | Distance distributions | Ordination and Shepard | Contributions |

Enabling **Parallel processing** can show that the option does not alter results or cause an error. It cannot prove that worker processes ran, because tofu may silently fall back to serial execution.

### Test PERMANOVA

**What this validates:** the student workflow, state-specific guidance, multivariate group comparison through `vegan::adonis2`, conditional Pairwise output, truthful permutation restrictions, and shared preprocessing.

1. Open `tofu-small.csv`.
2. Select **Analyses → tofu → PERMANOVA — Test group differences**.
3. Before assigning anything, confirm that **Getting started** says to add numeric Feature variables and one categorical Grouping variable. No empty result table should appear.
4. Move `feature_01`–`feature_08` to **Required: Feature variables**. Confirm that **Action needed** now asks for a Grouping variable and that no result table appears.
5. Move `group` to **Required: Grouping variable**.
6. Under **Analysis choices**, select:
   - **Transformation:** None
   - **Dissimilarity index:** Bray-Curtis
   - **Binary (presence/absence):** cleared
   - **Square-root distances:** cleared
   - **Additive constant:** None
   - **Pairwise comparisons:** cleared
7. Leave **Study design and model — use when part of your study design** collapsed. Its default is **Sequential terms** with **Free** permutation restrictions and no block, factor, covariate, or interaction.
8. Expand **Reproducibility and technical settings** and select:
   - **Number of permutations:** 999
   - **Random seed (0 = random):** 123
   - **Parallel processing:** cleared

Expected **PERMANOVA Table** values for `group`:

| Pseudo-F | R² | Permutation p |
|---:|---:|---:|
| approximately 6.3747 | approximately 0.3778 | .001 |

The **Pseudo-F** and **Permutation p** cells for the `Residual` and `Total` rows should be blank and identified as not applicable by the table note or footnote. `NaN` must not appear.

Pass when **Data Summary**, **PERMANOVA Table**, **Interpretation**, and **Analysis settings** appear; the values match above to the displayed precision; **Data handling warnings** and **Pairwise PERMANOVA** do not appear; and the settings report **Free**, no block, **Sequential terms**, seed 123, and serial execution.

<details>
<summary>PERMANOVA functionality regression checks</summary>

- **Pairwise dependency:** confirm **P-value adjustment** is disabled while **Pairwise comparisons** is cleared. Select **Pairwise comparisons**, leave the adjustment at **Holm**, and confirm that a populated **Pairwise PERMANOVA** table appears. Clear Pairwise and confirm that the table disappears rather than leaving an empty heading.
- **Additional factor and interactions:** expand **Study design and model — use when part of your study design**, move `treatment` to **Additional factors**, and confirm both model terms appear. Clear Pairwise, select **Include interactions**, and confirm the interaction row appears. Pairwise should be unavailable until interactions are cleared.
- **Covariates:** move `temperature` and `pH` to **Continuous covariates** and confirm both appear as model terms. Change **Test type** between **Sequential terms** and **Marginal terms** and confirm the Interpretation explains the selected model-term test.
- **Free with an assigned block:** move `block` to **Blocking variable** while leaving **Permutation restrictions: Free**. The main result should remain, **Data handling warnings** should say the block is unused, and **Analysis settings** should report **Block used: No**.
- **Within blocks without a block:** remove `block`, select **Within blocks — requires a Blocking variable**, and confirm that only the actionable correction appears, with no inferential table.
- **Blocked permutations:** assign `block` and keep **Within blocks — requires a Blocking variable**. The analysis should run and **Analysis settings** should report that the block is used.
- **State clearing:** from a valid result, remove `group`, confirm that all old result tables disappear and the Grouping-variable correction appears, then restore `group` and confirm a fresh valid result.
- **Active-section recovery:** close and reopen an analysis containing a block or non-default Test type, and confirm the study-design section opens so the active setting is not concealed.
- **Hellinger + Euclidean:** select **Transformation: Hellinger** and **Dissimilarity: Euclidean**. The group pseudo-F should be approximately 7.4512.
- **Presence/absence + Jaccard:** select **Transformation: Presence/absence**, **Dissimilarity: Jaccard**, and check **Binary**. The group pseudo-F should be approximately 1.0218 and p approximately .494.
- **Parallel-toggle invariance:** return to the baseline, record the table, select **Parallel processing**, and confirm the displayed statistics and p-value do not change.

</details>

For the large-data confirmation, open `tofu-large.csv`, assign `feature_01`–`feature_48`, repeat the baseline with **199 permutations**, and expect pseudo-F approximately **116.1245**, R² approximately **0.3941**, and p **.005**.

If the analysis fails, first confirm the feature columns are numeric, `group` is nominal, **Free** was selected unless a real block was assigned, and the newly built module is loaded.

### Test ANOSIM

**What this validates:** the student workflow, explicit incomplete states, global rank-based comparison, optional pairwise contrasts, and one truthful permutation design shared by both.

1. Open `tofu-small.csv`.
2. Select **Analyses → tofu → ANOSIM — Rank-based alternative**.
3. Before assigning anything, confirm that **Getting started** explains ANOSIM and names both required steps. No empty result table should appear.
4. Move `feature_01`–`feature_08` to **Feature variables (required)**. Confirm that **Action needed** asks for a Grouping variable and that no result table appears.
5. Move `group` to **Grouping variable (required)**.
6. Under **Analysis choices**, select **Transformation: None**, **Dissimilarity index: Bray-Curtis**, clear **Binary (presence/absence)**, and leave **Pairwise ANOSIM comparisons** cleared. **P-value adjustment** should be disabled.
7. Leave **Study design and permutation restrictions** collapsed. The default is **Free** with no Blocking variable.
8. Expand **Reproducibility and computation**, enter **999** permutations and seed **123**, and clear **Parallel processing**.

Expected baseline result:

| Result | Value |
|---|---:|
| Global R | approximately 0.4950 |
| Permutation p | .001 |

Pass when **Data Summary**, **Global ANOSIM**, **Interpretation**, and **Analysis settings** appear; the values match; **Data handling warnings** and **Pairwise ANOSIM** do not appear; and settings report Free permutations, no block, seed 123, Pairwise disabled, and serial execution.

<details>
<summary>ANOSIM functionality regression checks</summary>

- **Pairwise dependency:** select **Pairwise ANOSIM comparisons**, leave **P-value adjustment: Holm**, and confirm a populated **Pairwise ANOSIM** table appears. Expected R / adjusted p values are A vs B approximately 0.6780 / .003, A vs C approximately 0.4314 / .003, and B vs C approximately 0.3970 / .003. Clear Pairwise and confirm the heading and rows disappear.
- **Free with an assigned block:** expand **Study design and permutation restrictions**, move `block` to **Blocking variable (optional)**, and leave **Permutation restriction: Free**. The global result should remain, **Data handling warnings** should say the block is not used, and **Analysis settings** should report **Block used: No**.
- **Within blocks without a block:** remove `block`, select **Within blocks**, and confirm that only the Blocking-variable correction appears, with no inferential table.
- **Blocked global and pairwise tests:** reassign `block`, keep **Within blocks**, and select Pairwise. Settings should report **Block used: Yes**. The R values stay as above for this fixture, but each Holm-adjusted pairwise p is .006 because the same within-block restriction now applies to every contrast.
- **Series:** select **Series (rows in order)**. Settings should identify the current row order, and, when a block is assigned, the current row order within blocks. Do not sort the data between recording the design and running the test.
- **Two groups:** filter or recode the data to retain two groups, request Pairwise, and confirm that no empty pairwise table appears. Interpretation should explain that Global ANOSIM is the only contrast.
- **State clearing:** from a valid result, remove `group`, confirm that every old table disappears and the Grouping-variable correction appears, then restore `group` and confirm a fresh result.
- **Negative R guidance:** use a dataset that produces a negative R and confirm Interpretation explains that within-group observations are ranked as more dissimilar on average; it should not present negative R as a software error.
- **Parallel-toggle invariance:** compare the global and requested pairwise results with **Parallel processing** cleared and selected. Statistics and p values should not change; settings must disclose whether execution was effectively serial or parallel.

</details>

For `tofu-large.csv`, assign all 48 features and use **199 permutations**. Expect Global R approximately **0.8289** and Permutation p **.005**. Then select Pairwise and expect adjusted p **.015** for all three contrasts, with R approximately 0.9852, 0.7891, and 0.8187 for A vs B, A vs C, and B vs C respectively.

### Test PERMDISP

**What this validates:** the required-input guidance, distances to group centres, the dispersion permutation test, its accessible distribution summary and plot, and conditional pairwise output.

1. Open `tofu-small.csv`.
2. Select **Analyses → tofu → PERMDISP — Check group dispersion**. Before assigning variables, only **Getting started** should appear in the report.
3. Move `feature_01`–`feature_08` to **Feature variables (required)** and `group` to **Grouping variable (required)**.
4. Under **Analysis choices**, select **Transformation: None**, **Dissimilarity index: Bray-Curtis**, **Group centre: Median**, and clear **Pairwise dispersion comparisons**. **P-value adjustment** should be disabled.
5. Leave **Advanced options** at its defaults: Binary off, Square-root distances off, Additive constant None, and Bias adjustment off.
6. Under **Reproducibility and computation**, enter **999** permutations, select **Free**, enter seed **123**, and clear **Parallel processing**.

Expected results:

| Result | Value |
|---|---:|
| Dispersion F | approximately 1.6908 |
| Permutation p | approximately .219 |
| Mean distance A | approximately 0.1599 |
| Mean distance B | approximately 0.1631 |
| Mean distance C | approximately 0.2206 |

The **F** and **Permutation p** cells for the `Residuals` row should be blank because those statistics are not applicable to that row. Fail if `NaN` appears.

Pass when **Data Summary**, **Distances to Group Centre**, **Dispersion Test**, **Distance Distributions**, **Interpretation**, and **Analysis settings** appear. The distance table should contain n, mean, median, standard deviation, minimum, and maximum for all three groups. The plot should contain all three groups. **Data handling warnings** and **Pairwise Dispersion Comparisons** should be absent, not blank.

<details>
<summary>PERMDISP functionality regression checks</summary>

- Select **Pairwise dispersion comparisons** with **P-value adjustment: Holm**. **Pairwise Dispersion Comparisons** should appear with t, Permutation p, and Adjusted p.
- Change the centre to **Centroid** and select **Bias adjustment**. The dispersion F should be approximately **2.1561**.
- A copied legacy analysis containing **Stratified** should reproduce the Free baseline, show the compatibility conversion under **Data handling warnings**, and report requested/effective restrictions in **Analysis settings**. Stratified must not appear as a choice in a new analysis.
- **Series (rows in order)** uses the current row order. Do not sort the fixture before an optional Series smoke check.
- Under **Advanced options**, check **Square-root distances** and select **Cailliez** to confirm the distance-correction path completes.
- For parallel-toggle invariance, compare the baseline table before and after selecting **Parallel processing**.
- Remove the Grouping variable after a valid run. Only **Action needed** should remain; restoring it should recreate the results without stale pairwise rows.

</details>

For `tofu-large.csv`, use all 48 features and **199 permutations**. Expect F approximately **372.4867**, p **.005**, and mean distances A/B/C approximately **0.1583 / 0.1822 / 0.2472**.

### Test nMDS

**What this validates:** ordination, stress reporting, environmental fitting, group overlays, and the Shepard diagram.

1. Open `tofu-small.csv`.
2. Select **Analyses → tofu → nMDS — Visualise sample patterns**.
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
2. Select **Analyses → tofu → SIMPER — Feature contributions**.
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
| No features | `group` only | **Getting started** shows both required steps; no empty result table |
| No group | `valid_01`, `valid_02`; no grouping variable | **Action needed** asks for one categorical Grouping variable; no empty result table |
| Text feature | Try to move `text_feature` to **Feature variables** | jamovi refuses the transfer because the target permits numeric variables only |
| Negative abundance | `valid_01`, `negative_feature`; group `group` | **Action needed** identifies `negative_feature`; no inferential table |
| Missing abundance | `valid_01`, `missing_feature`; group `group` | warning reports one excluded row and the summary reports seven samples |
| All-zero feature | `valid_01`, `all_zero_feature`; group `group` | warning reports that the zero feature was removed |
| All-zero sample | `zero_case_01`, `zero_case_02`; group `group` | warning reports that one all-zero sample was removed |
| One group | `valid_01`, `valid_02`; group `single_group` | **Action needed** says the Grouping variable has fewer than two groups; no inferential table |

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
