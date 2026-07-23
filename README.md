# tofu

`tofu` brings community-ecology methods from R into [jamovi](https://www.jamovi.org). It provides seven focused analyses—PERMANOVA, PERMDISP, ANOSIM, SIMPER, nMDS, hierarchical cluster analysis, and PCoA—so you can analyse abundance-style data without leaving the spreadsheet. Its bounded `ggplot2` figures use a consistent scientific style suitable for teaching and publication-ready workflows.

Not all features are included as we have deliberately ported only the most commonly used techniques for teaching purposes. If you need additional functionality for teaching, let us know. Otherwise, please use `vegan` directly.

`tofu` supports the following analyses:

| Analysis | vegan function | What it does |
|---|---|---|
| PERMANOVA | `vegan::adonis2` | Compare groups with permutation tests |
| ANOSIM | `vegan::anosim` | Rank-based group comparison |
| PERMDISP | `vegan::betadisper` | Check multivariate dispersion, with a distance-to-centre plot |
| nMDS | `vegan::metaMDS` | Ordinate samples, with stress and Shepard plots |
| Cluster analysis | `vegan::vegdist` and `stats::hclust` | Display sample similarity as a group-average dendrogram |
| SIMPER | `vegan::simper` | Summarise feature contributions to average Bray-Curtis dissimilarity |
| PCoA | `vegan::wcmdscale` | Visualise distance structure among sites |

## Installation

`tofu` is being prepared for submission to the jamovi module library. Until it
is listed, install it from a source checkout:

```r
install.packages("jmvtools") # first time only
jmvtools::install(pkg = ".")
```

Testers can also side-load a compatible `.jmo` file through **Modules →
Side-load**. A `.jmo` build is specific to its operating system, processor
architecture, and jamovi series.

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

5. Relaunch jamovi. Open **Analyses → tofu** and confirm that seven items are present: PERMANOVA, ANOSIM, PERMDISP, SIMPER, nMDS, Cluster analysis, and PCoA.
6. Open PERMANOVA and confirm **Plots → Show companion PCoA** is available. Open PERMDISP and confirm **Distance-to-centre diagnostic** is selected by default. Open Cluster analysis and confirm **Define clusters** is optional. Open PCoA and confirm it includes **Feature variables (required)**, **Grouping variable (optional)**, **Plots**, and **Advanced corrections**.

Record the operating system plus the jamovi and tofu versions with your test notes. The version alone is not proof that the new build loaded because two local builds may share a version number. If the menu or controls do not match this guide, remove tofu, fully quit jamovi, rebuild from the confirmed path, and relaunch before investigating the analysis.

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
- [`tests/manual/workbooks/tofu-small-baselines.omv`](tests/manual/workbooks/tofu-small-baselines.omv): the small dataset with PERMANOVA, ANOSIM, PERMDISP, SIMPER, and nMDS configured.
- [`tests/manual/workbooks/tofu-large-baselines.omv`](tests/manual/workbooks/tofu-large-baselines.omv): the large dataset with PERMANOVA, ANOSIM, PERMDISP, SIMPER, and nMDS configured.

To open a file, select **File (☰) → Open → This PC → Browse**, navigate to `tests/manual`, and select the CSV.

For a faster repeat run, open one of the `.omv` workbooks instead. Install the current tofu build first, then force each saved analysis to recalculate by changing a documented control and restoring it. Do not judge a new build from cached workbook results. Cluster analysis and PCoA are not saved in these workbooks, so create them from the CSV. These repository-only workbooks are excluded from the installed module.

Before analysing either clean dataset, open each column's **Setup** and confirm:

| Columns | Data type | Measurement level |
|---|---|---|
| `sample_id` | Text | ID |
| `group`, `treatment`, `block` | Text | Nominal |
| `temperature`, `pH` | Decimal | Continuous |
| `feature_01` through the final feature | Decimal | Continuous |

To assign one variable, drag it into the named target box, or select it and use the right-arrow. To assign all abundance columns, select `feature_01`, Shift-click the final feature, then transfer the selected range into **Feature variables (required)**. Click a panel's chevron to expand a collapsed section. In PERMANOVA and ANOSIM, the two required targets stay visible; study-design and reproducibility controls are grouped separately.

jamovi recalculates automatically; there is no **Run** button. Wait until the spinner or progress message disappears and all named result sections have appeared. If you edit test data, reopen the clean CSV before starting another card.

### What tofu currently supports

This table describes current computation, including controls that are visible but limited or ignored.

| Capability | PERMANOVA | ANOSIM | PERMDISP | nMDS | Cluster | SIMPER | PCoA |
|---|---|---|---|---|---|---|---|
| Transformation | Yes | Yes | Yes | Yes | Yes | Yes | Yes |
| Selectable dissimilarity | Yes | Yes | Yes | Yes | Yes | No—calculation stays Bray-Curtis | Yes |
| Binary distance | Yes | Yes | Yes | Legacy saved analyses only; hidden for new analyses | No | Not available; SIMPER is fixed to Bray-Curtis | Yes |
| Blocking factor | Yes | Yes; required for Within blocks | No blocking target | No | No | No | No |
| Permutation scheme | Yes | Free, Within blocks, or Series for global and pairwise tests | Free and Series; legacy no-block Stratified values migrate to Free with disclosure | No | No | No | No |
| Parallel option | Yes | Yes, for global and pairwise tests | Yes | No | No | No | No |
| Pairwise output | Optional | Optional | Optional | No | No | Group contrasts | No |
| Main plots | Optional companion PCoA | Ranked-dissimilarity diagnostic | Distance-to-centre diagnostic; optional ordination | Ordination and Shepard diagnostic | Dendrogram; optional cut | Separate contribution plots; optional heatmap | Principal coordinates ordination |

Enabling **Parallel processing** can show that the option does not alter results or cause an error. It cannot prove that worker processes ran, because tofu may silently fall back to serial execution.

### Test PERMANOVA

**What this validates:** the student workflow, state-specific guidance, multivariate group comparison through `vegan::adonis2`, conditional Pairwise output, truthful permutation restrictions, shared preprocessing, and the optional descriptive PCoA.

1. Open `tofu-small.csv`.
2. Select **Analyses → tofu → PERMANOVA — Test group differences**.
3. Before assigning anything, confirm that **Getting started** says to add numeric Feature variables and one categorical Grouping variable. No empty result table should appear.
4. Move `feature_01`–`feature_08` to **Feature variables (required)**. Confirm that **Action needed** now asks for a Grouping variable and that no result table appears.
5. Move `group` to **Grouping variable (required)**.
6. Under **Analysis choices**, select:
   - **Transformation:** None
   - **Dissimilarity index:** Bray-Curtis
   - **Binary (presence/absence):** cleared
   - **Square-root distances:** cleared
   - **Additive constant:** None
   - **Pairwise comparisons:** cleared
7. Leave **Study design and model** collapsed. Its default is **Sequential terms** with **Free** permutation restrictions and no block, factor, covariate, or interaction.
8. Expand **Reproducibility and technical settings** and select:
   - **Number of permutations:** 999
   - **Random seed (0 = random):** 123
   - **Parallel processing:** cleared

Expected **PERMANOVA Table** values for `group`:

| Pseudo-F | R² | Permutation p |
|---:|---:|---:|
| approximately 6.3747 | approximately 0.3778 | .001 |

The **Pseudo-F** and **Permutation p** cells for the `Residual` and `Total` rows should be plain blanks without superscripts or explanatory notes. `NaN` must not appear.

Pass when **Data Summary**, **PERMANOVA Table**, **How to read these results**, and **Analysis settings** appear; the values match above to the displayed precision; **Data handling warnings** and **Pairwise PERMANOVA** do not appear; and the settings report **Free**, no block, **Sequential terms**, seed 123, and serial execution.

<details>
<summary>PERMANOVA functionality regression checks</summary>

- **Pairwise dependency:** confirm **P-value adjustment** is disabled while **Pairwise comparisons** is cleared. Select **Pairwise comparisons**, leave the adjustment at **Holm**, and confirm that a populated **Pairwise PERMANOVA** table appears. Clear Pairwise and confirm that the table disappears rather than leaving an empty heading.
- **Additional factor and interactions:** expand **Study design and model**, move `treatment` to **Additional factors (optional)**, and confirm both model terms appear. Clear Pairwise, select **Include interactions**, and confirm the interaction row appears. Pairwise should be unavailable until interactions are cleared.
- **Covariates:** move `temperature` and `pH` to **Continuous covariates (optional)** and confirm both appear as model terms. Change **Test type** between **Sequential terms** and **Marginal terms** and confirm **How to read these results** explains the selected model-term test.
- **Free with an assigned block:** move `block` to **Blocking variable (optional)** while leaving **Permutation restrictions: Free**. The main result should remain, **Data handling warnings** should say the block is unused, and **Analysis settings** should report **Block used: No**.
- **Within blocks without a block:** remove `block`, select **Within blocks — requires a Blocking variable**, and confirm that only the actionable correction appears, with no inferential table.
- **Blocked permutations:** assign `block` and keep **Within blocks — requires a Blocking variable**. The analysis should run and **Analysis settings** should report that the block is used.
- **State clearing:** from a valid result, remove `group`, confirm that all old result tables disappear and the Grouping-variable correction appears, then restore `group` and confirm a fresh valid result.
- **Active-section recovery:** close and reopen an analysis containing a block or non-default Test type, and confirm the study-design section opens so the active setting is not concealed.
- **Hellinger + Euclidean:** select **Transformation: Hellinger** and **Dissimilarity: Euclidean**. The group pseudo-F should be approximately 7.4512.
- **Presence/absence + Jaccard:** select **Transformation: Presence/absence**, **Dissimilarity: Jaccard**, and check **Binary**. The group pseudo-F should be approximately 1.0218 and p approximately .494.
- **Parallel-toggle invariance:** return to the baseline, record the table, select **Parallel processing**, and confirm the displayed statistics and p-value do not change.
- **Companion PCoA:** expand **Plots** and select **Show companion PCoA**. With the simple baseline model, `group` should be selected automatically. Confirm **PERMANOVA companion PCoA**, **Companion PCoA description**, and **Companion PCoA Site Coordinates** appear. Select **Show group centroids** and **Connect sites to centroids** and confirm the description names the effective layers. Add `treatment` to **Additional factors (optional)**, choose each **Model factor**, and confirm the display changes without changing the PERMANOVA table. Clear **Show companion PCoA** and confirm every companion section disappears. This plot describes the fitted resemblance structure; it is not another test of significance.

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
7. Leave **Study design and permutation restrictions** collapsed. The default is **Free**, with **Blocking variable (optional)** empty.
8. Expand **Reproducibility and computation**, enter **999** permutations and seed **123**, and clear **Parallel processing**.

Expected baseline result:

| Result | Value |
|---|---:|
| Global R | approximately 0.4950 |
| Permutation p | .001 |

Pass when **Data Summary**, **Global ANOSIM**, **Ranked dissimilarities by pair category**, **Plot details**, **Ranked-dissimilarity summary**, **Interpretation**, and **Analysis settings** appear; the values match; **Data handling warnings** and **Pairwise ANOSIM** do not appear; and settings report Free permutations, no block, seed 123, Pairwise disabled, and serial execution. The plot and summary diagnose the rank distributions behind R; they do not add a second hypothesis test.

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
- **Rank diagnostic toggle:** clear **Ranked-dissimilarity diagnostic**. The image and **Plot details** should disappear while **Ranked-dissimilarity summary** and every inferential result remain unchanged. Restore it and confirm the description reports how many pairwise ranks are shown.

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
- Under **Plots**, confirm **Distance-to-centre diagnostic** is selected and **Ordination with group centres** is cleared. The first image must be followed by **Plot details** and the full-data **Distance-to-centre summary**. Select the optional ordination and confirm **Ordination with group centres**, **Ordination details**, **Ordination coordinates**, and **Plot key** appear. Lines connect sites to fitted centres; no ellipse should appear. Clear each plot independently and confirm its table alternative remains while F and p do not change.
- For parallel-toggle invariance, compare the baseline table before and after selecting **Parallel processing**.
- Remove the Grouping variable after a valid run. Only **Action needed** should remain; restoring it should recreate the results without stale pairwise rows.

</details>

For `tofu-large.csv`, use all 48 features and **199 permutations**. Expect F approximately **372.4867**, p **.005**, and mean distances A/B/C approximately **0.1583 / 0.1822 / 0.2472**.

### Test nMDS

**What this validates:** ordination, stress reporting, environmental fitting, group overlays, and the Shepard diagram.

1. Open `tofu-small.csv`.
2. Select **Analyses → tofu → nMDS — Visualise sample patterns**.
3. Move `feature_01`–`feature_08` to **Feature variables (required)**, `group` to **Grouping variable (optional)**, and `temperature` plus `pH` to **Environmental variables (optional)**.
4. In **Analysis choices**, select **None** and **Bray-Curtis**. New analyses use two dimensions.
5. In **Output choices**, select **Show Shepard diagram** and clear **Show feature scores**.
6. In **Group display**, select **Style points by group** and clear **Group hulls**, **Group dispersion ellipses (1 SD)**, and **Group spiders**.
7. In **Environmental fit**, enter **99** permutations.
8. In **Reproducibility**, enter seed **123**, **20** maximum random starts, and **200** maximum iterations per run.

Expected results:

| Result | Expected display |
|---|---:|
| Stress | approximately 0.1636 |
| Temperature r² / unadjusted permutation p | approximately 0.292 / .060 |
| pH r² / unadjusted permutation p | approximately 0.238 / .100 |
| Site Score rows | 24 |

Pass when **Data Summary**, **Two-dimensional nMDS ordination**, **Stress and convergence diagnostics**, **Shepard diagram**, **Environmental Fit**, **Site Scores**, **Interpretation**, and **Analysis settings** appear. The ordination should contain 24 site points styled by group and two environmental vectors. Axes and vectors may rotate or reflect; coordinate signs are not pass criteria.

<details>
<summary>nMDS functionality regression checks</summary>

- Change seed **123 → 124 → 123**. Confirm **Analysis settings** follows each value and stress returns to approximately **0.1636**.
- Remove `group`. The ordination, 24-row **Site Scores**, and **Environmental Fit** must remain; group styling controls must become unavailable. Restore `group` and confirm the base configuration and stress do not change.
- Remove `pH`, then remove all environmental variables. **Environmental Fit** and the vectors should follow the requested variables while the base ordination remains. Restore `temperature` and `pH`.
- Select **Show feature scores** and expect 8 rows. Clear and restore **Show Shepard diagram**. Neither output-only change should alter **Site Scores**.
- With **Show Shepard diagram** selected, confirm **Shepard diagram description** and **Pairs shown in the Shepard diagram** follow the image. The table must report the plotted dissimilarity, ordination distance, and monotone fitted distance for the displayed pairs; toggling the Shepard output must not alter stress or site coordinates.
- Select the hull, 1-SD ellipse, and spider overlays. The ordination description must name every effective layer; **Interpretation** must state that group displays are descriptive and ellipses are not confidence regions.
- Change environmental-fit permutations to **19**. Temperature should display p **.050** and pH **.250**. Restore **99**.
- Select **Standardize** while retaining **Bray-Curtis**. An actionable compatibility message must replace the stale ordination and inferential output. Select **Euclidean** to recover, then restore **None** and **Bray-Curtis**.
- Remove every required feature. Only the getting-started guidance should remain in the nMDS report. Restore the eight features and confirm a fresh result returns.
- At 200% jamovi zoom, confirm the required and optional labels, the compact tips in **Group display** and **Environmental fit**, all controls, and the report remain readable and reachable. Restore 100%.
- Use Tab and Shift-Tab across the three reproducibility fields. After changing the seed, focus must remain in the options rather than jumping into the report.
- Binary and 3D options are retained only for compatibility with saved legacy analyses and are not visible in a new analysis.

</details>

For `tofu-large.csv`, use all 48 features with the same settings. Expect stress approximately **0.1844**, temperature r² approximately **0.385** with p **.010**, and pH r² approximately **0.529** with p **.010**. Confirm 360 **Site Scores**, 48 **Feature Scores** when requested, both plots, and two environmental vectors. With all three descriptive group overlays selected, the site configuration must remain unchanged; do not compare coordinate signs.

### Test cluster analysis

**What this validates:** Bray-Curtis calculation, group-average hierarchical clustering, sample labels, and bounded dendrogram output.

1. Open `tofu-small.csv`.
2. Select **Analyses → tofu → Cluster analysis — Visualise sample similarity**.
3. Before assigning anything, confirm that **Getting started** asks for numeric Feature variables and no empty plot or table appears.
4. Move `feature_01`–`feature_08` to **Feature variables (required)** and `sample_id` to **Sample labels (optional)**.
5. Under **Analysis choices**, select **Transformation: None** and **Dissimilarity index: Bray-Curtis**.
6. Under **Output choices**, select **Show sample labels**.

Expected results:

- **Data summary** reports 24 samples and 8 feature variables.
- **Analysis settings** reports **Group average (UPGMA)** linkage and `sample_id` as the label source.
- The lowest join in the dendrogram connects `S002` and `S023` at Bray-Curtis dissimilarity approximately **0.108**.
- **How to read this dendrogram** explains lower branch heights without presenting the clustering as a hypothesis test.

Pass when the dendrogram is populated, all 24 sample labels are contained within the plot, **Data handling warnings** is absent, and no resemblance matrix or empty result section appears.

<details>
<summary>Cluster-analysis functionality regression checks</summary>

- Change **Sample label display** between Automatic, Show, and Hide. Label display should not change branches or heights; restore Automatic.
- Select **Define clusters** and leave **Define clusters by: Number of clusters**, then enter **3**. Confirm a visible cut, a populated **Cluster membership** table, and cluster identities in **Dendrogram structure**. Change to **Dissimilarity height**, enter **0.5**, and confirm the displayed cut and memberships agree with that height. If the height ties a merge, the plot description must disclose the effective boundary. Clear **Define clusters** and confirm the membership table and cut disappear while the uncut dendrogram and structure table remain.
- Change the transformation to **Fourth root**. The dendrogram should recalculate and **Analysis settings** should report Fourth root; restore None.
- Remove every required feature after a valid run. Only the getting-started guidance should remain; restoring the features should create a fresh dendrogram without stale output.
- If a label column contains blanks or duplicate values, confirm that **Data handling warnings** explains the row-number fallback or disambiguation.
- Long labels should end with an ellipsis inside the plot, accompanied by a warning that the source data are unchanged.

</details>

For `tofu-large.csv`, use all 48 features and clear **Show sample labels**. **Data summary** should report 360 samples and 48 features, and the dendrogram should render without a report-width overflow. Selecting **Show sample labels** should give a crowding warning rather than failing.

### Test SIMPER

**What this validates:** Bray-Curtis feature contributions, compact output, top-N and cumulative filtering, optional details, and the exploratory permutation assessment.

1. Open `tofu-small.csv`.
2. Select **Analyses → tofu → SIMPER — Feature contributions**.
3. Move `feature_01`–`feature_08` to **Feature variables (required)** and `group` to **Grouping variable (required)**.
4. In **Analysis choices**, select transformation **None**. Read the adjacent **Tip**: transformations affect contributions and the displayed means use transformed values. SIMPER is fixed to Bray-Curtis.
5. Under **Features shown**, enter **Top N features: 10** and **Cumulative contribution (%): 70**. Clear **Show detailed statistics**.
6. Leave **Permutation assessment (advanced)** off for the descriptive baseline.

Expected first rows and displayed row counts:

| Contrast | First feature | First contribution | Rows shown |
|---|---|---:|---:|
| A vs B | `feature_04` | approximately 21.20% | 5 |
| A vs C | `feature_01` | approximately 26.32% | 5 |
| B vs C | `feature_04` | approximately 24.43% | 5 |

Pass when **Data Summary**, **Contrast summary**, the four-column **Descriptive feature contributions** table, **Contribution plots by contrast**, one separately titled **Contrast contribution** image per contrast, each image's **Plot details** and **Values shown in this plot**, **Interpretation**, and **Analysis settings** appear. The main table columns are **Contrast**, **Feature**, **Contribution (%)**, and **Cumulative (%)**; detailed statistics, the heatmap, and the permutation assessment should not occupy blank report space when they are not selected.

<details>
<summary>SIMPER functionality regression checks</summary>

- Change **Top N features** from **10 → 9**. Confirm recalculation and that each contrast stops at Top N or the cumulative threshold, while retaining the feature that crosses the threshold.
- Select **Show detailed statistics**. **Contribution variability** must contain five columns and **Group means** four columns; both must fit the report width. Clear the option and confirm both tables disappear.
- Select **Contrast overview heatmap**. Confirm the bounded **Contrast overview heatmap**, **Heatmap details**, and **Values shown in the heatmap** appear, then clear it and confirm all three disappear. The separate contrast figures and heatmap are descriptive contribution summaries, not evidence that a feature caused the groups to differ.
- Select **Square root**. Contributions and group means should change; for A vs B the first contribution becomes approximately **18.91%**. Restore **None**.
- Select **Assess contributions with permutations**. The permutations, adjustment, and seed controls must become available. Enter **19**, retain **Holm**, and enter seed **123**; expect a populated four-column **Exploratory permutation assessment**. Clear the assessment and confirm both its table and controls return to the inactive state.
- Remove `group`. All previous SIMPER tables, plot, and assessment must disappear and the guidance must ask for the required grouping variable. Restore `group` and confirm all three contrasts are recalculated without stale rows.
- At 200% jamovi zoom, confirm the options, four-column main table, and optional detail tables remain readable and reachable. Restore 100%.
- Fail if `NaN` or `Inf` appears in any visible result.

</details>

For `tofu-large.csv`, use all 48 features, **Top N features: 9**, and the 70% cumulative threshold. **Data Summary** should report 360 samples, 48 features, three groups, and three contrasts. Exactly nine contribution rows should appear per contrast. The first features for A vs B, A vs C, and B vs C should be `feature_47`, `feature_45`, and `feature_45`, with first contributions approximately **6.54%**, **6.85%**, and **7.10%**.

### Test PCoA

**What this validates:** descriptive principal coordinates analysis through `vegan::wcmdscale`, the exact resemblance preprocessing, eigenvalue accounting, accessible plot alternatives, corrections, and optional grouping overlays. PCoA has no significance test: pass or fail is determined by the stated coordinates, eigenvalues, sections, and interface states—not by separation between groups.

1. Open `tofu-small.csv`.
2. Select **Analyses → tofu → PCoA — Visualise distance structure**.
3. Before assigning variables, confirm **Getting started** asks for two or more numeric Feature variables and no empty image or table appears.
4. Move `feature_01`–`feature_08` to **Feature variables (required)**. Leave **Grouping variable (optional)** empty initially.
5. Under **Analysis choices**, select **Transformation: None**, **Dissimilarity index: Bray-Curtis**, and clear **Binary (presence/absence)**.
6. Under **Plots**, leave **Show group centroids** and **Connect sites to centroids** cleared. Both controls should be unavailable until a grouping variable is assigned.
7. Leave **Advanced corrections** at **Square-root distances: cleared** and **Additive correction: None**.

Expected direct-`vegan` checkpoints:

| Result | Small-data value |
|---|---:|
| Samples used | 24 |
| PCoA1 eigenvalue | approximately 0.57635 |
| PCoA2 eigenvalue | approximately 0.37191 |
| PCoA1 explained, positive-sum basis | approximately 34.5553% |
| PCoA2 explained, positive-sum basis | approximately 22.2979% |
| Absolute first-site PCoA1 / PCoA2 | approximately 0.04467 / 0.09797 |
| Negative eigenvalues: count / sum | 10 / approximately −0.20060 |

Pass when **Data Summary**, **Principal coordinates ordination**, **Ordination description**, **Site Coordinates**, **Eigenvalues**, **Interpretation**, and **Settings** appear. The image must have equal physical axis scaling and remain inside the report width. The description must state the two explained percentages, negative-eigenvalue handling, effective layers, and that the ordination is descriptive. Coordinate signs may reverse, so compare their absolute values or reflect the whole axis consistently.

<details>
<summary>PCoA functionality regression checks</summary>

- **Direct vegan parity:** in R, read the same CSV, select the feature columns, calculate `d <- vegan::vegdist(features, method = "bray")`, then run `fit <- vegan::wcmdscale(d, k = nrow(features) - 1, eig = TRUE, add = FALSE, x.ret = TRUE)`. Compare the first two positive `fit$eig` values, percentages calculated against `sum(fit$eig[fit$eig > 0])`, the negative count and sum, and `abs(fit$points[1, 1:2])` with the checkpoints above. Do not use tofu's internal PCoA helper to create the reference.
- **Grouping overlays:** move `group` to **Grouping variable (optional)**. Confirm points use both colour and shape and full group identities remain in **Site Coordinates**. Select **Show group centroids**, then **Connect sites to centroids**. Confirm **Group Centroids** appears with three groups and n = 8 each, the description names both layers, and selecting spiders also draws centroids. Clearing both controls must leave site coordinates and eigenvalues unchanged.
- **Corrections:** under **Advanced corrections**, select **Lingoes**, then **Cailliez**. Each run must report its correction and constant in **Settings**, update coordinates/eigenvalues, and complete without `NaN` or `Inf`. Restore None. Separately select **Square-root distances** and confirm the description and settings disclose it; restore the default.
- **One positive axis:** in a disposable copy of the data, create two continuous columns containing proportional values such as `0,1,2,3,4` and `0,2,4,6,8`. Assign only those columns, select Euclidean, and leave grouping empty. Confirm **Site Coordinates** and **Eigenvalues** remain, the description explains that only one positive axis is available, and no blank ordination image appears.
- **Invalid-state clearing:** from a valid result, remove every Feature variable. Confirm all previous PCoA images and tables disappear and only **Getting started** remains. Restore the eight features and confirm fresh results return.

</details>

For `tofu-large.csv`, assign all 48 features and repeat the defaults. Expect 360 samples, PCoA1/PCoA2 eigenvalues approximately **5.29746 / 5.09098**, explained percentages approximately **16.9022% / 16.2434%**, absolute first-site coordinates approximately **0.14261 / 0.08661**, and 210 negative eigenvalues summing to approximately **−7.30426**. Add `group`, centroids, and spiders. Pass when the plot is bounded, tables retain all 360 sites, any plotted-point limit is disclosed, and neither the options nor report requires horizontal scrolling.

### Shared negative-input checks

Open `tofu-invalid.csv` and use PERMANOVA unless stated otherwise. Reopen the file before each check. Before the all-zero-feature check, set `all_zero_feature` to **Continuous** under **Variables → Edit**; jamovi imports this constant column as Nominal by default.

| Check | What to assign | Expected current behaviour |
|---|---|---|
| No features | `group` only | **Getting started** shows both required steps; no empty result table |
| No group | `valid_01`, `valid_02`; no grouping variable | **Action needed** asks for one categorical Grouping variable; no empty result table |
| Text feature | Try to move `text_feature` to **Feature variables (required)** | jamovi refuses the transfer because the target permits numeric variables only |
| Negative abundance | `valid_01`, `negative_feature`; group `group` | **Action needed** identifies `negative_feature`; no inferential table |
| Missing abundance | `valid_01`, `missing_feature`; group `group` | warning reports one excluded row and the summary reports seven samples |
| All-zero feature | `valid_01`, `all_zero_feature`; group `group` | warning reports that the zero feature was removed |
| All-zero sample | `zero_case_01`, `zero_case_02`; group `group` | warning reports that one all-zero sample was removed |
| One group | `valid_01`, `valid_02`; group `single_group` | **Action needed** says the Grouping variable has fewer than two groups; no inferential table |

### Full large-data robustness pass

After the 199-permutation functionality checks pass, a release tester may repeat PERMANOVA, ANOSIM, and PERMDISP with **999 permutations** and the parallel option selected. Run nMDS, Cluster analysis, SIMPER, and PCoA with their documented large-data settings. Pass when every analysis completes, all required result sections render, bounded plots and narrative text remain inside the report width, expected current warnings are the only warnings, and jamovi remains usable. Record elapsed time only as diagnostic context; there is no timing threshold.

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
