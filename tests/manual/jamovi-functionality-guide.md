# Testing Multivariate Inference, Similarity and Ordination (MISO) in jamovi

This guide tests the module you see and use in jamovi. It is organised by analysis, so if you changed PERMANOVA you can go directly to **Test PERMANOVA**. The tests check that controls are wired correctly, results render, warnings are useful, and key values agree with direct calls to `vegan`.

A test passes when its output matches the stated reference. It does **not** pass or fail because a p-value happens to be significant. These manual checks complement the automated R tests; they do not establish whether an analysis is appropriate for a research question.

### Rebuild and verify the current checkout

Do this before trusting any jamovi result:

1. If Multivariate Inference, Similarity and Ordination (MISO) is already installed, open jamovi, select **Modules** in the top-right, open the installed-modules view, and remove **Multivariate Inference, Similarity and Ordination (MISO)**.
2. Fully quit jamovi.
3. In a terminal, change to the Multivariate Inference, Similarity and Ordination (MISO) repository and confirm the path:

   ```bash
   pwd
   ```

4. Build and install that checkout:

   ```bash
   R -q -e 'jmvtools::install(pkg = ".")'
   ```

5. Relaunch jamovi. Open **Analyses → MISO** and confirm that seven items are present: PERMANOVA, ANOSIM, PERMDISP, SIMPER, nMDS, Cluster analysis, and PCoA.
6. Open PERMANOVA and confirm **Plots → Show companion PCoA** is available. Open PERMDISP and confirm **Distance-to-Centre Diagnostic** is selected by default. Open Cluster analysis and confirm **Define clusters** is optional. Open PCoA and confirm it includes **Feature Variables**, **Grouping Variable**, **Plots**, and **Advanced Corrections**.

Record the operating system plus the jamovi and Multivariate Inference, Similarity and Ordination (MISO) versions with your test notes. The version alone is not proof that the new build loaded because two local builds may share a version number. If the menu or controls do not match this guide, remove the Multivariate Inference, Similarity and Ordination (MISO) module, fully quit jamovi, rebuild from the confirmed path, and relaunch before investigating the analysis.

### Choose a test scope

| Scope | Use it when | Run |
|---|---|---|
| Quick card | You changed one analysis | That analysis with `miso-small.csv` |
| Functionality regression | You changed its controls or results | Its small-data quick card, advanced checks, and 199-permutation large-data check |
| Full release regression | Shared preprocessing changed or a release is being prepared | All quick cards, advanced and negative checks, then the 999-permutation large-data robustness pass |

The 199- and 999-permutation large-data checks are alternative scopes. You do not need to run both consecutively.

### Open and prepare the test data

The manual-test files are:

- [`miso-small.csv`](miso-small.csv): 24 samples and 8 abundance features; use this for readable correctness checks.
- [`miso-large.csv`](miso-large.csv): 360 samples and 48 independently generated features; use this as a second correctness and robustness check.
- [`miso-invalid.csv`](miso-invalid.csv): dedicated bad-input cases; use it only for the negative checks.
- [`reference-results.csv`](reference-results.csv): full-precision audit values behind the rounded checkpoints below.
- [`miso-small-baselines.omv`](workbooks/miso-small-baselines.omv): the small dataset with PERMANOVA, ANOSIM, PERMDISP, SIMPER, and nMDS configured.
- [`miso-large-baselines.omv`](workbooks/miso-large-baselines.omv): the large dataset with PERMANOVA, ANOSIM, PERMDISP, SIMPER, and nMDS configured.

To open a file, select **File (☰) → Open → This PC → Browse**, navigate to `tests/manual`, and select the CSV.

For a faster repeat run, open one of the `.omv` workbooks instead. Install the current Multivariate Inference, Similarity and Ordination (MISO) build first, then force each saved analysis to recalculate by changing a documented control and restoring it. Do not judge a new build from cached workbook results. Cluster analysis and PCoA are not saved in these workbooks, so create them from the CSV. These repository-only workbooks are excluded from the installed module.

Before analysing either clean dataset, open each column's **Setup** and confirm:

| Columns | Data type | Measurement level |
|---|---|---|
| `sample_id` | Text | ID |
| `group`, `treatment`, `block` | Text | Nominal |
| `temperature`, `pH` | Decimal | Continuous |
| `feature_01` through the final feature | Decimal | Continuous |

To assign one variable, drag it into the named target box, or select it and use the right-arrow. To assign all abundance columns, select `feature_01`, Shift-click the final feature, then transfer the selected range into **Feature Variables**. Click a panel's chevron to expand a collapsed section. In PERMANOVA and ANOSIM, the two required targets stay visible; study-design and reproducibility controls are grouped separately.

jamovi recalculates automatically; there is no **Run** button. Wait until the spinner or progress message disappears and all named result sections have appeared. If you edit test data, reopen the clean CSV before starting another card.

### What Multivariate Inference, Similarity and Ordination (MISO) currently supports

This table describes current computation, including controls that are visible but limited or ignored.

| Capability | PERMANOVA | ANOSIM | PERMDISP | nMDS | Cluster | SIMPER | PCoA |
|---|---|---|---|---|---|---|---|
| Transformation | Yes | Yes | Yes | Yes | Yes | Yes | Yes |
| Selectable dissimilarity | Yes | Yes | Yes | Yes | Yes | No—calculation stays Bray-Curtis | Yes |
| Binary distance | Yes | Yes | Yes | Legacy saved analyses only; hidden for new analyses | No | Not available; SIMPER is fixed to Bray-Curtis | Yes |
| Blocking factor | Yes | Yes; required for Within blocks | No blocking target | No | No | No | No |
| Permutation scheme | Yes | Free, Within blocks, or Series for global and pairwise tests | Free and Series; legacy no-block Stratified values migrate to Free with disclosure | No | No | No | No |
| Parallel option | Yes | Yes, for global and pairwise tests | Yes | No | No | No | No |
| Pairwise table | Optional | Optional | Optional | No | No | Group contrasts | No |
| Main plots | Optional companion PCoA | Ranked-dissimilarity diagnostic | Distance-to-centre diagnostic; optional ordination | Ordination and Shepard diagnostic | Dendrogram; optional cut | Separate contribution plots; optional heatmap | Principal coordinates ordination |

Enabling **Parallel processing** can show that the option does not alter results or cause an error. It cannot prove that worker processes ran, because the module may silently fall back to serial execution.

### Test PERMANOVA

**What this validates:** the student workflow, state-specific guidance, multivariate group comparison through `vegan::adonis2`, conditional pairwise tables, truthful permutation restrictions, shared preprocessing, and the optional descriptive PCoA.

1. Open `miso-small.csv`.
2. Select **Analyses → MISO → PERMANOVA — Test group differences**.
3. Before assigning anything, confirm that the titled **PERMANOVA Table** shell is retained with cleared rows and **Getting started** says to add numeric Feature variables and one categorical Grouping variable. No stale inferential values should appear.
4. Move `feature_01`–`feature_08` to **Feature Variables**. Confirm that the retained **PERMANOVA Table** shell is still cleared, while **Action needed** asks for a Grouping variable.
5. Move `group` to **Grouping Variable**.
6. Under **Analysis choices**, select:
   - **Transformation:** None
   - **Dissimilarity index:** Bray-Curtis
   - **Binary (presence/absence):** cleared
   - **Square-root distances:** cleared
   - **Additive constant:** None
   - **Pairwise comparisons:** cleared
7. Leave **Study Design and Model** collapsed. Its default is **Sequential terms** with **Free** permutation restrictions and no block, factor, covariate, or interaction.
8. Expand **Reproducibility and technical settings** and select:
   - **Number of permutations:** 999
   - **Random seed (0 = random):** 123
   - **Parallel processing:** cleared

Expected **PERMANOVA Table** values for `group`:

| Pseudo-F | R² | Permutation p |
|---:|---:|---:|
| approximately 6.3747 | approximately 0.3778 | .001 |

The **Pseudo-F** and **Permutation p** cells for the `Residual` and `Total` rows should be plain blanks without superscripts or explanatory notes. `NaN` must not appear.

Pass when **PERMANOVA Table** appears with its **PERMANOVA Table** note stating **Permutation restrictions: Free**, **Block used: No**, **Test type: Sequential terms**, **Random seed: 123**, and **Execution: Serial**; values match above to the displayed precision; **Data handling warnings** and **Pairwise PERMANOVA** do not appear.

<details>
<summary>PERMANOVA functionality regression checks</summary>

- **Pairwise dependency:** confirm **P-value adjustment** is disabled while **Pairwise comparisons** is cleared. Select **Pairwise comparisons**, leave the adjustment at **Holm**, and confirm that a populated **Pairwise PERMANOVA** table appears. Clear Pairwise and confirm that the table disappears rather than leaving an empty heading.
- **Additional factor and interactions:** expand **Study Design and Model**, move `treatment` to **Additional Factors**, and confirm both model terms appear. Clear Pairwise, select **Model Interactions**, and confirm the interaction row appears. Pairwise should be unavailable until interactions are cleared.
- **Covariates:** move `temperature` and `pH` to **Continuous Covariates** and confirm both appear as model terms. Change **Test type** between **Sequential terms** and **Marginal terms** and confirm the **PERMANOVA Table** note states the selected **Test type**.
- **Free with an assigned block:** move `block` to **Blocking Variable** while leaving **Permutation restrictions: Free**. The **PERMANOVA Table** remains populated, **Data handling warnings** says the block is unused, and the **PERMANOVA Table** note states **Block used: No (Blocking variable 'block' is not used with Free permutations).**
- **Within blocks without a block:** remove `block`, select **Within blocks — requires a Blocking variable**, and confirm the retained **PERMANOVA Table** shell is cleared while **Action needed** names the missing Blocking variable.
- **Blocked permutations:** assign `block` and keep **Within blocks — requires a Blocking variable**. The analysis should run and the **PERMANOVA Table** note states **Block used: Yes (Blocking variable 'block' is used for Within blocks permutations).**
- **State clearing:** from a valid result, remove `group`, confirm the titled **PERMANOVA Table** and **Pairwise PERMANOVA** shells remain with cleared rows while the Grouping-variable correction appears, then restore `group` and confirm a fresh valid result.
- **Active-section recovery:** close and reopen an analysis containing a block or non-default Test type, and confirm the study-design section opens so the active setting is not concealed.
- **Hellinger + Euclidean:** select **Transformation: Hellinger** and **Dissimilarity: Euclidean**. The group pseudo-F should be approximately 7.4512.
- **Presence/absence + Jaccard:** select **Transformation: Presence/absence**, **Dissimilarity: Jaccard**, and check **Binary**. The group pseudo-F should be approximately 1.0218 and p approximately .494.
- **Parallel-toggle invariance:** return to the baseline, record the table, select **Parallel processing**, and confirm the displayed statistics and p-value do not change.
- **Companion PCoA:** expand **Plots** and select **Companion PCoA**. With the simple baseline model, `group` should be selected automatically. Confirm the **PERMANOVA Companion PCoA** description, **Companion PCoA** image, and **Companion PCoA Site Coordinates** table appear. Select **Group Centroids** and **Connect Sites to Centroids** and confirm the description names the effective layers. Add `treatment` to **Additional Factors**, choose each **Model Factor**, and confirm the display changes without changing the **PERMANOVA Table**. Clear **Companion PCoA** and confirm every companion section disappears. This plot describes the fitted resemblance structure; it is not another test of significance.

</details>

For the large-data confirmation, open `miso-large.csv`, assign `feature_01`–`feature_48`, repeat the baseline with **199 permutations**, and expect pseudo-F approximately **116.1245**, R² approximately **0.3941**, and p **.005**.

If the analysis fails, first confirm the feature columns are numeric, `group` is nominal, **Free** was selected unless a real block was assigned, and the newly built module is loaded.

### Test ANOSIM

**What this validates:** the student workflow, explicit incomplete states, global rank-based comparison, optional pairwise contrasts, and one truthful permutation design shared by both.

1. Open `miso-small.csv`.
2. Select **Analyses → MISO → ANOSIM — Rank-based alternative**.
3. Before assigning anything, confirm that the titled **Global ANOSIM** and **Pairwise ANOSIM** shells are retained with cleared rows while **Getting started** explains ANOSIM and names both required steps.
4. Move `feature_01`–`feature_08` to **Feature Variables**. Confirm that the retained **Global ANOSIM** and **Pairwise ANOSIM** shells remain cleared while **Action needed** asks for a Grouping variable.
5. Move `group` to **Grouping Variable**.
6. Under **Analysis choices**, select **Transformation: None**, **Dissimilarity index: Bray-Curtis**, clear **Binary (presence/absence)**, and leave **Pairwise ANOSIM comparisons** cleared. **P-value adjustment** should be disabled.
7. Leave **Study Design and Permutation Restrictions** collapsed. The default is **Free**, with **Blocking Variable** empty.
8. Expand **Reproducibility and computation**, enter **999** permutations and seed **123**, and clear **Parallel processing**.

Expected baseline result:

| Result | Value |
|---|---:|
| Global R | approximately 0.4950 |
| Permutation p | .001 |

Pass when **Global ANOSIM** appears with its note explaining ranked between/within-group dissimilarities and the requested/effective restriction, **Pairwise ANOSIM** is absent when disabled, and **Ranked Dissimilarities** is populated; the values match; **Data handling warnings** is absent; and the **Global ANOSIM** note discloses Free permutations, no block, seed 123, Pairwise disabled, and serial execution. The plot and summary diagnose the rank distributions behind R; they do not add a second hypothesis test.

<details>
<summary>ANOSIM functionality regression checks</summary>

- **Pairwise dependency:** select **Pairwise ANOSIM comparisons**, leave **P-value adjustment: Holm**, and confirm a populated **Pairwise ANOSIM** table appears. Expected R / adjusted p values are A vs B approximately 0.6780 / .003, A vs C approximately 0.4314 / .003, and B vs C approximately 0.3970 / .003. Clear Pairwise and confirm the heading and rows disappear.
- **Free with an assigned block:** expand **Study Design and Permutation Restrictions**, move `block` to **Blocking Variable**, and leave **Permutation restriction: Free**. **Global ANOSIM** remains populated, **Data handling warnings** says the block is not used, and its note reports the requested/effective restriction without a within-block block.
- **Within blocks without a block:** remove `block`, select **Within blocks**, and confirm the retained **Global ANOSIM** shell is cleared while the Blocking-variable correction appears.
- **Blocked global and pairwise tests:** reassign `block`, keep **Within blocks**, and select Pairwise. The **Global ANOSIM** note identifies the effective within-block design and the **Pairwise ANOSIM** note states that contrasts use the same effective restriction. The R values stay as above for this fixture, but each Holm-adjusted pairwise p is .006.
- **Series:** select **Series (rows in order)**. The **Global ANOSIM** note should identify the current row order, and, when a block is assigned, the current row order within blocks. Do not sort the data between recording the design and running the test.
- **Two groups:** filter or recode the data to retain two groups, request Pairwise, and confirm the retained **Pairwise ANOSIM** shell has no rows while the **Global ANOSIM** note explains that Global ANOSIM is the only contrast.
- **State clearing:** from a valid result, remove `group`, confirm the titled **Global ANOSIM** and **Pairwise ANOSIM** shells remain with cleared rows while the Grouping-variable correction appears, then restore `group` and confirm a fresh result.
- **Negative R guidance:** use a dataset that produces a negative R and confirm the **Global ANOSIM** note and guidance explain that within-group observations are ranked as more dissimilar on average; negative R is not a software error.
- **Parallel-toggle invariance:** compare the global and requested pairwise results with **Parallel processing** cleared and selected. Statistics and p values should not change; settings must disclose whether execution was effectively serial or parallel.
- **Rank diagnostic toggle:** clear **Ranked-dissimilarity diagnostic**. The image and **Plot details** should disappear while **Ranked-dissimilarity summary** and every inferential result remain unchanged. Restore it and confirm the description reports how many pairwise ranks are shown.

</details>

For `miso-large.csv`, assign all 48 features and use **199 permutations**. Expect Global R approximately **0.8289** and Permutation p **.005**. Then select Pairwise and expect adjusted p **.015** for all three contrasts, with R approximately 0.9852, 0.7891, and 0.8187 for A vs B, A vs C, and B vs C respectively.

### Test PERMDISP

**What this validates:** the required-input guidance, distances to group centres, the dispersion permutation test, its accessible distribution summary and plot, and conditional pairwise output.

1. Open `miso-small.csv`.
2. Select **Analyses → MISO → PERMDISP — Check group dispersion**. Before assigning variables, retain the titled **Dispersion Test** and **Distances to Group Centre** shells with cleared rows while **Getting started** is shown.
3. Move `feature_01`–`feature_08` to **Feature Variables** and `group` to **Grouping Variable**.
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

Pass when **Dispersion Test** appears with its structural-cell note stating that blank Residuals F and Permutation p cells are not applicable, and **Distances to Group Centre** appears with its method note naming the transformation, dissimilarity index, and centre. The distance table should contain n, mean, median, standard deviation, minimum, and maximum for all three groups. The plot should contain all three groups. **Data handling warnings** and **Pairwise Dispersion Comparisons** should be absent.

<details>
<summary>PERMDISP functionality regression checks</summary>

- Select **Pairwise dispersion comparisons** with **P-value adjustment: Holm**. **Pairwise Dispersion Comparisons** should appear with t, Permutation p, and Adjusted p.
- Change the centre to **Centroid** and select **Bias adjustment**. The dispersion F should be approximately **2.1561**.
- A copied legacy analysis containing **Stratified** should reproduce the Free baseline, show the compatibility conversion under **Data handling warnings**, and report requested/effective restrictions in the **Dispersion Test** note. Stratified must not appear as a choice in a new analysis.
- **Series (rows in order)** uses the current row order. Do not sort the fixture before an optional Series smoke check.
- Under **Advanced options**, check **Square-root distances** and select **Cailliez** to confirm the distance-correction path completes.
- Under **Plots**, confirm **Distance-to-Centre Diagnostic** is selected and **Ordination with group centres** is cleared. The first image must be followed by **Plot details** and the full-data **Distance-to-centre summary**. Select the optional ordination and confirm **Ordination with group centres**, **Ordination details**, **Ordination coordinates**, and **Plot key** appear. Lines connect sites to fitted centres; no ellipse should appear. Clear each plot independently and confirm its table alternative remains while F and p do not change.
- For parallel-toggle invariance, compare the baseline table before and after selecting **Parallel processing**.
- Remove the Grouping variable after a valid run. Only **Action needed** should remain; restoring it should recreate the results without stale pairwise rows.

</details>

For `miso-large.csv`, use all 48 features and **199 permutations**. Expect F approximately **372.4867**, p **.005**, and mean distances A/B/C approximately **0.1583 / 0.1822 / 0.2472**.

### Test nMDS

**What this validates:** ordination, stress reporting, environmental fitting, group overlays, and the Shepard Diagram.

1. Open `miso-small.csv`.
2. Select **Analyses → MISO → nMDS — Visualise sample patterns**.
3. Move `feature_01`–`feature_08` to **Feature Variables**, `group` to **Grouping Variable**, and `temperature` plus `pH` to **Environmental Variables**.
4. In **Analysis choices**, select **None** and **Bray-Curtis**. New analyses use two dimensions.
5. In **Plots**, select **Shepard Diagram** and clear **Feature Scores**.
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

Pass when **nMDS Ordination**, **Stress and Convergence Diagnostics**, **Shepard Diagram**, **Shepard Diagram Values**, **Environmental Fit**, and **Site Scores** appear. The **Stress and Convergence Diagnostics** note states the dimensions, starts, and iterations; the **Site Scores** note states the transformation and dissimilarity. The ordination should contain 24 site points styled by group and two environmental vectors. Axes and vectors may rotate or reflect; coordinate signs are not pass criteria.

<details>
<summary>nMDS functionality regression checks</summary>

- Change seed **123 → 124 → 123**. Confirm the **Stress and Convergence Diagnostics** note follows each value and stress returns to approximately **0.1636**.
- Remove `group`. The ordination, 24-row **Site Scores**, and **Environmental Fit** must remain; group styling controls must become unavailable. Restore `group` and confirm the base configuration and stress do not change.
- Remove `pH`, then remove all environmental variables. **Environmental Fit** and the vectors should follow the requested variables while the base ordination remains. Restore `temperature` and `pH`.
- Select **Feature Scores** and expect 8 rows. Clear and restore **Shepard Diagram**. Neither output-only change should alter **Site Scores**.
- With **Shepard Diagram** selected, confirm **Shepard Diagram** and **Shepard Diagram Values** follow the image. The table must report the plotted dissimilarity, ordination distance, and monotone fitted distance for the displayed pairs; toggling the Shepard output must not alter stress or site coordinates.
- Select the hull, 1-SD ellipse, and spider overlays. The **nMDS Ordination** description must name every effective layer; the **Site Scores** note and description must state that group displays are descriptive and ellipses are not confidence regions.
- Change environmental-fit permutations to **19**. Temperature should display p **.050** and pH **.250**. Restore **99**.
- Select **Standardize** while retaining **Bray-Curtis**. An actionable compatibility message must replace the stale ordination and inferential output. Select **Euclidean** to recover, then restore **None** and **Bray-Curtis**.
- Remove every required feature. Only the getting-started guidance should remain in the nMDS report. Restore the eight features and confirm a fresh result returns.
- At 200% jamovi zoom, confirm the required and optional labels, the compact tips in **Group display** and **Environmental fit**, all controls, and the report remain readable and reachable. Restore 100%.
- Use Tab and Shift-Tab across the three reproducibility fields. After changing the seed, focus must remain in the options rather than jumping into the report.
- Binary and 3D options are retained only for compatibility with saved legacy analyses and are not visible in a new analysis.

</details>

For `miso-large.csv`, use all 48 features with the same settings. Expect stress approximately **0.1844**, temperature r² approximately **0.385** with p **.010**, and pH r² approximately **0.529** with p **.010**. Confirm 360 **Site Scores**, 48 **Feature Scores** when requested, both plots, and two environmental vectors. With all three descriptive group overlays selected, the site configuration must remain unchanged; do not compare coordinate signs.

### Test cluster analysis

**What this validates:** Bray-Curtis calculation, group-average hierarchical clustering, sample labels, and bounded dendrogram output.

1. Open `miso-small.csv`.
2. Select **Analyses → MISO → Cluster analysis — Visualise sample similarity**.
3. Before assigning anything, confirm the titled **Cluster Dendrogram** and **Dendrogram Structure** shells are retained with cleared data while **Getting Started** asks for numeric Feature variables.
4. Move `feature_01`–`feature_08` to **Feature Variables** and `sample_id` to **Sample Labels**.
5. Under **Analysis choices**, select **Transformation: None** and **Dissimilarity index: Bray-Curtis**.
6. Under **Plots**, select **Show sample labels**.

Expected results:

- **Dendrogram Structure** reports 24 samples and 8 feature variables.
- the **Dendrogram Structure** note reports **Linkage: Group average** and the selected `sample_id` label source.
- The lowest join in the dendrogram connects `S002` and `S023` at Bray-Curtis dissimilarity approximately **0.108**.
- the dendrogram description explains lower branch heights without presenting the clustering as a hypothesis test.

Pass when the **Cluster Dendrogram** is populated, all 24 sample labels are contained within the plot, **Dendrogram Structure** is populated, and **Data handling warnings** is absent; no resemblance matrix is expected.

<details>
<summary>Cluster-analysis functionality regression checks</summary>

- Change **Sample label display** between Automatic, Show, and Hide. Label display should not change branches or heights; restore Automatic.
- Select **Define clusters** and leave **Define clusters by: Number of clusters**, then enter **3**. Confirm a visible cut, a populated **Cluster membership** table, and cluster identities in **Dendrogram structure**. Change to **Dissimilarity height**, enter **0.5**, and confirm the displayed cut and memberships agree with that height. If the height ties a merge, the plot description must disclose the effective boundary. Clear **Define clusters** and confirm the membership table and cut disappear while the uncut dendrogram and structure table remain.
- Change the transformation to **Fourth root**. The **Dendrogram Structure** note should report **Transformation: Fourth root**; restore None.
- Remove every required feature after a valid run. Retain the titled **Cluster Dendrogram** and **Dendrogram Structure** shells with cleared data, and show **Getting Started** guidance; restoring the features should create a fresh dendrogram without stale output.
- If a label column contains blanks or duplicate values, confirm that **Data handling warnings** explains the row-number fallback or disambiguation.
- Long labels should end with an ellipsis inside the plot, accompanied by a warning that the source data are unchanged.

</details>

For `miso-large.csv`, use all 48 features and clear **Show sample labels**. **Dendrogram Structure** should report 360 samples and 48 features, and the **Cluster Dendrogram** should render without a report-width overflow. Selecting **Show sample labels** should give a crowding warning rather than failing.

### Test SIMPER

**What this validates:** Bray-Curtis feature contributions, compact output, top-N and cumulative filtering, optional details, and the exploratory permutation assessment.

1. Open `miso-small.csv`.
2. Select **Analyses → MISO → SIMPER — Feature contributions**.
3. Move `feature_01`–`feature_08` to **Feature Variables** and `group` to **Grouping Variable**.
4. In **Analysis choices**, select transformation **None**. Read the adjacent **Tip**: transformations affect contributions and the displayed means use transformed values. SIMPER is fixed to Bray-Curtis.
5. Under **Features shown**, enter **Top N features: 10** and **Cumulative contribution (%): 70**. Clear **Detailed Statistics**.
6. Leave **Permutation assessment (advanced)** off for the descriptive baseline.

Expected first rows and displayed row counts:

| Contrast | First feature | First contribution | Rows shown |
|---|---|---:|---:|
| A vs B | `feature_04` | approximately 21.20% | 5 |
| A vs C | `feature_01` | approximately 26.32% | 5 |
| B vs C | `feature_04` | approximately 24.43% | 5 |

Pass when **Contrast Summary** and the four-column **Descriptive Feature Contributions** table appear with their meaning notes, **Contribution Plots by Contrast** contains one separately titled **Contrast contribution** group per contrast, and each group exposes **Contribution Plot** and **Contribution Values**. The main table columns are **Contrast**, **Feature**, **Contribution (%)**, and **Cumulative (%)**; detailed statistics, the heatmap, and **Permutation Assessment** should not occupy blank report space when they are not selected.

<details>
<summary>SIMPER functionality regression checks</summary>

- Change **Top N features** from **10 → 9**. Confirm recalculation and that each contrast stops at Top N or the cumulative threshold, while retaining the feature that crosses the threshold.
- Select **Detailed Statistics**. **Contribution Variability** must contain five columns and **Group Means** four columns; both must fit the report width. Clear the option and confirm both tables disappear.
- Select **Contrast overview heatmap**. Confirm the bounded **Contrast overview heatmap**, **Heatmap details**, and **Values shown in the heatmap** appear, then clear it and confirm all three disappear. The separate contrast figures and heatmap are descriptive contribution summaries, not evidence that a feature caused the groups to differ.
- Select **Square root**. Contributions and group means should change; for A vs B the first contribution becomes approximately **18.91%**. Restore **None**.
- Select **Assess contributions with permutations**. The permutations, adjustment, and seed controls must become available. Enter **19**, retain **Holm**, and enter seed **123**; expect a populated four-column **Exploratory permutation assessment**. Clear the assessment and confirm both its table and controls return to the inactive state.
- Remove `group`. Retain the titled **Contrast Summary**, **Descriptive Feature Contributions**, **Contribution Plots by Contrast**, and **Permutation Assessment** shells with cleared data; guidance must ask for the required grouping variable. Restore `group` and confirm all three contrasts are recalculated without stale rows.
- At 200% jamovi zoom, confirm the options, four-column main table, and optional detail tables remain readable and reachable. Restore 100%.
- Fail if `NaN` or `Inf` appears in any visible result.

</details>

For `miso-large.csv`, use all 48 features, **Top N features: 9**, and the 70% cumulative threshold. **Contrast Summary** should report 360 samples, 48 features, three groups, and three contrasts. Exactly nine contribution rows should appear per contrast. The first features for A vs B, A vs C, and B vs C should be `feature_47`, `feature_45`, and `feature_45`, with first contributions approximately **6.54%**, **6.85%**, and **7.10%**.

### Test PCoA

**What this validates:** descriptive principal coordinates analysis through `vegan::wcmdscale`, the exact resemblance preprocessing, eigenvalue accounting, accessible plot alternatives, corrections, and optional grouping overlays. PCoA has no significance test: pass or fail is determined by the stated coordinates, eigenvalues, sections, and interface states—not by separation between groups.

1. Open `miso-small.csv`.
2. Select **Analyses → MISO → PCoA — Visualise distance structure**.
3. Before assigning variables, confirm the titled **PCoA Ordination**, **Site Coordinates**, and **Eigenvalues** shells are retained with cleared data while **Getting started** asks for two or more numeric Feature variables.
4. Move `feature_01`–`feature_08` to **Feature Variables**. Leave **Grouping Variable** empty initially.
5. Under **Analysis choices**, select **Transformation: None**, **Dissimilarity index: Bray-Curtis**, and clear **Binary (presence/absence)**.
6. Under **Plots**, leave **Group Centroids** and **Connect Sites to Centroids** cleared. Both controls should be unavailable until a grouping variable is assigned.
7. Leave **Advanced Corrections** at **Square-root distances: cleared** and **Additive correction: None**.

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

Pass when **Principal coordinates ordination**, **PCoA Ordination**, **Site Coordinates**, and **Eigenvalues** appear. The **Site Coordinates** note states the transformation, dissimilarity, square-root setting, and additive correction; the **Eigenvalues** note states the positive-eigenvalue denominator. The image must have equal physical axis scaling and remain inside the report width. The description must state the two explained percentages, negative-eigenvalue handling, effective layers, and that the ordination is descriptive. Coordinate signs may reverse, so compare their absolute values or reflect the whole axis consistently.

<details>
<summary>PCoA functionality regression checks</summary>

- **Direct vegan parity:** in R, read the same CSV, select the feature columns, calculate `d <- vegan::vegdist(features, method = "bray")`, then run `fit <- vegan::wcmdscale(d, k = nrow(features) - 1, eig = TRUE, add = FALSE, x.ret = TRUE)`. Compare the first two positive `fit$eig` values, percentages calculated against `sum(fit$eig[fit$eig > 0])`, the negative count and sum, and `abs(fit$points[1, 1:2])` with the checkpoints above. Do not use the module internal PCoA helper to create the reference.
- **Grouping overlays:** move `group` to **Grouping Variable**. Confirm points use both colour and shape and full group identities remain in **Site Coordinates**. Select **Group Centroids**, then **Connect Sites to Centroids**. Confirm **Group Centroids** appears with three groups and n = 8 each, the description names both layers, and selecting spiders also draws centroids. Clearing both controls must leave site coordinates and eigenvalues unchanged.
- **Corrections:** under **Advanced Corrections**, select **Lingoes**, then **Cailliez**. Each run must report its correction and constant in the **Site Coordinates** note, while the **Eigenvalues** note retains the positive-axis denominator; coordinates/eigenvalues update and complete without `NaN` or `Inf`. Restore None. Separately select **Square-root distances** and confirm the **Site Coordinates** note discloses it; restore the default.
- **One positive axis:** in a disposable copy of the data, create two continuous columns containing proportional values such as `0,1,2,3,4` and `0,2,4,6,8`. Assign only those columns, select Euclidean, and leave grouping empty. Confirm **Site Coordinates** and **Eigenvalues** remain populated, the **PCoA Ordination** shell is retained without a misleading blank image, and the description explains that only one positive axis is available.
- **Invalid-state clearing:** from a valid result, remove every Feature variable. Confirm **PCoA Ordination**, **Site Coordinates**, and **Eigenvalues** remain as titled shells with cleared data, while **Getting Started** provides the missing-feature guidance. Restore the eight features and confirm fresh results return.

</details>

For `miso-large.csv`, assign all 48 features and repeat the defaults. Expect 360 samples, PCoA1/PCoA2 eigenvalues approximately **5.29746 / 5.09098**, explained percentages approximately **16.9022% / 16.2434%**, absolute first-site coordinates approximately **0.14261 / 0.08661**, and 210 negative eigenvalues summing to approximately **−7.30426**. Add `group`, centroids, and spiders. Pass when the plot is bounded, tables retain all 360 sites, any plotted-point limit is disclosed, and neither the options nor report requires horizontal scrolling.

### Shared negative-input checks

Open `miso-invalid.csv` and use PERMANOVA unless stated otherwise. Reopen the file before each check. Before the all-zero-feature check, set `all_zero_feature` to **Continuous** under **Variables → Edit**; jamovi imports this constant column as Nominal by default.

| Check | What to assign | Expected current behaviour |
|---|---|---|
| No features | `group` only | Retain the titled **PERMANOVA Table** shell with cleared rows; **Getting started** shows both required steps |
| No group | `valid_01`, `valid_02`; no grouping variable | Retain the titled **PERMANOVA Table** shell with cleared rows; **Action needed** asks for one categorical Grouping variable |
| Text feature | Try to move `text_feature` to **Feature Variables** | jamovi refuses the transfer because the target permits numeric variables only |
| Negative abundance | `valid_01`, `negative_feature`; group `group` | Retain the titled **PERMANOVA Table** shell with cleared rows; **Action needed** identifies `negative_feature` |
| Missing abundance | `valid_01`, `missing_feature`; group `group` | warning reports one excluded row and the summary reports seven samples |
| All-zero feature | `valid_01`, `all_zero_feature`; group `group` | warning reports that the zero feature was removed |
| All-zero sample | `zero_case_01`, `zero_case_02`; group `group` | warning reports that one all-zero sample was removed |
| One group | `valid_01`, `valid_02`; group `single_group` | Retain the titled **PERMANOVA Table** shell with cleared rows; **Action needed** says the Grouping variable has fewer than two groups |

### Full large-data robustness pass

After the 199-permutation functionality checks pass, a release tester may repeat PERMANOVA, ANOSIM, and PERMDISP with **999 permutations** and the parallel option selected. Run nMDS, Cluster analysis, SIMPER, and PCoA with their documented large-data settings. Pass when every analysis completes, all required result sections render, bounded plots and narrative text remain inside the report width, expected current warnings are the only warnings, and jamovi remains usable. Record elapsed time only as diagnostic context; there is no timing threshold.

### Maintaining this guide

When the module changes, use these sources together:

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

The generators call `vegan` directly and do not source module implementation files. See `tests/manual/reference-session-info.txt` for the recorded R, vegan, permute, RNG, and platform details.
