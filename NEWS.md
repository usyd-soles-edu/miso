# MISO 1.2.1

Release date: 5 October 2026.

## Changed

- Changing plots, labels and optional tables now reuses fitted results across more analyses, avoiding unnecessary recalculation.

## Fixed

- ANOSIM, PERMDISP and SIMPER keep comparisons distinct when group names contain contrast separators or other special characters.
- PERMANOVA and ANOSIM no longer drop samples because of missing values in an assigned Blocking variable when Free permutations are selected.
- PERMANOVA pairwise notes now distinguish sequential comparisons, which test the Grouping variable first, from marginal comparisons, which account for the other model terms.
- PERMDISP reports unavailable test statistics and p-values without presenting them as valid inference, and retains warnings raised during pairwise tests.
- Cluster analysis keeps generated sample labels unique when they overlap with labels already in the data.

## Installation

Choose the `.jmo` file for your computer:

- Apple Silicon Mac: `miso-1.2.1-macos-arm64.jmo`.
- Intel Mac: `miso-1.2.1-macos-x64.jmo`.
- Windows 64-bit: `miso-1.2.1-win-x64.jmo`.

In jamovi, choose **Modules → Sideload Module** and select the file.

# MISO 1.2.0

Release date: 4 October 2026.

## Added

- PERMANOVA now supports pairwise comparisons between groups within each level of one additional factor when interactions are enabled. Each comparison uses a separate fit for its subset, with Holm adjustment across all planned comparisons. Comparisons that cannot be estimated remain in the results. See the README for supported settings.

## Changed

- All PERMANOVA variables can now be assigned in one place: feature variables, the grouping variable, additional factors, a blocking variable and continuous covariates. The interaction option sits beneath additional factors, while permutation restrictions and test type are under analysis choices.
- Conditional pairwise results are grouped by the levels of the additional factor, keeping contrast labels shorter.
- PERMANOVA and pairwise notes are shorter and show the number of permutations used. Seeds are labelled as random or fixed, and methodological details are in the README.

## Installation

Choose the `.jmo` file for your computer:

- Apple Silicon Mac: `miso-1.2.0-macos-arm64.jmo`.
- Intel Mac: `miso-1.2.0-macos-x64.jmo`.
- Windows 64-bit: `miso-1.2.0-win-x64.jmo`.

In jamovi, choose **Modules → Sideload Module** and select the file.

# MISO 1.1.0

Release date: 2 October 2026.

## Added

- Optional site and feature score tables for nMDS, and sample order and merge history tables for cluster analysis.
- Sample context columns alongside cluster membership and sample order.
- Fixed seed controls for permutation analyses and nMDS. MISO keeps automatically generated seeds when an analysis is rerun or a saved file is reopened, and reports the seed used as automatic or fixed.

## Changed

- Core result tables are shown before all inputs are assigned. Notices explain problems with the inputs, replacing the generic startup guidance.
- Changing the nMDS display reuses the fitted ordination, avoiding another run of the analysis. Site and feature score tables are off by default; the feature score table also needs the Feature Scores plot option.
- SIMPER now combines group means and contribution variability in one optional table per contrast. These tables are faster to produce and follow the same feature shortlist as the contribution ranking.
- SIMPER heatmaps show full feature names, wrapping long labels and resizing to fit. The caption explains that grey cells mark features omitted by the display limits, which may still have a non-zero contribution.

## Fixed

- Two-dimensional nMDS results now serialise missing third-axis coordinates correctly.
- nMDS hull and ellipse outlines are easier to distinguish, and ellipses have light shading.
- SIMPER explains why an average/SD ratio is undefined and shows detail warnings only with the relevant output.

## Changes to results

- In nMDS, site and feature score tables are now off by default. Enable them under **Tables** when you need the coordinates. The feature score table also needs **Feature Scores** enabled under **Plots**.
- In cluster analysis, the former Dendrogram Structure table has been split into **Sample order** and **Merge history**. Both are available under **Tables** and are off by default.
- In SIMPER, enable **Group means and contribution variability** under **Tables** to see those details together, in one table per contrast. The tables follow your **Top N features** and **Cumulative contribution (%)** limits.
- Separate values tables for the nMDS Shepard diagram, SIMPER contribution plots and SIMPER heatmap have been removed. The plots remain available.
- To choose your own random seed, enable **Use fixed seed** and enter a positive whole number. Otherwise, MISO generates a seed and keeps it for that analysis. The results report the seed used; older saved analyses with a fixed seed continue to use it.

## Installation

Download one `.jmo` file from this release to match your computer:

- Apple Silicon Mac: `miso-1.1.0-macos-arm64.jmo`.
- Intel Mac: `miso-1.1.0-macos-x64.jmo`.
- Windows 64-bit: `miso-1.1.0-win-x64.jmo`.

In jamovi, choose **Modules → Sideload Module** and select the downloaded file.
The manual remains unpublished and is not included in this release.

# MISO 1.0.0

Initial release prepared for submission to the jamovi library.

- Seven multivariate analyses: PERMANOVA, ANOSIM, PERMDISP, nMDS, PCoA, cluster analysis, and SIMPER, powered by the `vegan` R package.
- Shared resemblance pipeline with selectable transformations, dissimilarity indices (including binary distances), and additive corrections for non-Euclidean distances.
- PERMANOVA model building with additional factors, covariates, interactions, blocking, and selectable permutation schemes; optional pairwise comparisons with multiple-comparison adjustment.
- Parallel permutation support with a reproducible seed option across PERMANOVA, ANOSIM, and PERMDISP.
- Ordination diagnostics: Shepard diagrams, stress and convergence tables, environmental fitting, feature scores, group centroids, hulls, ellipses, and site-centroid connections.
- Cluster analysis with group-average linkage, optional sample labels, and cluster definition by count or dissimilarity height.
- SIMPER contribution analysis with cumulative thresholds, per-contrast contribution plots, heatmaps, and an optional permutation-based assessment.
- Bundled example dataset (dune meadows) available from jamovi's Data Library.
- Full test suite (3,150+ assertions) including parity checks against `vegan` reference output; user manual in `docs/miso-manual/`.
