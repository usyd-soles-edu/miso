# Multivariate Inference, Similarity and Ordination (MISO)

MISO brings common multivariate methods, often used to analyse ecological and biological datasets, from R into [jamovi](https://www.jamovi.org). It was created to assist in teaching biostatistics at the University of Sydney and should be fine to use for research as long as the user recognises its limitations. The statistical calculations are powered primarily by the [`vegan`](https://cran.r-project.org/package=vegan) R package. If you need functionality that is not included, please use `vegan` directly or suggest an addition.


| Analysis | R function | What it does |
|---|---|---|
| PERMANOVA | `vegan::adonis2` | Tests multivariate group differences using permutations |
| ANOSIM | `vegan::anosim` | Provides a rank-based multivariate group comparison |
| PERMDISP | `vegan::betadisper` | Checks whether groups differ in multivariate dispersion |
| nMDS | `vegan::metaMDS` | Ordinates samples with stress and Shepard diagnostics |
| Cluster analysis | `vegan::vegdist`, `stats::hclust` | Displays sample similarity as a hierarchical dendrogram |
| SIMPER | `vegan::simper` | Summarises contributions to Bray-Curtis dissimilarity |
| PCoA | `vegan::wcmdscale` | Visualises distance structure using principal coordinates |

## Method citations

MISO uses the following
method references in addition to the [`vegan`](https://vegandevs.github.io/vegan/)
package reference:

- PERMANOVA: Anderson (2001), *Austral Ecology*, 26, 32-46.
- PERMDISP: Anderson (2006), *Biometrics*, 62, 245-253.
- ANOSIM and SIMPER: Clarke (1993), *Australian Journal of Ecology*, 18, 117-143.

## Installation

Download the `.jmo` file for your computer from the [latest GitHub release](https://github.com/usyd-soles-edu/miso/releases/latest): **macos-arm64** for Apple Silicon Macs, **macos-x64** for Intel Macs, or **win-x64** for 64-bit Windows. In jamovi, choose **Modules → Sideload Module** and select the downloaded file.

MISO is being prepared for submission to the jamovi module library. To build from source, clone this repository and run the following from the module repository directory:

```r
install.packages("jmvtools") # first time only
jmvtools::install(pkg = ".")
```


## Current support


**Group-comparison tests**

| Capability | PERMANOVA | ANOSIM | PERMDISP | SIMPER |
|---|---|---|---|---|
| Transformation | Yes | Yes | Yes | Yes |
| Selectable dissimilarity | Yes | Yes | Yes | Bray-Curtis only |
| Binary distance | Yes | Yes | Yes | No |
| Blocking factor | Yes | Yes | No | No |
| Permutation scheme | Yes | Yes | Yes | No |
| Parallel option | Yes | Yes | Yes | No |
| Pairwise or contrast output | Optional | Optional | Optional | Group contrasts |
| Main plots | Companion PCoA | Ranked dissimilarities | Distance to centre | Contributions and heatmap |

**Ordination and visualisation**

| Capability | nMDS | Cluster | PCoA |
|---|---|---|---|
| Transformation | Yes | Yes | Yes |
| Selectable dissimilarity | Yes | Yes | Yes |
| Binary distance | No | No | Yes |
| Main plots | Ordination and Shepard | Dendrogram | Ordination |

## Permutation restrictions and retained samples

In PERMANOVA and ANOSIM, assigning a Blocking variable restricts permutations only when a restriction that uses it is selected. With **Free** permutations, the Blocking variable is inactive: its missing values do not remove samples from the analysis. Other assigned analysis variables still determine which samples are retained.

## PERMANOVA pairwise comparisons

Without interactions, each comparison refits the selected model on a pair of Grouping levels. With **Sequential terms**, the Grouping variable is tested first, before Additional factors and Continuous covariates. With **Marginal terms**, it is tested after accounting for the other model terms. Active permutation restrictions are retained in each subset. Pairwise comparisons are unavailable with **Omnibus** tests.

### Comparisons with interactions

With **Model interactions** enabled, **Pairwise comparisons** tests each pair of Grouping levels within each level of one Additional factor. This mode requires **Marginal terms**, exactly one selected Additional factor retained in the fitted model, **Holm** adjustment, **Free** permutations, no Blocking variable or Continuous covariates, and either untransformed Euclidean or fourth-root Bray-Curtis distances. Binary distances, square-root distances and additive constants are not supported in this mode.

Each comparison refits the Grouping variable on its subset, using that subset's residual variance. Holm adjustment covers the complete planned family, including comparisons marked “not estimated”. Each Grouping level needs at least two samples in a subset; subsets with zero distances or invalid fitted statistics are also marked “not estimated”.

These tests assume independent observations and free permutations within each subset. They are conditional simple-effect tests, rather than full-model tests, Type III main effects or PRIMER pooled pairwise comparisons. The omnibus interaction test is separate from the adjusted pairwise family; comparisons are calculated regardless of its significance.

## Reusing fitted results

Changing plots, labels or optional tables reuses fitted results when those options affect presentation only. Changing the data, model, distance settings, permutation settings or effective seed recalculates the fit. Automatically generated seeds are retained through refits and saved-file reloads; the results report the seed used.

## Automated tests

Pull requests and pushes to `main` run two Linux checks: **R tests** and
**Archive validation**. The R check includes numerical, saved-result, cache,
parallel-worker and JavaScript controller tests. It installs `RProtoBuf` and
fails if any test is skipped. The archive check runs the validator's adversarial
fixtures and native 7-Zip tests using the build workflow's checksum-pinned parser.
Full platform builds remain available through the manual build and release workflows.

To run the same R check locally, install the package's dependencies plus `pkgload`,
`testthat` and `RProtoBuf`, ensure Node.js is available, then run
`Rscript .github/scripts/run-r-tests.R` from the repository root. Repository rules
can require the two checks before merging once they have run successfully on GitHub.

Some `jmvcore` binaries were built without saved-result support. CI rebuilds
`jmvcore` from source when needed, after installing `RProtoBuf`, and caches the
repaired package. If the local check reports this issue, reinstall it with
`install.packages("jmvcore", repos="https://cloud.r-project.org", type="source")`
after installing `RProtoBuf`.

## License

MISO is free software released under the **GNU General Public License v2 or later** ([GPL-2.0-or-later](https://spdx.org/licenses/GPL-2.0-or-later.html)). The full terms are in [`LICENSE`](LICENSE).

## Acknowledgements

- **[`vegan`](https://cran.r-project.org/package=vegan)** provides the community-ecology methods used by the module. If you use MISO in published work, please cite `vegan`; run `citation("vegan")` in R for the current reference.
- **[jamovi](https://www.jamovi.org)** is the statistical platform the module runs on.

MISO is developed at the University of Sydney by Dr Januar Harianto, Data Scientist and Lecturer in Biostatistics and Data Science in the School of Life and Environmental Sciences.

We acknowledge the use of OpenAI's Codex 5.5 and 5.6-sol in assistance with code testing and maintaining the git repository. We also acknowledge the use of GitHub Copilot for Education in assisstance with code review and documentation.
