# tofu

`tofu` brings community-ecology methods from R into [jamovi](https://www.jamovi.org). It is designed for teaching at the University of Sydney but is not limited to that setting. The module provides seven focused analyses with a consistent, accessible interface and publication-oriented `ggplot2` figures.

The ecological calculations are powered primarily by the [`vegan`](https://cran.r-project.org/package=vegan) R package. `tofu` deliberately presents a smaller set of commonly taught workflows rather than reproducing all of `vegan`. If you need functionality that is not included, please use `vegan` directly or suggest an addition.

| Analysis | R function | What it does |
|---|---|---|
| PERMANOVA | `vegan::adonis2` | Tests multivariate group differences using permutations |
| ANOSIM | `vegan::anosim` | Provides a rank-based multivariate group comparison |
| PERMDISP | `vegan::betadisper` | Checks whether groups differ in multivariate dispersion |
| nMDS | `vegan::metaMDS` | Ordinates samples with stress and Shepard diagnostics |
| Cluster analysis | `vegan::vegdist`, `stats::hclust` | Displays sample similarity as a hierarchical dendrogram |
| SIMPER | `vegan::simper` | Summarises contributions to Bray-Curtis dissimilarity |
| PCoA | `vegan::wcmdscale` | Visualises distance structure using principal coordinates |

## Installation

`tofu` is being prepared for submission to the jamovi module library. Until it is listed, clone this repository and run the following from the `tofu` directory:

```r
install.packages("jmvtools") # first time only
jmvtools::install(pkg = ".")
```

Testers can also install a compatible `.jmo` file through **Modules → Side-load**. A `.jmo` build is specific to its operating system, processor architecture, and jamovi series.

## Current support

| Capability | PERMANOVA | ANOSIM | PERMDISP | nMDS | Cluster | SIMPER | PCoA |
|---|---|---|---|---|---|---|---|
| Transformation | Yes | Yes | Yes | Yes | Yes | Yes | Yes |
| Selectable dissimilarity | Yes | Yes | Yes | Yes | Yes | Bray-Curtis only | Yes |
| Binary distance | Yes | Yes | Yes | Legacy analyses only | No | No | Yes |
| Blocking factor | Yes | Yes | No | No | No | No | No |
| Permutation scheme | Yes | Yes | Yes | No | No | No | No |
| Parallel option | Yes | Yes | Yes | No | No | No | No |
| Pairwise or contrast output | Optional | Optional | Optional | No | No | Group contrasts | No |
| Main plots | Companion PCoA | Ranked dissimilarities | Distance to centre | Ordination and Shepard | Dendrogram | Contributions and heatmap | Ordination |

## Development and testing

The [functionality-based testing guide](tests/manual/jamovi-functionality-guide.md) provides click-by-click checks for every analysis on small and large datasets. The [UI automation runbook](tests/manual/jamovi-ui-smoke-test.md) records the repeatable Computer Use workflow, lifecycle safeguards, and current automation coverage. Automated regression tests are under [`tests/testthat`](tests/testthat).

## License

`tofu` is free software released under the **GNU General Public License v2 or later** ([GPL-2.0-or-later](https://spdx.org/licenses/GPL-2.0-or-later.html)). The full terms are in [`LICENSE`](LICENSE).

## Acknowledgements

- **[`vegan`](https://cran.r-project.org/package=vegan)** provides the community-ecology methods used by tofu. If you use tofu in published work, please cite `vegan`; run `citation("vegan")` in R for the current reference.
- **[jamovi](https://www.jamovi.org)** is the statistical platform tofu runs on.

`tofu` is developed at the University of Sydney by Dr Januar Harianto, Lecturer in Biostatistics and Data Science in the School of Life and Environmental Sciences.
