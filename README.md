# Multivariate Inference, Similarity and Ordination (MISO)

Multivariate Inference, Similarity and Ordination (MISO) brings common multivariate methods, often used to analyse ecological and biological datasets, from R into [jamovi](https://www.jamovi.org). It was created to assist in teaching biostatistics at the University of Sydney and should be fine to use for research as long as the user recognises its limitations. The statistical calculations are powered primarily by the [`vegan`](https://cran.r-project.org/package=vegan) R package. If you need functionality that is not included, please use `vegan` directly or suggest an addition.

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

Multivariate Inference, Similarity and Ordination (MISO) uses the following
method references in addition to the [`vegan`](https://vegandevs.github.io/vegan/)
package reference:

- PERMANOVA: Anderson (2001), *Austral Ecology*, 26, 32-46.
- PERMDISP: Anderson (2006), *Biometrics*, 62, 245-253.
- ANOSIM and SIMPER: Clarke (1993), *Australian Journal of Ecology*, 18, 117-143.

## Installation

Multivariate Inference, Similarity and Ordination (MISO) is being prepared for submission to the jamovi module library. Until it is listed, clone this repository and run the following from the module repository directory:

```r
install.packages("jmvtools") # first time only
jmvtools::install(pkg = ".")
```


## Current support

| Capability | PERMANOVA | ANOSIM | PERMDISP | nMDS | Cluster | SIMPER | PCoA |
|---|---|---|---|---|---|---|---|
| Transformation | Yes | Yes | Yes | Yes | Yes | Yes | Yes |
| Selectable dissimilarity | Yes | Yes | Yes | Yes | Yes | Bray-Curtis only | Yes |
| Binary distance | Yes | Yes | Yes | No | No | No | Yes |
| Blocking factor | Yes | Yes | No | No | No | No | No |
| Permutation scheme | Yes | Yes | Yes | No | No | No | No |
| Parallel option | Yes | Yes | Yes | No | No | No | No |
| Pairwise or contrast output | Optional | Optional | Optional | No | No | Group contrasts | No |
| Main plots | Companion PCoA | Ranked dissimilarities | Distance to centre | Ordination and Shepard | Dendrogram | Contributions and heatmap | Ordination |

## License

Multivariate Inference, Similarity and Ordination (MISO) is free software released under the **GNU General Public License v2 or later** ([GPL-2.0-or-later](https://spdx.org/licenses/GPL-2.0-or-later.html)). The full terms are in [`LICENSE`](LICENSE).

## Acknowledgements

- **[`vegan`](https://cran.r-project.org/package=vegan)** provides the community-ecology methods used by the module. If you use Multivariate Inference, Similarity and Ordination (MISO) in published work, please cite `vegan`; run `citation("vegan")` in R for the current reference.
- **[jamovi](https://www.jamovi.org)** is the statistical platform the module runs on.

Multivariate Inference, Similarity and Ordination (MISO) is developed at the University of Sydney by Dr Januar Harianto, Lecturer in Biostatistics and Data Science in the School of Life and Environmental Sciences.
We acknowledge the use of OpenAI's Codex 5.5 and 5.6-sol in assistance with code testing and maintaining the git commit messages.
