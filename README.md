# tofu

A [jamovi](https://www.jamovi.org) module for multivariate abundance and community-style data, powered by the R [vegan](https://cran.r-project.org/package=vegan) package.

## Analyses

All analyses share a transformation and dissimilarity workflow:

| Analysis | Function | Description |
|----------|----------|-------------|
| PERMANOVA | `vegan::adonis2` | Group comparison using permutation tests |
| ANOSIM | `vegan::anosim` | Rank-based group comparison |
| PERMDISP | `vegan::betadisper` | Multivariate dispersion check with distance-to-centre plot |
| nMDS | `vegan::metaMDS` | Ordination with stress and Shepard plots |
| SIMPER | `vegan::simper` | Exploratory feature contributions to Bray-Curtis dissimilarity |

## Installation

Install the released module from within jamovi via the module library, or from source:

```r
# install.packages("jmvtools")
jmvtools::install()
```

## Usage

1. Open jamovi and load abundance-style data with samples as rows and features as columns.
2. Choose an analysis from the **tofu** menu:
   - **Compare groups**: PERMANOVA or ANOSIM
   - **Check dispersion**: PERMDISP
   - **Ordinate samples**: nMDS
   - **Explore feature contributions**: SIMPER
3. Drag feature columns to *Feature variables* and, where required, a grouping column to *Grouping variable*.
4. Choose a transformation and dissimilarity index in the *Resemblance* section.

## License

GPL (>= 2)
