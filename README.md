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


## License

`tofu` is free software, released under the **GNU General Public License v2 or later** ([GPL-2.0-or-later](https://spdx.org/licenses/GPL-2.0-or-later.html)). The full terms are in [`LICENSE`](LICENSE).



## Acknowledgements

Two projects do the heavy lifting behind tofu:

- **[`vegan`](https://cran.r-project.org/package=vegan)**, the community-ecology methods, by Jari Oksanen and the `vegan` development team. Every number tofu reports comes out of `vegan`. If you use tofu in published work, please cite `vegan`; run `citation("vegan")` in R for the reference.
- **[jamovi](https://www.jamovi.org)**, the statistical platform tofu runs on.

`tofu` is developed at the University of Sydney.
