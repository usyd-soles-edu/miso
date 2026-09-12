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
