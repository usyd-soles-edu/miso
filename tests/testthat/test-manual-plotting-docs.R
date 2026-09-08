manual_path <- function(...) test_path("..", "manual", ...)

functionality_guide_path <- function()
    manual_path("jamovi-functionality-guide.md")

test_that("functionality guide presents all seven analysis cards", {
    guide <- paste(readLines(functionality_guide_path(),
        warn=FALSE), collapse="\n")

    analysis_cards <- c(
        "PERMANOVA", "ANOSIM", "PERMDISP", "nMDS",
        "cluster analysis", "SIMPER", "PCoA")
    for (analysis in analysis_cards)
        expect_match(guide, paste("### Test", analysis), fixed=TRUE)
    expect_match(guide, "vegan::wcmdscale", fixed=TRUE)
    expect_match(guide, "Cluster analysis and PCoA are not saved in these workbooks",
        fixed=TRUE)
    expect_match(guide, "Optional companion PCoA", fixed=TRUE)
    expect_match(guide, "Ranked-dissimilarity diagnostic", fixed=TRUE)
    expect_match(guide, "Distance-to-centre diagnostic; optional ordination",
        fixed=TRUE)
    expect_match(guide, "Separate contribution plots; optional heatmap",
        fixed=TRUE)
    expect_match(guide, "Dendrogram; optional cut", fixed=TRUE)
    expect_match(guide, "pass or fail is determined", fixed=TRUE)
    expect_false(grepl("six analyses below", guide, fixed=TRUE))
    expect_false(grepl("all five baseline analyses", guide, fixed=TRUE))
})

test_that("click-by-click docs use the visible variable-target labels", {
    docs <- list(
        guide=paste(readLines(functionality_guide_path(),
            warn=FALSE), collapse="\n"),
        runbook=paste(readLines(manual_path("jamovi-ui-smoke-test.md"),
            warn=FALSE), collapse="\n"))
    visible_labels <- c(
        "Feature variables (required)",
        "Grouping variable (required)",
        "Grouping variable (optional)",
        "Environmental variables (optional)",
        "Sample labels (optional)")

    for (name in names(docs)) {
        document <- docs[[name]]
        expect_false(
            grepl("(Required|Optional):[[:space:]]+[A-Z]", document),
            info=paste(name, "must not use stale prefix labels"))
        for (label in visible_labels)
            expect_match(document, label, fixed=TRUE, info=name)
        expect_false(grepl(
            "features?[[:space:]]+(drive|drives)|drivers?|responsible",
            document, ignore.case=TRUE), info=name)
    }
})

test_that("runbook distinguishes validated plotting states from remaining gaps", {
    runbook <- paste(readLines(manual_path("jamovi-ui-smoke-test.md"),
        warn=FALSE), collapse="\n")

    expect_match(runbook, "## Current automated scope", fixed=TRUE)
    expect_match(runbook, "## Plotting-overhaul verification",
        fixed=TRUE)
    expect_match(runbook,
        "Status: core small- and large-data states validated in jamovi",
        fixed=TRUE)
    expect_match(runbook, "Still unverified:", fixed=TRUE)
    expect_match(runbook, "Only after the corresponding procedure is physically exercised",
        fixed=TRUE)
    expect_match(runbook, "approximately 320 CSS pixels", fixed=TRUE)
    expect_match(runbook, "VoiceOver speech as unverified", fixed=TRUE)
    expect_match(runbook, "Windows NVDA remains unverified", fixed=TRUE)

    lifecycle_phrases <- c(
        "at most one test-data document window",
        "one small-data document",
        "one large-data document",
        "Don't Save",
        "verify that no jamovi process remains",
        "A failed cleanup is a failed test run")
    for (phrase in lifecycle_phrases)
        expect_match(runbook, phrase, fixed=TRUE)

    required <- c(
        "Pending PERMANOVA plot procedure",
        "Pending ANOSIM plot procedure",
        "Pending PERMDISP plot procedure",
        "Pending SIMPER plot procedure",
        "Pending nMDS plot procedure",
        "Pending Cluster plot procedure",
        "Pending PCoA procedure",
        "PERMANOVA companion PCoA",
        "Ranked dissimilarities by pair category",
        "Distance-to-centre summary",
        "ten separate bounded contrast groups",
        "Pairs shown in the Shepard diagram",
        "Cluster membership",
        "Principal coordinates ordination")
    for (phrase in required)
        expect_match(runbook, phrase, fixed=TRUE)
})

test_that("checked-in PCoA references use direct vegan checkpoints", {
    references <- read.csv(manual_path("reference-results.csv"),
        stringsAsFactors=FALSE, check.names=FALSE)
    scenarios <- read.csv(manual_path("scenarios.csv"),
        stringsAsFactors=FALSE, check.names=FALSE)
    pcoa <- references[references$analysis == "PCoA", , drop=FALSE]

    expected_metrics <- c(
        "rows used", "PCoA1 eigenvalue", "PCoA2 eigenvalue",
        "PCoA1 explained percent", "PCoA2 explained percent",
        "first site absolute PCoA1", "first site absolute PCoA2",
        "negative eigenvalue count", "negative eigenvalue sum")
    expect_setequal(unique(pcoa$scenario_id),
        c("pcoa-small-baseline", "pcoa-large-baseline"))
    expect_true(all(c("pcoa-small-baseline", "pcoa-large-baseline") %in%
        scenarios$scenario_id))
    expect_identical(nrow(pcoa), 18L)
    for (scenario in unique(pcoa$scenario_id))
        expect_setequal(pcoa$metric[pcoa$scenario_id == scenario],
            expected_metrics)
    expect_true(all(grepl("vegan::wcmdscale|complete.cases",
        pcoa$reference_function)))
    expect_false(any(grepl("\\.misoPcoa", pcoa$reference_function)))

    for (dataset in c("miso-small.csv", "miso-large.csv")) {
        data <- read.csv(manual_path(dataset), stringsAsFactors=FALSE,
            check.names=FALSE)
        features <- data[grep("^feature_[0-9]+$", names(data), value=TRUE)]
        features <- features[stats::complete.cases(features), , drop=FALSE]
        features <- features[, vapply(features, function(x) any(x != 0),
            logical(1)), drop=FALSE]
        features <- features[rowSums(features) > 0, , drop=FALSE]
        distance <- vegan::vegdist(features, method="bray", binary=FALSE)
        fit <- vegan::wcmdscale(distance, k=nrow(features) - 1L,
            eig=TRUE, add=FALSE, x.ret=TRUE)
        eigenvalues <- as.numeric(fit$eig)
        positive <- eigenvalues > 0
        negative <- eigenvalues < 0
        explained <- 100 * eigenvalues[positive] /
            sum(eigenvalues[positive])
        actual <- c(
            "rows used"=nrow(features),
            "PCoA1 eigenvalue"=eigenvalues[positive][[1L]],
            "PCoA2 eigenvalue"=eigenvalues[positive][[2L]],
            "PCoA1 explained percent"=explained[[1L]],
            "PCoA2 explained percent"=explained[[2L]],
            "first site absolute PCoA1"=abs(fit$points[1L, 1L]),
            "first site absolute PCoA2"=abs(fit$points[1L, 2L]),
            "negative eigenvalue count"=sum(negative),
            "negative eigenvalue sum"=sum(eigenvalues[negative]))

        scenario <- if (identical(dataset, "miso-small.csv")) {
            "pcoa-small-baseline"
        } else {
            "pcoa-large-baseline"
        }
        expected <- pcoa[pcoa$scenario_id == scenario, , drop=FALSE]
        for (metric in names(actual)) {
            row <- expected[expected$metric == metric, , drop=FALSE]
            expect_identical(nrow(row), 1L)
            expect_equal(unname(actual[[metric]]), as.numeric(row$raw_value),
                tolerance=as.numeric(row$abs_tolerance))
        }
    }
})

test_that("reference generator records PCoA without Multivariate Inference, Similarity and Ordination (MISO) implementation calls", {
    generator <- paste(readLines(manual_path("generate-reference-results.R"),
        warn=FALSE), collapse="\n")

    expect_match(generator, "reference_pcoa <- function", fixed=TRUE)
    expect_match(generator, "vegan::vegdist", fixed=TRUE)
    expect_match(generator, "vegan::wcmdscale", fixed=TRUE)
    expect_match(generator, "first site absolute PCoA1", fixed=TRUE)
    expect_match(generator, "negative eigenvalue sum", fixed=TRUE)
    expect_false(grepl("source\\(.*pcoa", generator))
})
