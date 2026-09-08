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
        "Feature Variables",
        "Grouping Variable",
        "Environmental Variables",
        "Sample Labels" )

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
        "PERMANOVA Companion PCoA",
        "Ranked dissimilarities by pair category",
        "Distance-to-centre summary",
        "ten separate bounded contrast groups",
        "Shepard Diagram Values",
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

test_that("manual destinations use current titled result owners", {
    scenarios <- read.csv(manual_path("scenarios.csv"),
        stringsAsFactors=FALSE, check.names=FALSE)
    references <- read.csv(manual_path("reference-results.csv"),
        stringsAsFactors=FALSE, check.names=FALSE)
    generator <- paste(readLines(manual_path("generate-reference-results.R"),
        warn=FALSE), collapse="\n")
    stale <- c("Interpretation", "Analysis settings", "Data Summary",
        "Result Tables", "Settings")
    split_slots <- function(values) {
        unlist(strsplit(as.character(values), "\\|", fixed=FALSE),
            use.names=FALSE)
    }
    expect_false(any(split_slots(scenarios$result_slot) %in% stale))
    expect_false(any(as.character(references$result_slot) %in% stale))
    for (destination in stale)
        expect_false(grepl(paste0("\\\"", destination, "\\\""),
            generator, fixed=TRUE), info=destination)

    current_titles <- c("Companion PCoA", "Contribution Plot",
        "Contribution Values")
    documents <- paste(
        readLines(functionality_guide_path(), warn=FALSE),
        readLines(manual_path("jamovi-ui-smoke-test.md"), warn=FALSE),
        collapse="\n")
    for (title in current_titles)
        expect_match(documents, title, fixed=TRUE, info=title)
    for (title in c("Contribution Plot", "Contribution Values"))
        expect_true(any(grepl(title, scenarios$result_slot, fixed=TRUE)),
            info=title)
})

test_that("incomplete states retain titled shells and clear their data", {
    guide <- paste(readLines(functionality_guide_path(),
        warn=FALSE), collapse="\\n")
    runbook <- paste(readLines(manual_path("jamovi-ui-smoke-test.md"),
        warn=FALSE), collapse="\\n")
    for (document in c(guide, runbook)) {
        expect_false(grepl("no empty result table|no inferential table appears|result tables remain absent|only getting-started guidance|no blank result",
            document, ignore.case=TRUE))
        expect_match(document, "retained", fixed=TRUE)
        expect_match(document, "cleared", fixed=TRUE)
    }
    for (title in c("PERMANOVA Table", "Global ANOSIM",
        "Dispersion Test", "Cluster Dendrogram", "PCoA Ordination",
        "nMDS Ordination"))
        expect_match(paste(guide, runbook), title, fixed=TRUE, info=title)
})

test_that("manual documentation uses the completed MISO contract and assets", {
    root <- normalizePath(test_path("..", ".."), mustWork=TRUE)
    readme <- paste(readLines(file.path(root, "README.md"),
        warn=FALSE), collapse="\n")
    guide <- paste(readLines(manual_path("jamovi-functionality-guide.md"),
        warn=FALSE), collapse="\n")
    runbook <- paste(readLines(manual_path("jamovi-ui-smoke-test.md"),
        warn=FALSE), collapse="\n")
    dataset_generator <- paste(readLines(
        manual_path("generate-datasets.R"), warn=FALSE), collapse="\n")
    reference_generator <- paste(readLines(
        manual_path("generate-reference-results.R"), warn=FALSE), collapse="\n")
    verifier <- paste(readLines(manual_path("verify-fixtures.R"),
        warn=FALSE), collapse="\n")

    for (document in c(readme, guide, runbook))
        expect_match(document,
            "Multivariate Inference, Similarity and Ordination (MISO)",
            fixed=TRUE)
    expect_match(readme,
        "# Multivariate Inference, Similarity and Ordination (MISO)",
        fixed=TRUE)
    expect_match(runbook,
        "Multivariate Inference, Similarity and Ordination (MISO)", fixed=TRUE)
    expect_match(runbook, "Analyses → MISO", fixed=TRUE)
    for (asset in c(
        "miso-small.csv", "miso-large.csv", "miso-invalid.csv",
        "miso-small-baselines.omv", "miso-large-baselines.omv")) {
        path <- if (grepl("\\.omv$", asset)) {
            file.path(root, "tests", "manual", "workbooks", asset)
        } else {
            manual_path(asset)
        }
        expect_true(file.exists(path), info=asset)
        retired <- paste(c("t", "o", "f", "u"), collapse="")
        retired_path <- file.path(dirname(path),
            sub("^miso", retired, basename(path)))
        expect_false(file.exists(retired_path), info=asset)
    }
    for (asset in c("miso-small.csv", "miso-large.csv", "miso-invalid.csv"))
        expect_match(dataset_generator, asset, fixed=TRUE)
    for (asset in c("miso-small.csv", "miso-large.csv"))
        expect_match(reference_generator, asset, fixed=TRUE)
    for (asset in c("miso-small.csv", "miso-large.csv", "miso-invalid.csv",
        "reference-results.csv", "reference-session-info.txt"))
        expect_match(verifier, asset, fixed=TRUE)
    expect_match(runbook, "Don't Save", fixed=TRUE)
    expect_match(runbook, "do not save recalculated `.omv` files", fixed=TRUE)
    expect_match(runbook, "Never save recalculated `.omv` files", fixed=TRUE)
    expect_match(runbook, "Still unverified:", fixed=TRUE)
    expect_match(runbook, "VoiceOver speech", fixed=TRUE)
    expect_match(runbook, "Windows NVDA", fixed=TRUE)
})

test_that("manual sources and test descriptions contain no retired identifiers", {
    root <- normalizePath(test_path("..", ".."), mustWork=TRUE)
    retired <- paste(c("t", "o", "f", "u"), collapse="")
    scan <- c(
        file.path(root, "README.md"),
        list.files(file.path(root, "tests", "manual"), recursive=TRUE,
            full.names=TRUE),
        list.files(file.path(root, "tests", "testthat"), recursive=TRUE,
            full.names=TRUE))
    scan <- scan[file.info(scan)$isdir %in% FALSE]
    scan <- scan[grepl("\\.(md|R|csv|txt)$|README\\.md$", scan,
        ignore.case=TRUE)]
    has_retired <- function(path) {
        lines <- readLines(path, warn=FALSE, encoding="UTF-8")
        any(grepl(retired, tolower(lines), fixed=TRUE))
    }
    hit_files <- scan[vapply(scan, has_retired, logical(1))]
    expect_true(length(hit_files) == 0L,
        info=paste("retired identifiers in", paste(hit_files, collapse=", ")))
})
