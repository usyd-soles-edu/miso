# Executable acceptance contract for the approved MISO review remediation.
#
# Plan: docs/superpowers/plans/2026-09-08-miso-review-remediation.md (Task 1)
# Design: docs/superpowers/specs/2026-09-08-miso-review-remediation-design.md
#
# Task 1 ships this contract red on purpose: it must fail for pre-rename
# identity reasons until Task 2 performs the atomic rename of the package,
# module, and technical identifiers and sets the version to 1.0.0. Later
# remediation tasks append their focused assertions to this file.
#
# The retired identifier never occurs literally in this file: every detection
# pattern assembles it from letter fragments so the file needs no exemption
# from its own residue scan. It is never presented as a human-facing name;
# the sole human-facing module name is miso_display_name, exactly.

miso_display_name <- "Multivariate Inference, Similarity and Ordination (MISO)"
miso_technical_name <- "miso"
miso_identity_version <- "1.0.0"
miso_analysis_names <- c(
    "permanova", "anosim", "permdisp", "nmds", "pcoa", "cluster", "simper")

# Detection patterns below assemble the retired identifier from these letter
# fragments so that this file contains no literal occurrence of it and needs
# no exemption from its own residue scan.
miso_retired_identifier <- paste(c("t", "o", "f", "u"), collapse="")
miso_retired_identifier_variants <- c(
    miso_retired_identifier,
    toupper(miso_retired_identifier),
    paste0(
        toupper(substr(miso_retired_identifier, 1L, 1L)),
        substr(miso_retired_identifier, 2L, nchar(miso_retired_identifier))))

# Only the approved design and historical audit locations may still mention
# the retired identifier. Every other tracked occurrence fails the scan,
# including this contract test itself, so a new unintended occurrence is
# caught immediately.
miso_scan_exclusions <- c(
    "docs/superpowers/specs/",
    "jamovi-review/")

miso_repo_root <- function() {
    normalizePath(testthat::test_path("..", ".."), mustWork=TRUE)
}

miso_skip_outside_development_checkout <- function() {
    if (!file.exists(file.path(miso_repo_root(), "jamovi", "0000.yaml")))
        testthat::skip(
            "development source files are not present in the package-check tree")
}

miso_tracked_files <- function() {
    listing <- suppressWarnings(system2(
        "git",
        c("-C", shQuote(miso_repo_root()), "ls-files"),
        stdout=TRUE,
        stderr=FALSE))
    if (!is.character(listing) || !any(nzchar(listing)))
        testthat::skip("git is unavailable; the tracked-tree scan needs git")
    listing[nzchar(listing)]
}

miso_hit_lines <- function(bytes, positions) {
    newlines <- which(bytes == as.raw(10L))
    vapply(positions, function(position)
        sum(newlines < position) + 1L, integer(1))
}

miso_retired_identifier_occurrences <- function(files) {
    occurrences <- list()
    for (relative in files) {
        absolute <- file.path(miso_repo_root(), relative)
        size <- file.info(absolute)$size
        if (is.na(size) || size == 0)
            next
        bytes <- readBin(absolute, "raw", n=size)
        positions <- unlist(lapply(
            miso_retired_identifier_variants,
            function(needle)
                grepRaw(needle, bytes, fixed=TRUE, all=TRUE, value=FALSE)),
            use.names=FALSE)
        if (length(positions) == 0L)
            next
        occurrences[[relative]] <- sort(unique(miso_hit_lines(bytes, positions)))
    }
    occurrences
}

test_that("package metadata declares the Multivariate Inference, Similarity and Ordination (MISO) identity, exact title, and version 1.0.0", {
    fields <- read.dcf(
        file.path(miso_repo_root(), "DESCRIPTION"),
        fields=c("Package", "Title", "Version"))
    expect_identical(unname(fields[[1L, "Package"]]), miso_technical_name)
    expect_identical(unname(fields[[1L, "Title"]]), miso_display_name)
    expect_identical(unname(fields[[1L, "Version"]]), miso_identity_version)
})

test_that("module manifest declares the Multivariate Inference, Similarity and Ordination (MISO) identity, exact title, and version 1.0.0", {
    miso_skip_outside_development_checkout()
    module <- yaml::read_yaml(file.path(miso_repo_root(), "jamovi", "0000.yaml"))
    expect_identical(as.character(module$name), miso_technical_name)
    expect_identical(as.character(module$version), miso_identity_version)
    expect_identical(as.character(module$title), miso_display_name)
})

test_that("the seven public analysis names stay stable under the Multivariate Inference, Similarity and Ordination (MISO) namespace", {
    miso_skip_outside_development_checkout()
    module <- yaml::read_yaml(file.path(miso_repo_root(), "jamovi", "0000.yaml"))
    analyses <- module$analyses
    expect_identical(
        vapply(analyses, `[[`, character(1), "name"),
        miso_analysis_names)
    expect_identical(
        vapply(analyses, `[[`, character(1), "ns"),
        rep(miso_technical_name, length(analyses)))
    expect_identical(
        vapply(analyses, `[[`, character(1), "menuGroup"),
        rep(miso_technical_name, length(analyses)))
})

test_that("the test suite entry point runs the Multivariate Inference, Similarity and Ordination (MISO) package only", {
    entry <- paste(readLines(
        file.path(miso_repo_root(), "tests", "testthat.R"),
        warn=FALSE),
        collapse="\n")
    expect_match(entry, "test_check(\"miso\")", fixed=TRUE)
    expect_false(grepl(
        paste0("test_check(\"", miso_retired_identifier, "\")"),
        entry,
        fixed=TRUE))
})

test_that("the tracked working tree contains no unapproved retired-identifier occurrences", {
    miso_skip_outside_development_checkout()
    files <- miso_tracked_files()
    excluded <- Reduce(`|`, lapply(miso_scan_exclusions,
        function(prefix) startsWith(files, prefix)))
    scanned <- files[!excluded]
    expect_gt(length(scanned), 0L)

    occurrences <- miso_retired_identifier_occurrences(scanned)
    entries <- unlist(Map(
        function(path, lines) paste0(path, ":", lines),
        names(occurrences),
        occurrences),
        use.names=FALSE)
    summary <- sprintf(
        paste0("retired-identifier occurrences: %d lines in %d of %d ",
            "scanned tracked files"),
        length(entries), length(occurrences), length(scanned))
    shown <- if (length(entries) > 60L)
        c(entries[seq_len(60L)], sprintf("... and %d more", length(entries) - 60L))
    else
        entries
    expect_true(length(entries) == 0L,
        info=paste(c(summary, shown), collapse="\n"))
})

test_that("no tracked file keeps a retired asset name", {
    miso_skip_outside_development_checkout()
    files <- miso_tracked_files()
    w <- miso_retired_identifier
    retired_exact <- c(paste0(w, "-common.R"), paste0(w, ".Rproj"))
    retired_patterns <- c(
        paste0("(^|/)", w, "-[^/]*\\.csv$"),
        paste0("(^|/)", w, "-[^/]*-baselines\\.omv$"))
    offenders <- files[basename(files) %in% retired_exact]
    for (pattern in retired_patterns)
        offenders <- c(offenders, files[grepl(pattern, files)])
    offenders <- unique(offenders)
    expect_true(length(offenders) == 0L,
        info=paste0(
            "tracked files still using retired asset names: ",
            paste(offenders, collapse="; ")))
})

test_that("no R source exports or resolves the retired namespace", {
    miso_skip_outside_development_checkout()
    namespaceLines <- readLines(
        file.path(miso_repo_root(), "NAMESPACE"),
        warn=FALSE)
    namespaceHits <- namespaceLines[grepl(
        miso_retired_identifier, namespaceLines, ignore.case=TRUE)]
    expect_true(length(namespaceHits) == 0L,
        info=paste0(
            "NAMESPACE lines naming the retired identifier: ",
            paste(namespaceHits, collapse=" | ")))

    rFiles <- miso_tracked_files()
    rFiles <- rFiles[grepl("^R/[^/]*\\.R$", rFiles)]
    w <- miso_retired_identifier
    resolutionPattern <- paste(
        paste0(w, "::"),
        paste0("requireNamespace\\([\"']", w, "[\"']"),
        paste0("loadNamespace\\([\"']", w, "[\"']"),
        paste0("library\\(", w, "\\)"),
        paste0("useDynLib\\([\"']?", w, "[\"']?"),
        sep="|")
    hits <- character()
    for (relative in rFiles) {
        lines <- readLines(file.path(miso_repo_root(), relative), warn=FALSE)
        matched <- grepl(resolutionPattern, lines, ignore.case=TRUE)
        if (any(matched))
            hits <- c(hits, paste0(relative, ":", which(matched)))
    }
    expect_true(length(hits) == 0L,
        info=paste0(
            "R sources exporting or resolving the retired namespace: ",
            paste(hits, collapse="; ")))
})

# --- Task 6: native result titles, table notes, and display-only lifecycle ---

miso_set_option_value <- function(options, name, value) {
    option <- options$option(name)
    option$.__enclos_env__$private$.value <- value
    invisible(NULL)
}

miso_task6_result_items <- function(name) {
    yaml::read_yaml(file.path(miso_repo_root(), "jamovi", paste0(name, ".r.yaml")))$items
}

test_that("all analyses expose only native titled result elements", {
    miso_skip_outside_development_checkout()
    for (analysisName in miso_analysis_names) {
        items <- miso_task6_result_items(analysisName)
        names <- vapply(items, `[[`, character(1), "name")
        expect_false(any(grepl("Purpose$|^summary$|^settings$", names)), info=analysisName)
        titled <- !vapply(items, function(item) item$name %in% c("guidance", "warnings"), logical(1))
        expect_true(all(vapply(items[titled], function(item) !is.null(item$title) && nzchar(item$title), logical(1))), info=analysisName)
    }
})

test_that("Task 10 keeps analysis exports, namespace loading, and shared helpers discoverable", {
    miso_skip_outside_development_checkout()
    namespace <- readLines(file.path(miso_repo_root(), "NAMESPACE"), warn=FALSE)
    expect_true(all(paste0("export(", miso_analysis_names, ")") %in% namespace))
    expect_false(any(grepl("^import\\((R6|vegan)\\)$", namespace)))

    packageNamespace <- asNamespace(miso_technical_name)
    sharedHelpers <- c("miso_prepare_resemblance", "miso_parallel",
        "miso_parallel_stop")
    expect_true(all(vapply(sharedHelpers, exists, logical(1),
        envir=packageNamespace, inherits=FALSE)))
    for (analysisName in miso_analysis_names) {
        source <- paste(readLines(file.path(
            miso_repo_root(), "R", paste0(analysisName, ".b.R")), warn=FALSE),
            collapse="\\n")
        expect_match(source, "miso_prepare_resemblance", fixed=TRUE,
            info=analysisName)
    }
})

test_that("nMDS and PERMANOVA lifecycle methods have focused seams", {
    nmdsPrivate <- nmdsClass$new(
        options=nmdsOptions$new(vars=c("sp1", "sp2")),
        data=data.frame(sp1=1:6, sp2=6:1))$.__enclos_env__$private
    expect_true(all(c(".prepareNmds", ".fitNmds", ".assembleNmdsResults") %in%
        names(nmdsPrivate)))

    permanovaPrivate <- permanovaClass$new(
        options=permanovaOptions$new(vars=c("sp1", "sp2"), factor="group"),
        data=data.frame(sp1=1:6, sp2=6:1,
            group=factor(c("A", "A", "B", "B", "C", "C"))))$.__enclos_env__$private
    expect_true(all(c(".preparePermanova", ".fitPermanova",
        ".assemblePermanovaResults", ".runCompanionPcoa",
        ".runPairwisePermanova") %in% names(permanovaPrivate)))
})

test_that("display-only controls do not invalidate unrelated native tables", {
    miso_skip_outside_development_checkout()
    displayOnly <- list(
        permanova=c("showCompanionPcoa", "pcoaDisplayFactor", "pcoaCentroids", "pcoaSpiders"),
        anosim="showRankPlot", permdisp=c("showDistancePlot", "showOrdinationPlot"),
        nmds=c("nmdsShepard", "nmdsOverlay", "nmdsSpecies", "nmdsHull", "nmdsEllipse", "nmdsSpider"),
        pcoa=c("showCentroids", "showSpiders"), cluster="sampleLabels",
        simper=c("simperDetails", "simperPlots", "simperHeatmap"))
    for (analysisName in names(displayOnly)) {
        items <- miso_task6_result_items(analysisName)
        for (item in items) {
            clearWith <- item$clearWith
            if (!is.null(clearWith))
                expect_false(any(displayOnly[[analysisName]] %in% clearWith), info=paste(analysisName, item$name))
        }
    }
})

test_that("nMDS optional plot output restores from a display-only rerun", {
    data <- data.frame(sp1=1:9, sp2=9:1, sp3=seq(2, 18, by=2))
    options <- nmdsOptions$new(vars=c("sp1", "sp2", "sp3"), nmdsTrymax=2,
        seed=123, nmdsShepard=FALSE)
    analysis <- nmdsClass$new(options=options, data=data)
    suppressWarnings(suppressMessages(analysis$run()))
    expect_false(analysis$results$shepard$visible)
    miso_set_option_value(options, "nmdsShepard", TRUE)
    suppressWarnings(suppressMessages(analysis$run()))
    expect_true(analysis$results$shepard$visible)
    expect_gt(length(analysis$results$shepardPairs$rowKeys), 0L)
})

# --- Task 11: analysis references resolve to complete citation records ---

test_that("result YAML references resolve to applicable citation records", {
    miso_skip_outside_development_checkout()
    refs <- yaml::read_yaml(file.path(miso_repo_root(), "jamovi", "00refs.yaml"))$refs
    expect_true(is.list(refs))
    ref_ids <- vapply(refs, `[[`, character(1), "id")
    expected_ids <- c("vegan", "anderson2001", "anderson2006", "clarke1993")
    expect_setequal(ref_ids, expected_ids)

    by_id <- setNames(refs, ref_ids)
    for (id in expected_ids) {
        expect_true(nzchar(by_id[[id]]$author), info=id)
        expect_true(is.numeric(by_id[[id]]$year), info=id)
        expect_true(nzchar(by_id[[id]]$title), info=id)
    }
    expect_match(by_id$vegan$title, "vegan", ignore.case=TRUE)
    expect_equal(by_id$anderson2001$year, 2001)
    expect_equal(by_id$anderson2006$year, 2006)
    expect_equal(by_id$clarke1993$year, 1993)

    expected_result_refs <- list(
        permanova=c("vegan", "anderson2001"),
        anosim=c("vegan", "clarke1993"),
        permdisp=c("vegan", "anderson2006"),
        nmds="vegan", pcoa="vegan", cluster="vegan",
        simper=c("vegan", "clarke1993"))
    for (name in names(expected_result_refs)) {
        result <- yaml::read_yaml(file.path(
            miso_repo_root(), "jamovi", paste0(name, ".r.yaml")))
        actual <- as.character(result$refs)
        expect_true(length(actual) > 0L, info=name)
        expect_true(all(actual %in% ref_ids), info=name)
        expect_identical(actual, as.character(expected_result_refs[[name]]),
            info=name)
    }
})

test_that("run-generated notes survive restored analyses and display-only updates", {
    data <- data.frame(sp1=c(1,2,1,7,8,7,3,4,3), sp2=c(2,1,2,8,7,8,4,3,4), sp3=c(1,1,2,6,7,6,3,3,2), group=factor(rep(c("A", "B", "C"), each=3)), block=factor(rep(1:3, times=3)))
    note_text <- function(table) as.character(table$asString())
    restore_analysis <- function(analysis) unserialize(serialize(analysis, NULL))

    permanovaOptionsValue <- permanovaOptions$new(vars=c("sp1", "sp2", "sp3"), factor="group", permN=19, seed=123, permPairwise=TRUE)
    permanovaAnalysis <- permanovaClass$new(options=permanovaOptionsValue, data=data)
    suppressWarnings(suppressMessages(permanovaAnalysis$run()))
    permanovaRestored <- restore_analysis(permanovaAnalysis)
    miso_set_option_value(permanovaRestored$options, "showCompanionPcoa", TRUE)
    suppressWarnings(suppressMessages(permanovaRestored$run()))
    expect_match(note_text(permanovaRestored$results$table), "Bray-Curtis dissimilarities", fixed=TRUE)
    expect_match(note_text(permanovaRestored$results$pairwise), "P-value adjustment")

    permdispOptionsValue <- permdispOptions$new(vars=c("sp1", "sp2", "sp3"), factor="group", permN=19, seed=123, dispPairwise=TRUE, showDistancePlot=TRUE)
    permdispAnalysis <- permdispClass$new(options=permdispOptionsValue, data=data)
    suppressWarnings(suppressMessages(permdispAnalysis$run()))
    permdispRestored <- restore_analysis(permdispAnalysis)
    miso_set_option_value(permdispRestored$options, "showDistancePlot", FALSE)
    suppressWarnings(suppressMessages(permdispRestored$run()))
    expect_match(note_text(permdispRestored$results$anova), "Bray-Curtis dissimilarities", fixed=TRUE)
    expect_match(note_text(permdispRestored$results$distances), "Bray-Curtis dissimilarities", fixed=TRUE)
    expect_match(note_text(permdispRestored$results$pairwise), "Holm across all 3 pairwise contrasts", fixed=TRUE)

    simperOptionsValue <- simperOptions$new(vars=c("sp1", "sp2", "sp3"), factor="group", simperDetails=FALSE, simperAssess=TRUE, simperN=19, seed=123)
    simperAnalysis <- simperClass$new(options=simperOptionsValue, data=data)
    suppressWarnings(suppressMessages(simperAnalysis$run()))
    simperRestored <- restore_analysis(simperAnalysis)
    miso_set_option_value(simperRestored$options, "simperDetails", TRUE)
    suppressWarnings(suppressMessages(simperRestored$run()))
    for (table in list(simperRestored$results$contrasts, simperRestored$results$means))
        expect_false(grepl("Note.", note_text(table), fixed=TRUE))
    expect_match(note_text(simperRestored$results$contributions), "not renormalised", fixed=TRUE)
    expect_match(note_text(simperRestored$results$variability), "between-group sample pairs", fixed=TRUE)
    expect_match(note_text(simperRestored$results$assessment), "Holm adjustment", fixed=TRUE)
})

# --- P1 results-review remediation: data-aware structural cache keys ---
#
# jamovi reruns the same analysis object when its dataset is edited, so a
# structural cache key that covers options only lets same-object data edits
# take the display-only branch and serve stale results. These probes edit
# feature values, group assignments, and the filtered row set on the live
# analysis object and require the rerun to match a fresh analysis of the
# edited data.

miso_cache_probe_data <- function() {
    data.frame(
        sp1=c(1, 2, 1, 7, 8, 7, 3, 4, 3),
        sp2=c(2, 1, 2, 8, 7, 8, 4, 3, 4),
        sp3=c(1, 1, 2, 6, 7, 6, 3, 3, 2),
        group=factor(rep(c("A", "B", "C"), each=3L)))
}

miso_cache_probes <- list(
    list(
        analysis="PERMANOVA",
        build=function(data) permanovaClass$new(options=permanovaOptions$new(
            vars=c("sp1", "sp2", "sp3"), factor="group", permN=19, seed=123),
            data=data),
        statistic=function(analysis) analysis$results$table$asDF),
    list(
        analysis="ANOSIM",
        build=function(data) anosimClass$new(options=anosimOptions$new(
            vars=c("sp1", "sp2", "sp3"), factor="group", anosimN=19, seed=123),
            data=data),
        statistic=function(analysis) analysis$results$global$asDF),
    list(
        analysis="PERMDISP",
        build=function(data) permdispClass$new(options=permdispOptions$new(
            vars=c("sp1", "sp2", "sp3"), factor="group", permN=19, seed=123),
            data=data),
        statistic=function(analysis) analysis$results$anova$asDF),
    list(
        analysis="PCoA",
        build=function(data) pcoaClass$new(options=pcoaOptions$new(
            vars=c("sp1", "sp2", "sp3"), factor="group"), data=data),
        statistic=function(analysis) analysis$results$sites$asDF),
    list(
        analysis="nMDS",
        build=function(data) nmdsClass$new(options=nmdsOptions$new(
            vars=c("sp1", "sp2", "sp3"), factor="group",
            seed=123, nmdsTrymax=2), data=data),
        statistic=function(analysis) analysis$results$sites$asDF),
    list(
        analysis="cluster",
        build=function(data) clusterClass$new(options=clusterOptions$new(
            vars=c("sp1", "sp2", "sp3"), defineClusters=TRUE,
            numberClusters=3), data=data),
        statistic=function(analysis) analysis$results$membership$asDF),
    list(
        analysis="SIMPER",
        build=function(data) simperClass$new(options=simperOptions$new(
            vars=c("sp1", "sp2", "sp3"), factor="group", simperN=19,
            seed=123), data=data),
        statistic=function(analysis) analysis$results$contrasts$asDF))

miso_feature_edit <- function(data) {
    # Collapse group C onto group A's feature values.
    data$sp1[c(7L, 8L, 9L)] <- c(1, 2, 1)
    data$sp2[c(7L, 8L, 9L)] <- c(2, 1, 2)
    data$sp3[c(7L, 8L, 9L)] <- c(1, 1, 2)
    data
}

miso_group_edit <- function(data) {
    # Move one sample from group B into group A.
    data$group <- factor(
        c("A", "A", "A", "A", "B", "B", "C", "C", "C"),
        levels=c("A", "B", "C"))
    data
}

miso_row_edit <- function(data) {
    # A new sample joins the filtered row set. Values are assigned by column
    # name because jmvcore trims each analysis's data to its required
    # variables, so not every analysis object still carries 'group'.
    row <- setNames(as.list(rep(5, ncol(data))), names(data))
    if (! is.null(row$group))
        row$group <- "A"
    data[nrow(data) + 1L, ] <- row
    data
}

miso_expect_rerun_tracks_data <- function(probe, edit) {
    analysis <- probe$build(miso_cache_probe_data())
    suppressWarnings(suppressMessages(analysis$run()))
    before <- probe$statistic(analysis)

    # jamovi updates the dataset on the live analysis object and reruns it;
    # this is the same seam the existing state tests use.
    private <- analysis$.__enclos_env__$private
    private$.data <- edit(private$.data)
    suppressWarnings(suppressMessages(analysis$run()))
    after <- probe$statistic(analysis)

    fresh <- probe$build(private$.data)
    suppressWarnings(suppressMessages(fresh$run()))

    expect_equal(after, probe$statistic(fresh),
        info=paste(probe$analysis, "must recompute after the data edit"))
    expect_false(isTRUE(all.equal(before, after)),
        info=paste(probe$analysis,
            "probe must respond to the data edit for this test to bite"))
}

test_that("same-object reruns recompute after feature values change", {
    for (probe in miso_cache_probes)
        miso_expect_rerun_tracks_data(probe, miso_feature_edit)
})

test_that("same-object reruns recompute after group assignments change", {
    for (probe in miso_cache_probes) {
        if (identical(probe$analysis, "cluster"))
            next  # cluster has no grouping input to reassign
        miso_expect_rerun_tracks_data(probe, miso_group_edit)
    }
})

test_that("same-object reruns recompute after the filtered row set changes", {
    for (probe in miso_cache_probes)
        miso_expect_rerun_tracks_data(probe, miso_row_edit)
})

# --- P1 results-review remediation milestone 2: keyed row reconciliation ---
#
# Structural reruns (changed data signature or model inputs) may only rebuild
# rows whose ordered key sequence actually changed. When every key survives a
# rerun, miso_reconcile_table_rows() must refresh values through setRow so the
# existing Cell objects stay referenced, and miso_clear_table_values() must
# blank stale values without removing the keyed rows available for
# reconciliation.

miso_reconciliation_data <- function() {
    data.frame(
        sp1=c(1, 2, 1, 7, 8, 7, 3, 4, 3),
        sp2=c(2, 1, 2, 8, 7, 8, 4, 3, 4),
        sp3=c(1, 1, 2, 6, 7, 6, 3, 3, 2),
        group=factor(rep(c("A", "B", "C"), each=3L)))
}

miso_reconciliation_analysis <- function(data) {
    analysis <- permdispClass$new(options=permdispOptions$new(
        vars=c("sp1", "sp2", "sp3"), factor="group", permN=19, seed=123),
        data=data)
    suppressWarnings(suppressMessages(analysis$run()))
    analysis
}

miso_keyed_cells <- function(table) {
    table$columns[[1L]]$.__enclos_env__$private$.cells
}

miso_reconciliation_row_values <- function(table, i) {
    as.list(table$asDF[i, , drop=FALSE])
}

test_that("miso_reconcile_table_rows refreshes unchanged keys through the same cells", {
    analysis <- miso_reconciliation_analysis(miso_reconciliation_data())
    table <- analysis$results$distances
    keys <- as.character(unlist(table$rowKeys))
    cells <- miso_keyed_cells(table)
    before <- table$asDF

    rows <- lapply(seq_along(keys), function(i) {
        values <- miso_reconciliation_row_values(table, i)
        values$n <- values$n + 1L
        values$distance <- values$distance + 1
        list(key=keys[[i]], values=values)
    })
    miso_reconcile_table_rows(table, rows)

    expect_identical(as.character(unlist(table$rowKeys)), keys)
    expect_true(identical(miso_keyed_cells(table), cells))
    expect_equal(table$asDF$n, before$n + 1L, tolerance=0)
    expect_equal(table$asDF$distance, before$distance + 1, tolerance=0)
})

test_that("miso_reconcile_table_rows rebuilds when the ordered keys change", {
    analysis <- miso_reconciliation_analysis(miso_reconciliation_data())
    table <- analysis$results$distances
    keys <- as.character(unlist(table$rowKeys))
    cells <- miso_keyed_cells(table)
    before <- table$asDF

    kept <- seq_len(length(keys) - 1L)
    rows <- lapply(kept, function(i)
        list(key=keys[[i]], values=miso_reconciliation_row_values(table, i)))
    miso_reconcile_table_rows(table, rows)

    expect_identical(as.character(unlist(table$rowKeys)), keys[kept])
    expect_false(identical(miso_keyed_cells(table), cells))
    expect_identical(table$asDF$group, before$group[kept])
})

test_that("miso_reconcile_table_rows falls back to a keyed rebuild on duplicate keys", {
    analysis <- miso_reconciliation_analysis(miso_reconciliation_data())
    table <- analysis$results$distances
    cells <- miso_keyed_cells(table)

    rows <- list(
        list(key="dup", values=miso_reconciliation_row_values(table, 1L)),
        list(key="dup", values=miso_reconciliation_row_values(table, 2L)))
    miso_reconcile_table_rows(table, rows)

    expect_identical(as.character(unlist(table$rowKeys)), c("dup", "dup"))
    expect_false(identical(miso_keyed_cells(table), cells))
    expect_length(miso_keyed_cells(table), 2L)
})

test_that("miso_reconcile_table_rows clears the table for an empty target", {
    analysis <- miso_reconciliation_analysis(miso_reconciliation_data())
    table <- analysis$results$distances
    miso_reconcile_table_rows(table, list())
    expect_length(table$rowKeys, 0L)
    expect_equal(nrow(table$asDF), 0L)
})

test_that("miso_clear_table_values blanks values while keeping rows and cells", {
    analysis <- miso_reconciliation_analysis(miso_reconciliation_data())
    table <- analysis$results$distances
    keys <- as.character(unlist(table$rowKeys))
    cells <- miso_keyed_cells(table)

    miso_clear_table_values(table)
    expect_identical(as.character(unlist(table$rowKeys)), keys)
    expect_true(identical(miso_keyed_cells(table), cells))
    expect_true(all(is.na(table$asDF$n)))
    expect_true(all(is.na(table$asDF$distance)))
    expect_true(all(vapply(seq_along(keys), function(i)
        identical(table$columns[[1L]]$getCell(i)$value, ""), logical(1))))
})

test_that("table notes retain interpretation-critical method choices", {
    data <- data.frame(sp1=c(1,2,1,7,8,7,3,4,3), sp2=c(2,1,2,8,7,8,4,3,4), sp3=c(1,1,2,6,7,6,3,3,2), group=factor(rep(c("A", "B", "C"), each=3)), block=factor(rep(1:3, times=3)))
    anosimResult <- anosim(data=data, vars=c("sp1", "sp2", "sp3"), factor="group",
        strata="block", permRestriction="stratified", anosimPairwise=TRUE,
        anosimAdjust="holm", anosimN=19, seed=123)
    expect_match(miso_table_note(anosimResult$global, "meaning"), "Effective restriction")
    expect_match(miso_table_note(anosimResult$pairwise, "scope"), "P-value adjustment")

    pcoaResult <- pcoa(data=data, vars=c("sp1", "sp2", "sp3"), factor="group",
        sqrtDist=TRUE, correction="lingoes")
    expect_match(miso_table_note(pcoaResult$sites, "method"), "Square-root")
    expect_match(miso_table_note(pcoaResult$eigenvalues, "denominator"), "correction")

    clusterResult <- cluster(data=data, vars=c("sp1", "sp2", "sp3"),
        transform="sqrt", defineClusters=TRUE, cutMode="number", numberClusters=3)
    expect_match(miso_table_note(clusterResult$dendrogramStructure, "method"), "Square root transformation", fixed=TRUE)
    expect_match(miso_table_note(clusterResult$membership, "method"), "Cut rule: 3 clusters.", fixed=TRUE)
})
