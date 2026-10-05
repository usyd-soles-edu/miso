fit_cache_data <- function() {
    i <- seq_len(18L)
    data.frame(x=1 + i %% 5L, y=2 + i %% 7L, z=3 + rev(i %% 6L),
        group=factor(rep(c("A", "B", "C"), each=6L)),
        alternative=factor(rep(c("X", "Y"), 9L)), temperature=10 + i)
}

fit_cache_run <- function(analysis) {
    suppressWarnings(suppressMessages(analysis$run()))
    invisible(analysis)
}

fit_cache_set <- function(analysis, name, value, data=NULL) {
    option <- analysis$options$option(name)
    option$value <- value
    analysis$optionsChangedHandler(name)
    # Native jamovi supplies the newly requested columns on option changes.
    if (!is.null(data))
        analysis$.__enclos_env__$private$.data <- data
    fit_cache_run(analysis)
}

test_that("NMDS grouping and environmental edits reuse the fit and match fresh output", {
    original <- vegan::metaMDS
    calls <- 0L
    testthat::local_mocked_bindings(metaMDS=function(...) {
        calls <<- calls + 1L
        original(...)
    }, .package="vegan")
    data <- fit_cache_data()
    # Retained source-row mapping must survive a missing feature value.
    data$x[[3L]] <- NA_real_
    options <- list(vars=c("x", "y", "z"), seed=123, nmdsTrymax=5,
        nmdsSpecies=TRUE, nmdsSiteTable=TRUE, nmdsOverlay=TRUE, nmdsHull=TRUE)
    analysis <- fit_cache_run(nmdsClass$new(
        options=do.call(nmdsOptions$new, options), data=data))
    private <- analysis$.__enclos_env__$private
    fit <- private$.state$fit
    sites <- private$.state$sites
    expect_identical(calls, 1L)

    for (group in list("group", "alternative", NULL, "group")) {
        before <- calls
        rng <- .Random.seed
        fit_cache_set(analysis, "factor", group, data=data)
        expect_identical(calls, before)
        expect_identical(.Random.seed, rng)
        expect_identical(private$.state$fit, fit)
        expect_identical(private$.state$sites, sites)
        options$factor <- group
        fresh <- fit_cache_run(nmdsClass$new(
            options=do.call(nmdsOptions$new, options), data=data))
        for (name in c("sites", "stress", "features"))
            expect_equal(analysis$results[[name]]$asDF, fresh$results[[name]]$asDF)
        expect_equal(analysis$results$ordination$state, fresh$results$ordination$state)
        expect_identical(miso_squish_result(analysis$results$warnings),
            miso_squish_result(fresh$results$warnings))
    }

    data$group[[1L]] <- NA
    data$temperature[[4L]] <- NA_real_
    private$.data <- data
    before <- calls
    fit_cache_run(analysis)
    fit_cache_set(analysis, "nmdsEnv", "temperature", data=data)
    fit_cache_set(analysis, "nmdsEnvPerm", 19)
    expect_identical(calls, before)
    expect_identical(private$.state$fit, fit)
    options$nmdsEnv <- "temperature"
    options$nmdsEnvPerm <- 19
    fresh <- fit_cache_run(nmdsClass$new(
        options=do.call(nmdsOptions$new, options), data=data))
    expect_equal(analysis$results$sites$asDF, fresh$results$sites$asDF)
    expect_equal(analysis$results$envfit$asDF, fresh$results$envfit$asDF)
    expect_equal(analysis$results$ordination$state, fresh$results$ordination$state)
    expect_identical(miso_squish_result(analysis$results$warnings),
        miso_squish_result(fresh$results$warnings))

    before <- calls
    private$.data$x[[1L]] <- private$.data$x[[1L]] + 1
    fit_cache_run(analysis)
    expect_identical(calls, before + 1L)
    before <- calls
    fit_cache_set(analysis, "seed", 321)
    expect_identical(calls, before + 1L)
    before <- calls
    fit_cache_set(analysis, "nmdsTrymax", 6)
    expect_identical(calls, before + 1L)
})

test_that("serialized NMDS cache retains fit dependencies and automatic seed provenance", {
    original <- vegan::metaMDS
    calls <- 0L
    testthat::local_mocked_bindings(metaMDS=function(...) {
        calls <<- calls + 1L
        original(...)
    }, .package="vegan")
    data <- fit_cache_data()
    options <- list(vars=c("x", "y", "z"), nmdsTrymax=5, seed=0)
    analysis <- fit_cache_run(nmdsClass$new(
        options=do.call(nmdsOptions$new, options), data=data))
    private <- analysis$.__enclos_env__$private
    cache <- unserialize(serialize(analysis$results$analysisCache$state, NULL))
    restored <- nmdsClass$new(options=do.call(nmdsOptions$new, options), data=data)
    restored$results$analysisCache$setState(cache)
    restored$results$seedState$setState(analysis$results$seedState$state)
    rng <- .Random.seed
    fit_cache_run(restored)
    fit_cache_set(restored, "factor", "group", data=data)
    expect_identical(calls, 1L)
    expect_identical(.Random.seed, rng)
    state <- restored$.__enclos_env__$private$.state
    expect_identical(state$fit, private$.state$fit)
    expect_identical(state$prep$actualSeed, private$.state$prep$actualSeed)
    expect_identical(state$prep$seedSource, "automatic")
    invalid <- data
    invalid$x <- -1
    restored$.__enclos_env__$private$.data <- invalid
    fit_cache_run(restored)
    expect_null(restored$.__enclos_env__$private$.state$fit)
    expect_null(restored$results$analysisCache$state)
    restored$.__enclos_env__$private$.data <- data
    fit_cache_run(restored)
    expect_identical(calls, 2L)
})

test_that("PERMANOVA adjustment edits reuse fits while updating values and notes", {
    original <- vegan::adonis2
    calls <- 0L
    testthat::local_mocked_bindings(adonis2=function(formula, ...) {
        calls <<- calls + 1L
        # adonis2 evaluates its response in the caller's frame.
        do.call(original, c(list(formula=formula), list(...)), envir=parent.frame())
    }, .package="vegan")
    data <- fit_cache_data()
    options <- list(vars=c("x", "y", "z"), factor="group",
        distance="euclidean", permN=19, seed=123, permPairwise=TRUE)
    analysis <- fit_cache_run(permanovaClass$new(
        options=do.call(permanovaOptions$new, options), data=data))
    expect_identical(calls, 4L)
    global <- analysis$results$table$asDF
    cell <- miso_table_first_cell(analysis$results$pairwise)
    for (adjust in c("none", "BH", "bonferroni", "holm")) {
        before <- calls
        rng <- .Random.seed
        fit_cache_set(analysis, "permAdjust", adjust)
        expect_identical(calls, before)
        expect_identical(.Random.seed, rng)
        expect_identical(analysis$results$table$asDF, global)
        expect_identical(miso_table_first_cell(analysis$results$pairwise), cell)
        options$permAdjust <- adjust
        fresh <- fit_cache_run(permanovaClass$new(
            options=do.call(permanovaOptions$new, options), data=data))
        expect_equal(analysis$results$pairwise$asDF, fresh$results$pairwise$asDF)
        expect_identical(miso_table_note(analysis$results$pairwise, "scope"),
            miso_table_note(fresh$results$pairwise, "scope"))
    }
    before <- calls
    analysis$.__enclos_env__$private$.data$x[[1L]] <- 20
    fit_cache_run(analysis)
    expect_identical(calls, before + 4L)
    before <- calls
    fit_cache_set(analysis, "seed", 321)
    expect_identical(calls, before + 4L)
})

test_that("conditional PERMANOVA adjustment edits preserve eligibility and cached planned rows", {
    original <- vegan::adonis2
    calls <- 0L
    testthat::local_mocked_bindings(adonis2=function(formula, ...) {
        calls <<- calls + 1L
        do.call(original, c(list(formula=formula), list(...)), envir=parent.frame())
    }, .package="vegan")
    data <- fit_cache_data()
    options <- list(vars=c("x", "y", "z"), factor="group", permFactors="alternative",
        permInteractions=TRUE, permBy="margin", permPairwise=TRUE,
        distance="euclidean", permN=19, seed=123)
    for (initial in c("holm", "none")) {
        options$permAdjust <- initial
        analysis <- fit_cache_run(permanovaClass$new(
            options=do.call(permanovaOptions$new, options), data=data))
        global <- analysis$results$table$asDF
        before <- calls
        fit_cache_set(analysis, "permAdjust", "none")
        expect_identical(calls, before)
        expect_false(analysis$results$pairwise$visible)
        expect_miso_empty_table(analysis$results$pairwise)
        expect_match(miso_squish_result(analysis$results$warnings),
            "P-value adjustment is None", fixed=TRUE)
        fit_cache_set(analysis, "permAdjust", "holm")
        # Activating previously unavailable comparisons fits only that derived analysis.
        expect_identical(calls, before + if (initial == "none") 6L else 0L)
        fitted <- calls
        pairwise <- analysis$results$pairwise$asDF
        fit_cache_set(analysis, "permAdjust", "bonferroni")
        fit_cache_set(analysis, "permAdjust", "holm")
        expect_identical(calls, fitted)
        expect_identical(analysis$results$table$asDF, global)
        expect_identical(analysis$results$pairwise$asDF, pairwise)
        expect_false(analysis$results$warnings$visible)
        expect_match(miso_table_note(analysis$results$pairwise, "scope"),
            "Holm correction across 6 planned contrasts", fixed=TRUE)
    }
})

test_that("SIMPER display limits reuse full descriptive and permutation statistics", {
    original <- vegan::simper
    calls <- 0L
    testthat::local_mocked_bindings(simper=function(...) {
        calls <<- calls + 1L
        original(...)
    }, .package="vegan")
    data <- fit_cache_data()
    options <- list(vars=c("x", "y", "z"), factor="group", simperTop=1,
        simperCum=100, simperDetails=TRUE, simperPlots=TRUE, simperHeatmap=TRUE,
        simperAssess=TRUE, simperN=19, seed=123)
    analysis <- fit_cache_run(simperClass$new(
        options=do.call(simperOptions$new, options), data=data))
    private <- analysis$.__enclos_env__$private
    full <- private$.state$descriptive$fullRows
    fit <- private$.state$descriptive$fit
    assessment <- private$.state$assessmentValues
    expect_identical(calls, 2L)
    for (selection in list(c(3, 100), c(3, 40), c(1, 100), c(3, 100))) {
        images <- c(lapply(analysis$results$contributionPlots$items,
            function(item) item$plot), list(analysis$results$heatmap))
        for (image in images)
            image$.setPath("cached-image.png")
        before <- calls
        rng <- .Random.seed
        fit_cache_set(analysis, "simperTop", selection[[1L]])
        fit_cache_set(analysis, "simperCum", selection[[2L]])
        expect_identical(calls, before)
        expect_identical(.Random.seed, rng)
        expect_identical(private$.state$descriptive$fullRows, full)
        expect_identical(private$.state$descriptive$fit, fit)
        expect_identical(private$.state$assessmentValues, assessment)
        expect_length(analysis$results$detailsByContrast$items, 3L)
        expect_length(analysis$results$contributionPlots$items, 3L)
        for (image in images)
            expect_null(image$.__enclos_env__$private$.filePath)
        options$simperTop <- selection[[1L]]
        options$simperCum <- selection[[2L]]
        fresh <- fit_cache_run(simperClass$new(
            options=do.call(simperOptions$new, options), data=data))
        for (name in c("contrasts", "contributions", "assessment"))
            expect_equal(analysis$results[[name]]$asDF, fresh$results[[name]]$asDF)
        expect_equal(simper_test_detail_frame(analysis$results),
            simper_test_detail_frame(fresh$results))
        expect_equal(analysis$results$heatmap$state, fresh$results$heatmap$state)
        expect_equal(lapply(analysis$results$contributionPlots$items, function(item) item$plot$state),
            lapply(fresh$results$contributionPlots$items, function(item) item$plot$state))
    }
    before <- calls
    private$.data$x[[1L]] <- 20
    fit_cache_run(analysis)
    expect_identical(calls, before + 2L)
    before <- calls
    fit_cache_set(analysis, "seed", 321)
    expect_identical(calls, before + 2L)
})

test_that("expanding SIMPER display limits restores assessment notes from cached values", {
    original <- vegan::simper
    calls <- 0L
    testthat::local_mocked_bindings(simper=function(..., permutations) {
        calls <<- calls + 1L
        fit <- original(..., permutations=permutations)
        if (permutations > 0L) {
            for (i in seq_along(fit)) {
                average <- fit[[i]]$average
                first <- names(average)[order(-average, names(average))][[1L]]
                fit[[i]]$p[] <- .5
                fit[[i]]$p[[first]] <- NA_real_
            }
        }
        fit
    }, .package="vegan")
    analysis <- fit_cache_run(simperClass$new(options=simperOptions$new(
        vars=c("x", "y", "z"), factor="group", simperTop=1, simperCum=100,
        simperAssess=TRUE, simperN=19, seed=123), data=fit_cache_data()))
    expect_false(analysis$results$assessment$visible)
    expect_match(miso_squish_result(analysis$results$warnings),
        "No usable permutation p-values", fixed=TRUE)
    rng <- .Random.seed
    fit_cache_set(analysis, "simperTop", 3)
    expect_identical(calls, 2L)
    expect_identical(.Random.seed, rng)
    expect_true(analysis$results$assessment$visible)
    expect_equal(nrow(analysis$results$assessment$asDF), 6L)
    expect_false(analysis$results$warnings$visible)
    expect_match(miso_table_note(analysis$results$assessment, "seed"),
        "Random seed: 123 (fixed).", fixed=TRUE)
    fit_cache_set(analysis, "simperTop", 1)
    expect_false(analysis$results$assessment$visible)
    expect_miso_empty_table(analysis$results$assessment)
    expect_identical(calls, 2L)
})

test_that("PERMDISP global and pairwise output share one seeded permutation calculation", {
    original <- vegan::permutest
    calls <- 0L
    testthat::local_mocked_bindings(permutest=function(...) {
        calls <<- calls + 1L
        original(...)
    }, .package="vegan")
    data <- fit_cache_data()
    for (pairwise in c(FALSE, TRUE)) {
        before <- calls
        result <- suppressWarnings(suppressMessages(permdisp(data=data,
            vars=c("x", "y", "z"), factor="group", distance="euclidean",
            permN=19, seed=123, dispPairwise=pairwise)))
        expect_identical(calls, before + 1L)
        rng <- .Random.seed
        set.seed(123)
        fit <- vegan::betadisper(stats::dist(data[c("x", "y", "z")]), data$group)
        expected <- suppressWarnings(original(fit,
            permutations=permute::how(nperm=19), pairwise=pairwise))
        expect_equal(result$anova$asDF$p[[1L]], expected$tab[1L, "Pr(>F)"])
        expect_identical(.Random.seed, rng)
        if (pairwise) {
            expect_equal(result$pairwise$asDF$p, unname(expected$pairwise$permuted))
            expect_equal(result$pairwise$asDF$padj,
                unname(stats::p.adjust(expected$pairwise$permuted, "holm")))
        }
    }
})
