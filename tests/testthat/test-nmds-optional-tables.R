optional_nmds <- function(...) {
    i <- seq_len(18L)
    data <- data.frame(a=1+i%%5, b=2+i%%7, c=3+i%%3)
    analysis <- nmdsClass$new(options=nmdsOptions$new(
        vars=c("a", "b", "c"), seed=123, nmdsTrymax=3, ...), data=data, analysisId=1L)
    suppressMessages(analysis$run())
    analysis
}

optional_nmds_set <- function(analysis, name, value) {
    option <- analysis$options$option(name)
    option$value <- value
    analysis$optionsChangedHandler(name)
    suppressMessages(analysis$run())
}

test_that("NMDS default output hides long tables but retains values and plots", {
    analysis <- optional_nmds(nmdsSpecies=TRUE)
    for (name in c("sites", "features", "shepardPairs")) {
        expect_false(analysis$results[[name]]$visible, info=name)
        expect_gt(nrow(analysis$results[[name]]$asDF), 0L)
    }
    expect_true(analysis$results$ordination$visible)
    expect_true(analysis$results$stress$visible)
    expect_true(analysis$results$shepard$visible)
})

test_that("NMDS table toggles preserve the fit, RNG and plot resources", {
    analysis <- optional_nmds(nmdsSpecies=TRUE)
    private <- analysis$.__enclos_env__$private
    fit <- private$.state$fit
    rng <- .Random.seed
    paths <- c(tempfile(), tempfile())
    on.exit(unlink(paths), add=TRUE)
    for (path in paths) file.create(path)
    analysis$results$ordination$.setPath(paths[[1]])
    analysis$results$shepard$.setPath(paths[[2]])
    plotStates <- lapply(c("ordination", "shepard"), function(name)
        analysis$results[[name]]$state)
    tables <- c(nmdsSiteTable="sites", nmdsFeatureTable="features",
        nmdsShepardTable="shepardPairs")
    values <- lapply(unname(tables), function(name) analysis$results[[name]]$asDF)
    for (option in names(tables)) {
        for (shown in c(TRUE, FALSE, TRUE)) {
            optional_nmds_set(analysis, option, shown)
            expect_identical(analysis$results[[tables[[option]]]]$visible, shown)
            expect_identical(private$.state$fit, fit)
            expect_identical(.Random.seed, rng)
            expect_identical(lapply(unname(tables), function(name)
                analysis$results[[name]]$asDF), values)
            expect_identical(lapply(c("ordination", "shepard"), function(name)
                analysis$results[[name]]$state), plotStates)
            expect_identical(analysis$results$ordination$.__enclos_env__$private$.filePath,
                paths[[1]])
            expect_identical(analysis$results$shepard$.__enclos_env__$private$.filePath,
                paths[[2]])
        }
    }
})

test_that("Shepard numerical table and plot have independent controls", {
    for (plot in c(FALSE, TRUE)) for (table in c(FALSE, TRUE)) {
        analysis <- optional_nmds(nmdsShepard=plot, nmdsShepardTable=table)
        expect_identical(analysis$results$shepard$visible, plot)
        expect_identical(analysis$results$shepardPairs$visible, table)
        if (table) expect_gt(nrow(analysis$results$shepardPairs$asDF), 0L)
    }
    analysis <- optional_nmds(nmdsShepard=FALSE)
    fit <- analysis$.__enclos_env__$private$.state$fit
    optional_nmds_set(analysis, "nmdsShepardTable", TRUE)
    expect_true(analysis$results$shepardPairs$visible)
    expect_false(analysis$results$shepard$visible)
    expect_identical(analysis$.__enclos_env__$private$.state$fit, fit)
})

test_that("Feature table requires available feature scores", {
    analysis <- optional_nmds(nmdsFeatureTable=TRUE, nmdsSpecies=FALSE)
    expect_false(analysis$results$features$visible)
    expect_true(analysis$options$nmdsFeatureTable)
})

test_that("NMDS saved table selections restore with header-only data", {
    skip_if_not_installed("RProtoBuf")
    RProtoBuf::readProtoFiles(file=system.file("jamovi.proto", package="jmvcore"))
    for (shown in c(FALSE, TRUE)) {
        analysis <- optional_nmds(nmdsSpecies=TRUE, nmdsSiteTable=shown,
            nmdsFeatureTable=shown, nmdsShepardTable=shown)
        path <- tempfile()
        on.exit(unlink(path), add=TRUE)
        analysis$.setStatePathSource(function() path)
        analysis$.save()
        # Standalone R options have no incoming UI protobuf. Round-trip the
        # persisted table switches explicitly alongside the structural settings.
        flags <- c("nmdsSiteTable", "nmdsFeatureTable", "nmdsShepardTable")
        optionPB <- RProtoBuf::new(RProtoBuf::P("jamovi.coms.AnalysisOptions"))
        optionPB$hasNames <- TRUE
        optionPB$names <- flags
        optionPB$options <- lapply(flags, function(name) {
            value <- RProtoBuf::new(RProtoBuf::P("jamovi.coms.AnalysisOption"))
            value$o <- as.integer(shown)
            value
        })
        options <- nmdsOptions$new(vars=c("a", "b", "c"), seed=123,
            nmdsTrymax=3, nmdsSpecies=TRUE)
        options$fromProtoBuf(RProtoBuf::read(
            RProtoBuf::P("jamovi.coms.AnalysisOptions"), optionPB$serialize(NULL)))
        restored <- nmdsClass$new(options=options, data=data.frame(a=numeric(), b=numeric(), c=numeric()),
            analysisId=1L)
        restored$.setStatePathSource(function() path)
        restored$init()
        restored$.__enclos_env__$private$.dataProvided <- FALSE
        restored$.load()
        restored$postInit()
        for (name in c("sites", "features", "shepardPairs")) {
            expect_identical(restored$results[[name]]$visible, shown, info=name)
            expect_identical(restored$results[[name]]$asDF,
                analysis$results[[name]]$asDF, info=name)
        }
        expect_identical(restored$.__enclos_env__$private$.state$fit,
            analysis$.__enclos_env__$private$.state$fit)
    }
})

test_that("Legacy NMDS options hide tables without invalidating the cached fit", {
    skip_if_not_installed("RProtoBuf")
    RProtoBuf::readProtoFiles(file=system.file("jamovi.proto", package="jmvcore"))
    original <- optional_nmds(nmdsSpecies=TRUE, nmdsSiteTable=TRUE,
        nmdsFeatureTable=TRUE, nmdsShepardTable=TRUE)
    options <- nmdsOptions$new(vars=c("a", "b", "c"), seed=123,
        nmdsTrymax=3, nmdsSpecies=TRUE)
    # Legacy messages omit the newly introduced table switches.
    options$fromProtoBuf(RProtoBuf::new(RProtoBuf::P("jamovi.coms.AnalysisOptions")))
    restored <- nmdsClass$new(options=options,
        data=data.frame(a=numeric(), b=numeric(), c=numeric()), analysisId=1L)
    restored$results$analysisCache$setState(original$results$analysisCache$state)
    restored$.__enclos_env__$private$.dataProvided <- FALSE
    rng <- .Random.seed
    restored$postInit()
    for (name in c("sites", "features", "shepardPairs"))
        expect_false(restored$results[[name]]$visible)
    expect_identical(restored$.__enclos_env__$private$.state$fit,
        original$.__enclos_env__$private$.state$fit)
    expect_identical(.Random.seed, rng)
})
