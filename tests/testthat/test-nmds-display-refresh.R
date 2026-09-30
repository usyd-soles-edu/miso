# Focused regressions for NMDS plot construction and cached display updates.

nmds_display_data <- function(n=18L, groups=3L) {
    index <- seq_len(n)
    group <- factor(rep(LETTERS[seq_len(groups)], length.out=n))
    data.frame(
        feature_01=1 + (index %% 5L) + as.integer(group),
        feature_02=2 + (index %% 7L) + 2 * as.integer(group),
        feature_03=3 + rev(index %% 6L) + as.integer(group),
        feature_04=1 + (index %% 3L) * as.integer(group),
        group=group,
        temperature=10 + index,
        pH=6 + (index %% 5L) / 10,
        check.names=FALSE)
}

run_nmds_display <- function(data, ...) {
    analysis <- nmdsClass$new(options=nmdsOptions$new(...), data=data)
    suppressWarnings(suppressMessages(
        analysis$.__enclos_env__$private$.run()))
    analysis
}

test_that("NMDS serialized fit cache restores tables and display changes", {
    skip_if_not_installed("RProtoBuf")
    RProtoBuf::readProtoFiles(
        file=system.file("jamovi.proto", package="jmvcore"))

    data <- nmds_display_data()
    options <- list(
        vars=paste0("feature_0", 1:4),
        factor="group",
        nmdsEnv=c("temperature", "pH"),
        nmdsSpecies=TRUE,
        nmdsShepard=TRUE,
        seed=123,
        nmdsTrymax=5)
    statePath <- tempfile()
    imagePaths <- c(
        tempfile("ordination-", fileext=".png"),
        tempfile("shepard-", fileext=".png"))
    on.exit(unlink(c(statePath, imagePaths)), add=TRUE)

    original <- nmdsClass$new(
        options=do.call(nmdsOptions$new, options),
        data=data,
        analysisId=1L)
    suppressMessages(original$run())
    originalPrivate <- original$.__enclos_env__$private
    originalFit <- originalPrivate$.state$fit
    originalEnvfit <- original$results$envfit$asDF
    expect_gt(nrow(originalEnvfit), 0L)
    expect_true(originalPrivate$.validAnalysisCache(
        original$results$analysisCache$state))
    expect_identical(original$results$analysisCache$visible, FALSE)

    for (path in imagePaths)
        expect_true(file.create(path))
    original$results$ordination$.setPath(imagePaths[[1L]])
    original$results$shepard$.setPath(imagePaths[[2L]])
    original$.setStatePathSource(function() statePath)
    original$.save()

    changedDisplay <- options
    changedDisplay$nmdsHull <- TRUE
    beforeInitLoad <- nmdsClass$new(
        options=do.call(nmdsOptions$new, changedDisplay),
        data=data,
        analysisId=1L)
    beforeInitLoad$.setStatePathSource(function() statePath)
    beforeInitLoad$init()
    beforeInitLoad$.load()
    beforeInitLoad$postInit()
    rngBefore <- .Random.seed
    suppressMessages(beforeInitLoad$run())
    restoredPrivate <- beforeInitLoad$.__enclos_env__$private
    expect_null(beforeInitLoad$results$ordination$.__enclos_env__$private$.filePath)
    expect_identical(
        beforeInitLoad$results$shepard$.__enclos_env__$private$.filePath,
        imagePaths[[2L]])
    expect_identical(restoredPrivate$.state$fit, originalFit)
    expect_identical(.Random.seed, rngBefore)
    expect_true(restoredPrivate$.state$overlays$effective[["hull"]])
    expect_identical(beforeInitLoad$results$envfit$asDF, originalEnvfit)
    expect_identical(beforeInitLoad$results$envfit$rowKeys,
        original$results$envfit$rowKeys)
    for (name in c("sites", "stress", "envfit", "features", "shepardPairs")) {
        expect_identical(
            beforeInitLoad$results[[name]]$rowKeys,
            original$results[[name]]$rowKeys,
            info=name)
        expect_identical(
            beforeInitLoad$results[[name]]$asDF,
            original$results[[name]]$asDF,
            info=name)
    }

    headerOnly <- nmdsClass$new(
        options=do.call(nmdsOptions$new, changedDisplay),
        data=data[0, , drop=FALSE],
        analysisId=1L)
    headerOnly$.setStatePathSource(function() statePath)
    headerOnly$init()
    headerPrivate <- headerOnly$.__enclos_env__$private
    headerPrivate$.dataProvided <- FALSE
    headerOnly$.load()
    headerOnly$postInit()

    expect_identical(headerPrivate$.state$fit, originalFit)
    for (name in c("sites", "stress", "envfit", "features", "shepardPairs")) {
        expect_identical(
            headerOnly$results[[name]]$rowKeys,
            original$results[[name]]$rowKeys,
            info=paste(name, "rows should be restored before run"))
        expect_identical(
            headerOnly$results[[name]]$asDF,
            original$results[[name]]$asDF,
            info=paste(name, "cells should be restored before run"))
    }
    expect_true(headerOnly$results$ordination$visible)
    expect_true(headerOnly$results$stress$visible)
    expect_true(headerOnly$results$sites$visible)
    expect_true(headerOnly$results$envfit$visible)
    expect_true(headerOnly$results$features$visible)
    expect_true(headerOnly$results$shepard$visible)
    expect_true(headerOnly$results$shepardPairs$visible)

    headerPrivate$.data <- data
    headerPrivate$.dataProvided <- TRUE
    rngBefore <- .Random.seed
    suppressMessages(headerPrivate$.run())
    expect_identical(headerPrivate$.state$fit, originalFit)
    expect_identical(.Random.seed, rngBefore)

    loadBeforeInit <- nmdsClass$new(
        options=do.call(nmdsOptions$new, changedDisplay),
        data=data,
        analysisId=1L)
    loadBeforeInit$.setStatePathSource(function() statePath)
    loadBeforeInit$.load()
    rngBefore <- .Random.seed
    suppressMessages(loadBeforeInit$run())
    expect_identical(
        loadBeforeInit$.__enclos_env__$private$.state$fit, originalFit)
    expect_identical(.Random.seed, rngBefore)
    expect_identical(loadBeforeInit$results$envfit$asDF, originalEnvfit)

    changedData <- data
    changedData$feature_01[[1L]] <- changedData$feature_01[[1L]] + 100
    changedHeader <- nmdsClass$new(
        options=do.call(nmdsOptions$new, options),
        data=data[0, , drop=FALSE],
        analysisId=1L)
    changedHeader$.setStatePathSource(function() statePath)
    changedHeader$init()
    changedPrivate <- changedHeader$.__enclos_env__$private
    changedPrivate$.dataProvided <- FALSE
    changedHeader$.load()
    changedHeader$postInit()
    expect_identical(changedPrivate$.state$fit, originalFit)
    expect_identical(
        changedHeader$results$sites$rowKeys,
        original$results$sites$rowKeys)
    changedPrivate$.data <- changedData
    changedPrivate$.dataProvided <- TRUE
    suppressMessages(changedPrivate$.run())
    expect_false(identical(changedPrivate$.state$prep$transformed,
        originalPrivate$.state$prep$transformed))
    expect_identical(
        changedHeader$results$analysisCache$state$key,
        changedPrivate$.structuralKey())

    changedOptions <- options
    changedOptions$nmdsK <- 3L
    optionMismatch <- nmdsClass$new(
        options=do.call(nmdsOptions$new, changedOptions),
        data=data[0, , drop=FALSE],
        analysisId=1L)
    optionMismatch$.setStatePathSource(function() statePath)
    optionMismatch$init()
    optionMismatch$.__enclos_env__$private$.dataProvided <- FALSE
    optionMismatch$.load()
    optionMismatch$postInit()
    expect_null(optionMismatch$.__enclos_env__$private$.state$fit)
    expect_false(optionMismatch$results$ordination$visible)

})

test_that("NMDS plot construction is independent of graphics devices", {
    sourceRoot <- normalizePath(test_path("..", ".."), mustWork=FALSE)
    if (!file.exists(file.path(sourceRoot, "R", "nmds.b.R")) ||
            !requireNamespace("pkgload", quietly=TRUE))
        skip("source checkout or pkgload is unavailable for the subprocess test")
    script <- tempfile(fileext=".R")
    defaultDevice <- tempfile(fileext=".pdf")
    activeDevice <- tempfile(fileext=".png")
    on.exit(unlink(c(script, defaultDevice, activeDevice)), add=TRUE)

    writeLines(c(
        "args <- commandArgs(trailingOnly=TRUE)",
        "pkgload::load_all(args[[1L]], quiet=TRUE)",
        "testBuilder <- function() {",
        "  originalDevices <- dev.list()",
        "  on.exit({",
        "    currentDevices <- dev.list()",
        "    opened <- setdiff(currentDevices, originalDevices)",
        "    for (device in rev(opened)) grDevices::dev.off(which=device)",
        "  }, add=TRUE)",
        "  i <- seq_len(18L)",
        "  data <- data.frame(a=1 + i %% 5L, b=2 + i %% 7L, c=3 + rev(i %% 6L), env=10 + i, group=factor(rep(letters[1:3], length.out=length(i))))",
        "  deviceOpened <- FALSE",
        "  options(device=function(...) { deviceOpened <<- TRUE; grDevices::pdf(file=args[[2L]], ...) })",
        "  options <- nmdsOptions$new(vars=c('a', 'b', 'c'), nmdsEnv='env', seed=123, nmdsTrymax=5)",
        "  analysis <- nmdsClass$new(options=options, data=data)",
        "  suppressMessages(analysis$.__enclos_env__$private$.run())",
        "  private <- analysis$.__enclos_env__$private",
        "  veganNamespace <- asNamespace('vegan')",
        "  originalOrdiellipse <- get('ordiellipse', envir=veganNamespace)",
        "  legacyOrdiellipse <- function(ord, groups, ..., kind='sd', draw='lines', col=NULL, border=NULL, lty=NULL, lwd=NULL) {",
        "    if (missing(col)) col <- graphics::par('col')",
        "    if (missing(border)) border <- graphics::par('col')",
        "    if (missing(lty)) lty <- graphics::par('lty')",
        "    if (missing(lwd)) lwd <- graphics::par('lwd')",
        "    do.call(originalOrdiellipse, c(list(ord, groups), list(...), list(kind=kind, draw=draw, col=col, border=border, lty=lty, lwd=lwd)))",
        "  }",
        "  unlockBinding('ordiellipse', veganNamespace)",
        "  assign('ordiellipse', legacyOrdiellipse, envir=veganNamespace)",
        "  lockBinding('ordiellipse', veganNamespace)",
        "  on.exit({ unlockBinding('ordiellipse', veganNamespace); assign('ordiellipse', originalOrdiellipse, envir=veganNamespace); lockBinding('ordiellipse', veganNamespace) }, add=TRUE)",
        "  beforeEllipse <- list(current=dev.cur(), devices=dev.list())",
        "  ellipseOptions <- nmdsOptions$new(vars=c('a', 'b', 'c'), factor='group', nmdsEllipse=TRUE, seed=123, nmdsTrymax=5)",
        "  ellipseAnalysis <- nmdsClass$new(options=ellipseOptions, data=data)",
        "  suppressMessages(ellipseAnalysis$.__enclos_env__$private$.run())",
        "  stopifnot(isTRUE(ellipseAnalysis$.__enclos_env__$private$.state$overlays$effective[['ellipse']]))",
        "  stopifnot(!deviceOpened, identical(dev.cur(), beforeEllipse$current), identical(dev.list(), beforeEllipse$devices))",
        "  before <- list(current=dev.cur(), devices=dev.list())",
        "  plot <- private$.buildNmdsPlot()",
        "  stopifnot(identical(dev.cur(), before$current), identical(dev.list(), before$devices))",
        "  plotData <- private$.nmdsPlotData(); plotData$vectorEndpoints <- NULL",
        "  private$.buildNmdsPlot(plotData)",
        "  stopifnot(identical(dev.cur(), before$current), identical(dev.list(), before$devices))",
        "  vectorLayer <- function(plot) {",
        "    which <- which(vapply(plot$layers, function(layer) is.data.frame(layer$data) && 'layer' %in% names(layer$data) && any(layer$data$layer == 'Environmental vector'), logical(1)))",
        "    plot$layers[[which[[1L]]]]$data",
        "  }",
        "  first <- vectorLayer(plot)",
        "  stopifnot(nrow(first) > 0L, all(is.finite(first$xend)), all(is.finite(first$yend)))",
        "  grDevices::png(args[[3L]], width=800, height=600)",
        "  active <- dev.cur(); devices <- dev.list()",
        "  graphics::par(usr=c(-2, 2, -4, 4))",
        "  firstActive <- vectorLayer(private$.buildNmdsPlot())",
        "  stopifnot(identical(dev.cur(), active), identical(dev.list(), devices))",
        "  graphics::par(usr=c(-200, 200, -30, 30))",
        "  secondActive <- vectorLayer(private$.buildNmdsPlot())",
        "  stopifnot(identical(dev.cur(), active), identical(dev.list(), devices))",
        "  coordinates <- function(data) data[c('xend', 'yend')]",
        "  stopifnot(isTRUE(all.equal(coordinates(first), coordinates(firstActive), tolerance=0)))",
        "  stopifnot(isTRUE(all.equal(coordinates(firstActive), coordinates(secondActive), tolerance=0)))",
        "  edgeData <- private$.nmdsPlotData()",
        "  edgeData$sites[,] <- 0",
        "  edgeData$vectorEndpoints <- rbind(east=c(1, 0), north=c(0, 2), zero=c(0, 0), invalid=c(NA, Inf))",
        "  edgeData$vectorLabelSelection <- list(shown=1:4, omitted=integer())",
        "  edgePlot <- private$.buildNmdsPlot(edgeData)",
        "  edgeVectors <- vectorLayer(edgePlot)",
        "  stopifnot(all(is.finite(edgeVectors$xend)), all(is.finite(edgeVectors$yend)))",
        "  stopifnot(identical(edgeVectors$label, c('east', 'north', 'zero'))) ",
        "  stopifnot(edgeVectors$xend[[1L]] > 0, edgeVectors$yend[[1L]] == 0, edgeVectors$xend[[2L]] == 0, edgeVectors$yend[[2L]] > 0)",
        "  stopifnot(isTRUE(all.equal(edgeVectors$yend[[2L]] / edgeVectors$xend[[1L]], 2, tolerance=1e-12)))",
        "  stopifnot(identical(edgeVectors$xend[[3L]], 0), identical(edgeVectors$yend[[3L]], 0))",
        "  stopifnot(identical(dev.cur(), active), identical(dev.list(), devices))",
        "  invisible(TRUE)",
        "}",
        "testBuilder()"), script)

    output <- suppressWarnings(system2(
        file.path(R.home("bin"), "Rscript"),
        args=c(shQuote(script), shQuote(sourceRoot),
            shQuote(defaultDevice), shQuote(activeDevice)),
        stdout=TRUE, stderr=TRUE))
    status <- attr(output, "status")
    if (is.null(status))
        status <- 0L
    expect_identical(status, 0L, info=paste(output, collapse="\n"))
})

test_that("NMDS group display refresh preserves fit, tables, and Shepard state", {
    analysis <- run_nmds_display(
        nmds_display_data(),
        vars=paste0("feature_0", 1:4),
        factor="group",
        nmdsEnv=c("temperature", "pH"),
        nmdsSpecies=TRUE,
        nmdsShepard=TRUE,
        seed=0,
        nmdsTrymax=5)
    private <- analysis$.__enclos_env__$private
    fit <- private$.state$fit
    baseWarnings <- private$.state$baseWarnings
    tables <- c("sites", "stress", "envfit", "features", "shepardPairs")
    tableState <- lapply(tables, function(name) list(
        rows=analysis$results[[name]]$rowKeys,
        values=analysis$results[[name]]$asDF,
        firstCell=miso_table_first_cell(analysis$results[[name]])))
    names(tableState) <- tables
    shepardState <- analysis$results$shepard$state
    shepardCell <- miso_table_first_cell(analysis$results$shepardPairs)

    expectedEffective <- c(
        nmdsOverlay=FALSE, nmdsHull=TRUE,
        nmdsEllipse=TRUE, nmdsSpider=TRUE)
    for (name in names(expectedEffective)) {
        imagePaths <- c(tempfile("ordination-"), tempfile("shepard-"))
        expect_true(all(file.create(imagePaths)))
        analysis$results$ordination$.setPath(imagePaths[[1L]])
        analysis$results$shepard$.setPath(imagePaths[[2L]])
        option <- analysis$options$option(name)
        option$.__enclos_env__$private$.value <- !option$value
        ordinationBefore <- analysis$results$ordination$state
        set.seed(88231L)
        rngBefore <- .Random.seed
        suppressMessages(private$.run())

        expect_identical(private$.state$fit, fit, info=name)
        expect_identical(private$.state$baseWarnings, baseWarnings, info=name)
        expect_identical(.Random.seed, rngBefore, info=name)
        expect_false(identical(
            analysis$results$ordination$state, ordinationBefore), info=name)
        expect_null(
            analysis$results$ordination$.__enclos_env__$private$.filePath,
            info=paste(name, "invalidates ordination image"))
        expect_identical(
            analysis$results$shepard$.__enclos_env__$private$.filePath,
            imagePaths[[2L]],
            info=paste(name, "preserves Shepard image"))
        layer <- switch(name,
            nmdsOverlay="points", nmdsHull="hull",
            nmdsEllipse="ellipse", nmdsSpider="spider")
        expect_identical(
            private$.state$overlays$effective[[layer]],
            expectedEffective[[name]], info=name)
        expect_identical(
            analysis$results$ordination$state$overlays$effective[[layer]],
            expectedEffective[[name]], info=paste(name, "plot state"))
        expect_identical(analysis$results$shepard$state, shepardState, info=name)
        expect_identical(
            miso_table_first_cell(analysis$results$shepardPairs),
            shepardCell,
            info=name)
        for (table in tables) {
            expect_identical(
                analysis$results[[table]]$rowKeys,
                tableState[[table]]$rows,
                info=paste(name, table, "rows"))
            expect_equal(
                analysis$results[[table]]$asDF,
                tableState[[table]]$values,
                tolerance=0,
                info=paste(name, table, "values"))
            expect_identical(
                miso_table_first_cell(analysis$results[[table]]),
                tableState[[table]]$firstCell,
                info=paste(name, table, "cell"))
        }
        unlink(imagePaths)
    }
})

test_that("NMDS Shepard visibility uses cached data without resetting either plot", {
    analysis <- run_nmds_display(
        nmds_display_data(),
        vars=paste0("feature_0", 1:4),
        factor="group",
        nmdsShepard=TRUE,
        seed=0,
        nmdsTrymax=5)
    private <- analysis$.__enclos_env__$private
    ordinationState <- analysis$results$ordination$state
    shepardState <- analysis$results$shepard$state
    pairCell <- miso_table_first_cell(analysis$results$shepardPairs)
    shepardOption <- analysis$options$option("nmdsShepard")

    shepardOption$.__enclos_env__$private$.value <- FALSE
    suppressMessages(private$.run())
    expect_false(analysis$results$shepard$visible)
    expect_identical(analysis$results$ordination$state, ordinationState)
    expect_identical(analysis$results$shepard$state, shepardState)
    expect_gt(length(analysis$results$shepardPairs$rowKeys), 0L)
    expect_identical(
        miso_table_first_cell(analysis$results$shepardPairs), pairCell)

    shepardOption$.__enclos_env__$private$.value <- TRUE
    suppressMessages(private$.run())
    expect_true(analysis$results$shepard$visible)
    expect_identical(analysis$results$ordination$state, ordinationState)
    expect_identical(analysis$results$shepard$state, shepardState)
    expect_identical(
        miso_table_first_cell(analysis$results$shepardPairs), pairCell)

    ordinationState <- analysis$results$ordination$state
    hullOption <- analysis$options$option("nmdsHull")
    hullOption$.__enclos_env__$private$.value <- TRUE
    shepardOption$.__enclos_env__$private$.value <- FALSE
    set.seed(74921L)
    rngBefore <- .Random.seed
    suppressMessages(private$.run())
    expect_false(identical(analysis$results$ordination$state, ordinationState))
    expect_identical(analysis$results$shepard$state, shepardState)
    expect_identical(.Random.seed, rngBefore)
    expect_identical(
        miso_table_first_cell(analysis$results$shepardPairs), pairCell)

    ordinationState <- analysis$results$ordination$state
    ellipseOption <- analysis$options$option("nmdsEllipse")
    ellipseOption$.__enclos_env__$private$.value <- TRUE
    shepardOption$.__enclos_env__$private$.value <- TRUE
    set.seed(95172L)
    rngBefore <- .Random.seed
    suppressMessages(private$.run())
    expect_true(analysis$results$shepard$visible)
    expect_false(identical(analysis$results$ordination$state, ordinationState))
    expect_identical(analysis$results$shepard$state, shepardState)
    expect_identical(.Random.seed, rngBefore)
    expect_identical(
        miso_table_first_cell(analysis$results$shepardPairs), pairCell)

    ordinationState <- analysis$results$ordination$state
    displayWarningHtml <- private$.state$warningHtml
    set.seed(19751L)
    rngBefore <- .Random.seed
    suppressMessages(private$.run())
    expect_identical(analysis$results$ordination$state, ordinationState)
    expect_identical(private$.state$warningHtml, displayWarningHtml)
    expect_identical(.Random.seed, rngBefore)
    expect_identical(
        miso_table_first_cell(analysis$results$shepardPairs), pairCell)
})

test_that("NMDS first Shepard display populates from a cached fit", {
    analysis <- run_nmds_display(
        nmds_display_data(),
        vars=paste0("feature_0", 1:4),
        factor="group",
        nmdsShepard=FALSE,
        seed=0,
        nmdsTrymax=5)
    private <- analysis$.__enclos_env__$private
    fit <- private$.state$fit
    ordinationState <- analysis$results$ordination$state
    shepardOption <- analysis$options$option("nmdsShepard")
    expect_length(analysis$results$shepardPairs$rowKeys, 0L)

    shepardOption$.__enclos_env__$private$.value <- TRUE
    suppressMessages(private$.run())
    expect_identical(private$.state$fit, fit)
    expect_identical(analysis$results$ordination$state, ordinationState)
    expect_true(analysis$results$shepard$visible)
    expect_gt(length(analysis$results$shepardPairs$rowKeys), 0L)
})
