cluster_state_data <- function(n=18L, long_labels=FALSE) {
    index <- seq_len(n)
    labels <- if (isTRUE(long_labels))
        paste0("Sample_", index, "_", paste(rep("VeryLongLabel", 4L), collapse=""))
    else
        paste0("Sample ", index)
    data.frame(
        feature_01=(index * 3L) %% 17L + 1,
        feature_02=(index * 5L) %% 19L + 1,
        feature_03=(index * 7L) %% 23L + 1,
        feature_04=(index * 11L) %% 29L + 1,
        sample=factor(labels))
}

cluster_yaml_node <- function(node, name) {
    if (is.list(node) && identical(node$name, name))
        return(node)
    if (is.list(node)) {
        for (child in node) {
            found <- cluster_yaml_node(child, name)
            if (!is.null(found))
                return(found)
        }
    }
    NULL
}

expect_cluster_visibility <- function(result, visible, hidden) {
    for (name in visible)
        expect_true(result[[name]]$visible, info=paste(name, "should be visible"))
    for (name in hidden)
        expect_false(result[[name]]$visible, info=paste(name, "should be hidden"))
}

run_cluster_private <- function(data, ...) {
    options <- do.call(clusterOptions$new, list(...))
    analysis <- clusterClass$new(options=options, data=data)
    suppressWarnings(suppressMessages(analysis$run()))
    analysis
}

cluster_private <- function(analysis) {
    analysis$.__enclos_env__$private
}

test_that("cluster schema exposes a compact plots workflow", {
    analysis <- yaml::read_yaml(tofu_fixture_path("jamovi", "cluster.a.yaml"))
    ui <- yaml::read_yaml(tofu_fixture_path("jamovi", "cluster.u.yaml"))
    results <- yaml::read_yaml(
        tofu_fixture_path("jamovi", "cluster.r.yaml"))$items
    options <- analysis$options
    by_name <- setNames(options, vapply(options, `[[`, character(1), "name"))
    result_by_name <- setNames(
        results, vapply(results, `[[`, character(1), "name"))

    expect_identical(
        names(by_name),
        c(
            "data", "vars", "labels", "transform", "distance",
            "sampleLabels", "showLabels", "defineClusters", "cutMode",
            "numberClusters", "cutHeight"))
    expect_identical(by_name$vars$title, "Feature variables (required)")
    expect_identical(by_name$labels$title, "Sample labels (optional)")
    expect_identical(by_name$labels$permitted, c("numeric", "factor", "id"))
    expect_identical(by_name$sampleLabels$default, "auto")
    expect_identical(
        vapply(by_name$sampleLabels$options, `[[`, character(1), "name"),
        c("auto", "show", "hide"))
    expect_false(by_name$defineClusters$default)
    expect_identical(by_name$cutMode$default, "number")
    expect_identical(by_name$numberClusters$default, 3L)
    expect_identical(by_name$numberClusters$min, 2L)
    expect_equal(by_name$cutHeight$default, .5)
    expect_equal(by_name$cutHeight$min, 0)
    expect_identical(by_name$showLabels$type, "String")
    expect_identical(by_name$showLabels$default, "__tofu_unset__")
    expect_true(by_name$showLabels$hidden)
    expect_null(cluster_yaml_node(ui, "showLabels"))

    expect_false(cluster_yaml_node(ui, "analysisChoices")$collapsed)
    expect_false(cluster_yaml_node(ui, "plots")$collapsed)
    expect_identical(cluster_yaml_node(ui, "sampleLabels")$type, "ComboBox")
    expect_identical(cluster_yaml_node(ui, "defineClusters")$type, "CheckBox")
    expect_identical(cluster_yaml_node(ui, "numberClusters")$format, "number")
    expect_identical(cluster_yaml_node(ui, "cutHeight")$format, "number")

    expect_identical(
        names(result_by_name),
        c(
            "guidance", "summaryPurpose", "summary", "warnings",
            "dendrogramDescription", "dendrogram",
            "dendrogramStructurePurpose", "dendrogramStructure",
            "membershipPurpose", "membership", "interpretation",
            "settingsPurpose", "settings"))
    expect_true(all(vapply(
        results, function(item) identical(item$visible, FALSE), logical(1))))
    expect_identical(result_by_name$dendrogram$type, "Image")
    expect_identical(result_by_name$dendrogram$width, 600L)
    expect_identical(result_by_name$dendrogram$height, 500L)
    expect_lte(result_by_name$dendrogram$height, 650L)
    expect_identical(result_by_name$membership$type, "Table")
    expect_identical(result_by_name$dendrogramStructure$type, "Table")
    expect_identical(
        vapply(result_by_name$membership$columns, `[[`, character(1), "name"),
        c("sample", "cluster"))
})

test_that("cluster JS enables only the relevant cut control", {
    js <- paste(
        readLines(tofu_fixture_path("jamovi", "js", "cluster.js"), warn=FALSE),
        collapse="\n")
    expect_match(js, "ui.cutMode.setEnabled(defining)", fixed=TRUE)
    expect_match(
        js,
        "ui.numberClusters.setEnabled(defining && byNumber)",
        fixed=TRUE)
    expect_match(
        js,
        "ui.cutHeight.setEnabled(defining && !byNumber)",
        fixed=TRUE)
    expect_match(js, "ui.plots.expand()", fixed=TRUE)
    expect_match(js, "setTimeout(() => refreshView(ui), 100)", fixed=TRUE)
})

test_that("new cluster analysis shows only complete getting-started guidance", {
    options <- clusterOptions$new(vars=character())
    analysis <- clusterClass$new(options=options, data=cluster_state_data())
    cluster_private(analysis)$.run()
    result <- analysis$results

    expect_match(as.character(result$guidance$asString()), "max-width: 44em", fixed=TRUE)
    expect_match(as.character(result$guidance$asString()), "groups samples with similar")
    expect_match(as.character(result$guidance$asString()), "Feature variables")
    expect_cluster_visibility(
        result,
        visible="guidance",
        hidden=c(
            "summary", "warnings", "dendrogram", "dendrogramDescription",
            "membership", "interpretation", "settings"))
})

test_that("default cluster output is explicitly uncut", {
    analysis <- run_cluster_private(
        cluster_state_data(),
        vars=paste0("feature_0", 1:4),
        labels="sample")
    result <- analysis$results
    private <- cluster_private(analysis)

    expect_cluster_visibility(
        result,
        visible=c(
            "summary", "dendrogram", "dendrogramDescription",
            "interpretation", "settings"),
        hidden=c("guidance", "warnings", "membership"))
    expect_null(private$.state$membership)
    expect_null(private$.state$cutLine)
    expect_match(
        tofu_squish_result(result$dendrogramDescription),
        "merge into clusters as dissimilarity increases")
    expect_match(
        tofu_squish_result(result$interpretation),
        "Merge height shows dissimilarity")
    plot <- private$.buildDendrogram()
    expect_s3_class(plot, "ggplot")
    geom_classes <- vapply(
        plot$layers, function(layer) class(layer$geom)[[1L]], character(1))
    expect_identical(unname(geom_classes), "GeomSegment")
})

test_that("cluster fit is exact and invariant to display and cut options", {
    data <- cluster_state_data(120L)
    vars <- paste0("feature_0", 1:4)
    expected <- stats::hclust(
        vegan::vegdist(data[, vars, drop=FALSE] ^ .25, method="bray"),
        method="average")
    settings <- list(
        list(sampleLabels="auto", defineClusters=FALSE),
        list(sampleLabels="show", defineClusters=FALSE),
        list(sampleLabels="hide", defineClusters=TRUE, cutMode="number", numberClusters=5))

    fits <- lapply(settings, function(setting) {
        analysis <- do.call(
            run_cluster_private,
            c(list(
                data=data,
                vars=vars,
                labels="sample",
                transform="fourthroot",
                distance="bray"),
            setting))
        cluster_private(analysis)$.state$fit
    })
    for (fit in fits) {
        expect_identical(fit$merge, expected$merge)
        expect_equal(fit$height, expected$height, tolerance=0)
        expect_identical(fit$order, expected$order)
        expect_identical(fit$method, "average")
    }
    expect_identical(fits[[1L]]$merge, fits[[3L]]$merge)
    expect_equal(fits[[1L]]$height, fits[[3L]]$height, tolerance=0)
})

test_that("automatic sample labels use the documented forty-sample threshold", {
    cases <- list(
        list(n=40L, mode="auto", shown=TRUE, phrase="Sample labels are shown"),
        list(n=41L, mode="auto", shown=FALSE, phrase="automatically hidden"),
        list(n=90L, mode="show", shown=TRUE, phrase="Sample labels are shown"),
        list(n=12L, mode="hide", shown=FALSE, phrase="Sample labels are hidden"))
    for (case in cases) {
        analysis <- run_cluster_private(
            cluster_state_data(case$n),
            vars=paste0("feature_0", 1:4),
            labels="sample",
            sampleLabels=case$mode)
        private <- cluster_private(analysis)
        expect_identical(private$.state$showLabels, case$shown)
        plot <- private$.buildDendrogram()
        x_scale <- plot$scales$get_scales("x")
        if (case$shown)
            expect_length(x_scale$breaks, case$n)
        else
            expect_null(x_scale$breaks)
    }
})

test_that("legacy Boolean sample-label options survive constructor and saved decoding", {
    oldEnumExists <- exists(
        "jamovi.coms.AnalysisOption.Other",
        envir=.GlobalEnv,
        inherits=FALSE)
    if (oldEnumExists)
        oldEnum <- get(
            "jamovi.coms.AnalysisOption.Other",
            envir=.GlobalEnv,
            inherits=FALSE)
    assign(
        "jamovi.coms.AnalysisOption.Other",
        list("TRUE"=1L, "FALSE"=2L),
        envir=.GlobalEnv)
    on.exit({
        if (oldEnumExists)
            assign(
                "jamovi.coms.AnalysisOption.Other",
                oldEnum,
                envir=.GlobalEnv)
        else
            rm("jamovi.coms.AnalysisOption.Other", envir=.GlobalEnv)
    }, add=TRUE)

    cases <- list(
        list(n=12L, legacy=FALSE, shown=FALSE),
        list(n=90L, legacy=FALSE, shown=FALSE),
        list(n=12L, legacy=TRUE, shown=TRUE),
        list(n=90L, legacy=TRUE, shown=TRUE))

    for (case in cases) {
        constructor <- run_cluster_private(
            cluster_state_data(case$n),
            vars=paste0("feature_0", 1:4),
            labels="sample",
            showLabels=case$legacy)
        expect_identical(
            cluster_private(constructor)$.state$showLabels,
            case$shown)
        expect_identical(
            cluster_private(constructor)$.state$labelModeSource,
            "legacy")
        constructorBreaks <- cluster_private(constructor)$.buildDendrogram()$
            scales$get_scales("x")$breaks
        if (case$shown)
            expect_length(constructorBreaks, case$n)
        else
            expect_null(constructorBreaks)

        options <- clusterOptions$new(
            vars=paste0("feature_0", 1:4),
            labels="sample")
        legacyOption <- new.env(parent=emptyenv())
        legacyOption$o <- if (case$legacy) 1L else 2L
        legacyOption$has <- function(name) identical(name, "o")
        options$fromProtoBuf(list(
            names="showLabels",
            options=list(legacyOption)))
        decoded <- clusterClass$new(
            options=options,
            data=cluster_state_data(case$n))
        suppressWarnings(suppressMessages(decoded$run()))
        expect_identical(options$showLabels, case$legacy)
        expect_identical(
            cluster_private(decoded)$.state$showLabels,
            case$shown)
        expect_identical(
            cluster_private(decoded)$.state$labelModeSource,
            "legacy")
        decodedBreaks <- cluster_private(decoded)$.buildDendrogram()$
            scales$get_scales("x")$breaks
        if (case$shown)
            expect_length(decodedBreaks, case$n)
        else
            expect_null(decodedBreaks)
    }
})

test_that("new analyses retain automatic labels above and below the threshold", {
    small <- run_cluster_private(
        cluster_state_data(12L),
        vars=paste0("feature_0", 1:4),
        labels="sample")
    large <- run_cluster_private(
        cluster_state_data(90L),
        vars=paste0("feature_0", 1:4),
        labels="sample")

    expect_identical(small$options$showLabels, "__tofu_unset__")
    expect_identical(large$options$showLabels, "__tofu_unset__")
    expect_true(cluster_private(small)$.state$showLabels)
    expect_false(cluster_private(large)$.state$showLabels)
    expect_identical(
        cluster_private(small)$.state$labelModeSource,
        "current")
    expect_identical(
        cluster_private(large)$.state$labelModeSource,
        "current")
    expect_false(grepl(
        "inherited",
        tofu_squish_result(small$results$dendrogramDescription)))
})

test_that("current sample-label choices supersede legacy values", {
    cases <- list(
        list(current="hide", legacy=TRUE, shown=FALSE),
        list(current="show", legacy=FALSE, shown=TRUE))

    for (case in cases) {
        analysis <- run_cluster_private(
            cluster_state_data(60L),
            vars=paste0("feature_0", 1:4),
            labels="sample",
            sampleLabels=case$current,
            showLabels=case$legacy)
        private <- cluster_private(analysis)
        expect_identical(private$.state$labelMode, case$current)
        expect_identical(private$.state$labelModeSource, "current")
        expect_identical(private$.state$showLabels, case$shown)
        expect_false(grepl(
            "inherited",
            tofu_squish_result(
                analysis$results$dendrogramDescription)))
    }
})

test_that("saved current sample-label choices are not re-overridden", {
    oldEnumExists <- exists(
        "jamovi.coms.AnalysisOption.Other",
        envir=.GlobalEnv,
        inherits=FALSE)
    if (oldEnumExists)
        oldEnum <- get(
            "jamovi.coms.AnalysisOption.Other",
            envir=.GlobalEnv,
            inherits=FALSE)
    assign(
        "jamovi.coms.AnalysisOption.Other",
        list("TRUE"=1L, "FALSE"=2L),
        envir=.GlobalEnv)
    on.exit({
        if (oldEnumExists)
            assign(
                "jamovi.coms.AnalysisOption.Other",
                oldEnum,
                envir=.GlobalEnv)
        else
            rm("jamovi.coms.AnalysisOption.Other", envir=.GlobalEnv)
    }, add=TRUE)

    cases <- list(
        list(current="hide", legacy=TRUE, legacyPB=1L, shown=FALSE),
        list(current="show", legacy=FALSE, legacyPB=2L, shown=TRUE))
    for (case in cases) {
        legacyOption <- new.env(parent=emptyenv())
        legacyOption$o <- case$legacyPB
        legacyOption$has <- function(name) identical(name, "o")
        currentOption <- new.env(parent=emptyenv())
        currentOption$s <- case$current
        currentOption$has <- function(name) identical(name, "s")

        options <- clusterOptions$new(
            vars=paste0("feature_0", 1:4),
            labels="sample")
        options$fromProtoBuf(list(
            names=c("showLabels", "sampleLabels"),
            options=list(legacyOption, currentOption)))
        analysis <- clusterClass$new(
            options=options,
            data=cluster_state_data(60L))
        suppressWarnings(suppressMessages(analysis$run()))
        private <- cluster_private(analysis)

        expect_identical(options$showLabels, case$legacy)
        expect_identical(options$sampleLabels, case$current)
        expect_identical(private$.state$labelMode, case$current)
        expect_identical(private$.state$labelModeSource, "current")
        expect_identical(private$.state$showLabels, case$shown)
    }
})

test_that("changing legacy Automatic to Show or Hide takes effect immediately", {
    cases <- list(
        list(legacy=TRUE, current="hide", shown=FALSE),
        list(legacy=FALSE, current="show", shown=TRUE))

    for (case in cases) {
        options <- clusterOptions$new(
            vars=paste0("feature_0", 1:4),
            labels="sample",
            sampleLabels="auto",
            showLabels=case$legacy)
        analysis <- clusterClass$new(
            options=options,
            data=cluster_state_data(60L))
        suppressWarnings(suppressMessages(analysis$run()))
        expect_identical(
            cluster_private(analysis)$.state$labelModeSource,
            "legacy")

        sampleLabelsOption <- options$option("sampleLabels")
        sampleLabelsOption$value <- case$current
        suppressWarnings(suppressMessages(analysis$run()))
        private <- cluster_private(analysis)
        expect_identical(private$.state$labelMode, case$current)
        expect_identical(private$.state$labelModeSource, "current")
        expect_identical(private$.state$showLabels, case$shown)
        breaks <- private$.buildDendrogram()$scales$get_scales("x")$breaks
        if (case$shown)
            expect_length(breaks, 60L)
        else
            expect_null(breaks)
    }
})

test_that("dendrogram geometry matches a literal tied-height tree", {
    analysis <- run_cluster_private(
        cluster_state_data(4L),
        vars=paste0("feature_0", 1:4),
        labels="sample")
    private <- cluster_private(analysis)
    tiedFit <- structure(
        list(
            merge=matrix(
                c(-1L, -2L, -3L, -4L, 1L, 2L),
                ncol=2L,
                byrow=TRUE),
            height=c(1, 1, 2),
            order=c(3L, 4L, 1L, 2L),
            labels=c("A", "B", "C", "D"),
            method="average",
            call=quote(stats::hclust(d)),
            dist.method="euclidean"),
        class="hclust")
    geometry <- private$.dendrogramData(
        tiedFit,
        tiedFit$labels)

    expectedLeaves <- data.frame(
        sampleIndex=c(3L, 4L, 1L, 2L),
        x=c(1L, 2L, 3L, 4L),
        y=c(0, 0, 0, 0),
        fullLabel=c("C", "D", "A", "B"),
        plotLabel=c("C", "D", "A", "B"),
        stringsAsFactors=FALSE)
    expectedMergeNodes <- data.frame(
        node=c(5L, 6L, 7L),
        kind=c("merge", "merge", "merge"),
        merge=c(1L, 2L, 3L),
        x=c(3.5, 1.5, 2.5),
        y=c(1, 1, 2),
        stringsAsFactors=FALSE)
    row.names(expectedMergeNodes) <- 5:7
    expectedSegments <- data.frame(
        merge=c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L),
        segment=c(
            "vertical", "vertical", "horizontal",
            "vertical", "vertical", "horizontal",
            "vertical", "vertical", "horizontal"),
        child=c(1L, 2L, NA, 1L, 2L, NA, 1L, 2L, NA),
        x=c(3, 4, 3, 1, 2, 1, 3.5, 1.5, 1.5),
        xend=c(3, 4, 4, 1, 2, 2, 3.5, 1.5, 3.5),
        y=c(0, 0, 1, 0, 0, 1, 1, 1, 2),
        yend=c(1, 1, 1, 1, 1, 1, 2, 2, 2),
        stringsAsFactors=FALSE)

    expect_identical(geometry$leaf, expectedLeaves)
    expect_identical(geometry$nodes[5:7, ], expectedMergeNodes)
    expect_identical(geometry$segments, expectedSegments)

    plot <- private$.buildDendrogram(
        fit=tiedFit,
        labels=tiedFit$labels,
        showLabels=TRUE,
        membership=NULL,
        cutLine=NULL)
    expect_identical(plot$layers[[1L]]$data, expectedSegments)
    built <- ggplot2::ggplot_build(plot)
    expect_equal(
        unname(as.matrix(
            built$data[[1L]][, c("x", "xend", "y", "yend")])),
        unname(as.matrix(
            expectedSegments[, c("x", "xend", "y", "yend")])),
        tolerance=0)
})

test_that("number cuts match cutree and expose an actual cut line", {
    analysis <- run_cluster_private(
        cluster_state_data(24L),
        vars=paste0("feature_0", 1:4),
        labels="sample",
        defineClusters=TRUE,
        cutMode="number",
        numberClusters=4)
    private <- cluster_private(analysis)
    fit <- private$.state$fit
    expected <- stats::cutree(fit, k=4L)
    lower <- fit$height[[20L]]
    upper <- fit$height[[21L]]
    expectedLine <- if (upper > lower) mean(c(lower, upper)) else upper

    expect_identical(private$.state$membership, expected)
    expect_equal(private$.state$cutLine, expectedLine, tolerance=0)
    expect_true(analysis$results$membership$visible)
    table <- analysis$results$membership$asDF
    expect_identical(table$sample, private$.state$labels)
    expect_identical(table$cluster, as.integer(expected))
    plot <- private$.buildDendrogram()
    geom_classes <- vapply(
        plot$layers, function(layer) class(layer$geom)[[1L]], character(1))
    expect_true("GeomHline" %in% geom_classes)
    expect_true("GeomPoint" %in% geom_classes)
    expect_false(is.null(plot$scales$get_scales("colour")))
    expect_false(is.null(plot$scales$get_scales("shape")))
    hline <- plot$layers[[which(geom_classes == "GeomHline")]]
    expect_equal(hline$geom_params$na.rm, FALSE)
    expect_equal(hline$data$yintercept, expectedLine, tolerance=0)
    lineMembership <- stats::cutree(fit, h=private$.state$cutLine)
    expect_identical(
        match(lineMembership, unique(lineMembership)),
        match(expected, unique(expected)))
})

test_that("tied merges retain number membership without a misleading cut line", {
    analysis <- run_cluster_private(
        cluster_state_data(4L),
        vars=paste0("feature_0", 1:4),
        labels="sample",
        defineClusters=TRUE,
        cutMode="number",
        numberClusters=3)
    private <- cluster_private(analysis)
    tiedFit <- structure(
        list(
            merge=matrix(
                c(-1L, -2L, -3L, -4L, 1L, 2L),
                ncol=2L,
                byrow=TRUE),
            height=c(1, 1, 1.2071),
            order=seq_len(4L),
            labels=private$.state$labels,
            method="average",
            call=quote(stats::hclust(d)),
            dist.method="euclidean"),
        class="hclust")

    definition <- private$.clusterDefinition(tiedFit)
    expected <- stats::cutree(tiedFit, k=3L)
    expect_identical(unname(expected), c(1L, 1L, 2L, 3L))
    expect_identical(definition$membership, expected)
    expect_null(definition$cutLine)

    state <- private$.state
    state$fit <- tiedFit
    state$membership <- definition$membership
    state$cutLine <- definition$cutLine
    state$cutDescription <- definition$description
    private$.state <- state
    expect_match(definition$description,
        "no single horizontal cut height represents it")
    expect_match(definition$description, "because merges are tied")

    plot <- private$.buildDendrogram()
    geom_classes <- vapply(
        plot$layers, function(layer) class(layer$geom)[[1L]], character(1))
    expect_false("GeomHline" %in% geom_classes)
    expect_true("GeomPoint" %in% geom_classes)
})

test_that("height cuts match cutree exactly", {
    base <- run_cluster_private(
        cluster_state_data(28L),
        vars=paste0("feature_0", 1:4),
        labels="sample")
    fit <- cluster_private(base)$.state$fit
    height <- mean(fit$height[c(18L, 19L)])
    analysis <- run_cluster_private(
        cluster_state_data(28L),
        vars=paste0("feature_0", 1:4),
        labels="sample",
        defineClusters=TRUE,
        cutMode="height",
        cutHeight=height)
    private <- cluster_private(analysis)

    expect_identical(
        private$.state$membership,
        stats::cutree(private$.state$fit, h=height))
    expect_equal(private$.state$cutLine, height, tolerance=0)
    expect_equal(
        nrow(analysis$results$membership$asDF),
        nrow(cluster_state_data(28L)))
})

test_that("invalid cut settings clear membership without losing the valid dendrogram", {
    data <- cluster_state_data(18L)
    options <- clusterOptions$new(
        vars=paste0("feature_0", 1:4),
        labels="sample",
        defineClusters=TRUE,
        cutMode="number",
        numberClusters=3)
    analysis <- clusterClass$new(options=options, data=data)
    suppressWarnings(suppressMessages(analysis$run()))
    expect_true(analysis$results$membership$visible)

    numberOption <- options$option("numberClusters")
    numberOption$.__enclos_env__$private$.value <- 100
    suppressWarnings(suppressMessages(analysis$run()))
    expect_false(analysis$results$membership$visible)
    expect_equal(length(analysis$results$membership$rowKeys), 0L)
    expect_true(analysis$results$dendrogram$visible)
    expect_null(cluster_private(analysis)$.state$membership)
    expect_null(cluster_private(analysis)$.state$cutLine)
    expect_match(
        tofu_squish_result(analysis$results$warnings),
        "whole number from 2 to 18")

    numberOption$.__enclos_env__$private$.value <- 4
    suppressWarnings(suppressMessages(analysis$run()))
    expect_true(analysis$results$membership$visible)
    expect_equal(length(analysis$results$membership$rowKeys), 18L)

    cutModeOption <- options$option("cutMode")
    cutModeOption$.__enclos_env__$private$.value <- "height"
    heightOption <- options$option("cutHeight")
    heightOption$.__enclos_env__$private$.value <-
        max(cluster_private(analysis)$.state$fit$height) + 1
    suppressWarnings(suppressMessages(analysis$run()))
    expect_false(analysis$results$membership$visible)
    expect_equal(length(analysis$results$membership$rowKeys), 0L)
    expect_match(
        tofu_squish_result(analysis$results$warnings),
        "below")
})

test_that("full labels are preserved while plot labels are collision safe", {
    data <- cluster_state_data(16L)
    full <- paste0(
        "Shared twenty four character prefix for sample ",
        sprintf("%02d", seq_len(nrow(data))))
    data$sample <- factor(full, levels=full)
    analysis <- run_cluster_private(
        data,
        vars=paste0("feature_0", 1:4),
        labels="sample",
        sampleLabels="show",
        defineClusters=TRUE,
        numberClusters=3)
    private <- cluster_private(analysis)
    plotData <- private$.dendrogramData(
        private$.state$fit,
        private$.state$labels,
        private$.state$membership)

    expect_identical(analysis$results$membership$asDF$sample, full)
    expect_true(all(nchar(plotData$leaf$plotLabel) <= 24L))
    expect_length(unique(plotData$leaf$plotLabel), nrow(data))
    expect_match(
        tofu_squish_result(analysis$results$warnings),
        "full labels are retained")
})

test_that("missing and duplicate labels never change clustering", {
    data <- cluster_state_data(16L)
    raw <- as.character(data$sample)
    raw[2:3] <- "Repeated"
    raw[4] <- NA_character_
    data$sample <- factor(raw)
    vars <- paste0("feature_0", 1:4)
    labelled <- run_cluster_private(data, vars=vars, labels="sample")
    unlabelled <- run_cluster_private(data, vars=vars)
    labelled_private <- cluster_private(labelled)
    unlabelled_private <- cluster_private(unlabelled)

    expect_identical(labelled_private$.state$fit$merge, unlabelled_private$.state$fit$merge)
    expect_equal(labelled_private$.state$fit$height, unlabelled_private$.state$fit$height)
    expect_match(tofu_squish_result(labelled$results$warnings), "missing sample label")
    expect_match(tofu_squish_result(labelled$results$warnings), "Duplicate sample labels")
    expect_true(any(grepl("\\[row [23]\\]", labelled_private$.state$labels)))
    expect_true("Row 4" %in% labelled_private$.state$labels)
})

test_that("more than 64 defined clusters use the disclosed fallback", {
    analysis <- run_cluster_private(
        cluster_state_data(70L),
        vars=paste0("feature_0", 1:4),
        labels="sample",
        sampleLabels="auto",
        defineClusters=TRUE,
        cutMode="height",
        cutHeight=0)
    private <- cluster_private(analysis)
    clusterCount <- length(unique(private$.state$membership))

    expect_gt(clusterCount, 64L)
    expect_false(private$.state$clusterStyleAvailable)
    expect_false(private$.state$showLabels)
    plot <- private$.buildDendrogram()
    geom_classes <- vapply(
        plot$layers, function(layer) class(layer$geom)[[1L]], character(1))
    expect_true("GeomPoint" %in% geom_classes)
    expect_false("GeomText" %in% geom_classes)
})

test_that("nine clusters add visible numbers when shapes begin to repeat", {
    analysis <- run_cluster_private(
        cluster_state_data(9L),
        vars=paste0("feature_0", 1:4),
        labels="sample",
        defineClusters=TRUE,
        cutMode="height",
        cutHeight=0)
    private <- cluster_private(analysis)
    expect_identical(
        sort(unique(as.integer(private$.state$membership))),
        seq_len(9L))

    aesthetics <- .tofuGroupAesthetics(as.character(seq_len(9L)))
    expect_lt(length(unique(unname(aesthetics$shape))), 9L)
    plot <- private$.buildDendrogram()
    geom_classes <- vapply(
        plot$layers, function(layer) class(layer$geom)[[1L]], character(1))
    textIndex <- which(geom_classes == "GeomText")
    expect_length(textIndex, 1L)
    built <- ggplot2::ggplot_build(plot)
    expect_setequal(
        as.integer(built$data[[textIndex]]$label),
        seq_len(9L))
})

test_that("ggplot builder and callback render bounded read-only output", {
    analysis <- run_cluster_private(
        cluster_state_data(55L, long_labels=TRUE),
        vars=paste0("feature_0", 1:4),
        labels="sample",
        sampleLabels="show",
        defineClusters=TRUE,
        numberClusters=5)
    private <- cluster_private(analysis)
    before <- serialize(private$.state, NULL)
    plot <- private$.buildDendrogram()
    expect_s3_class(plot, "ggplot")
    expect_silent(ggplot2::ggplot_build(plot))

    path <- tempfile(fileext=".png")
    on.exit(unlink(path), add=TRUE)
    grDevices::png(path, width=600, height=500)
    expect_true(private$.plotDendrogram(NULL))
    grDevices::dev.off()

    expect_gt(file.info(path)$size, 1000)
    expect_identical(serialize(private$.state, NULL), before)
})

test_that("dendrogram structure is a complete exact table alternative", {
    data <- cluster_state_data(18L, long_labels=TRUE)
    analysis <- run_cluster_private(
        data,
        vars=paste0("feature_0", 1:4),
        labels="sample")
    private <- cluster_private(analysis)
    fit <- private$.state$fit
    structure <- analysis$results$dendrogramStructure$asDF
    leaves <- structure[structure$recordType == "Leaf", , drop=FALSE]
    merges <- structure[structure$recordType == "Merge", , drop=FALSE]

    expect_true(analysis$results$dendrogramStructure$visible)
    expect_identical(nrow(structure), 2L * length(fit$order) - 1L)
    expect_identical(leaves$displayOrder, seq_along(fit$order))
    expect_identical(leaves$sample, private$.state$labels[fit$order])
    expect_identical(merges$mergeStep, seq_len(nrow(fit$merge)))
    expect_equal(merges$height, fit$height, tolerance=0)
    expect_identical(
        analysis$results$dendrogramStructure$getCell(
            rowKey="1", col="mergeStep")$value,
        "")
    expect_identical(
        analysis$results$dendrogramStructure$getCell(
            rowKey="1", col="height")$value,
        "")
    expect_identical(
        analysis$results$dendrogramStructure$getCell(
            rowKey=as.character(length(fit$order) + 1L),
            col="displayOrder")$value,
        "")

    childLabel <- function(value) if (value < 0L)
        private$.state$labels[[-value]] else paste0("Merge ", value)
    expect_identical(merges$leftChild,
        vapply(fit$merge[, 1L], childLabel, character(1)))
    expect_identical(merges$rightChild,
        vapply(fit$merge[, 2L], childLabel, character(1)))
    expect_false(grepl("NaN", as.character(
        analysis$results$dendrogramStructure$asString())))
})

test_that("cluster valid invalid valid transitions clear stale state", {
    data <- cluster_state_data()
    options <- clusterOptions$new(vars=paste0("feature_0", 1:4))
    analysis <- clusterClass$new(options=options, data=data)

    suppressWarnings(suppressMessages(analysis$run()))
    expect_true(analysis$results$dendrogram$visible)
    expect_false(is.null(cluster_private(analysis)$.state$fit))

    varsOption <- options$option("vars")
    varsOption$.__enclos_env__$private$.value <- character()
    analysis$run()
    expect_true(analysis$results$guidance$visible)
    expect_false(analysis$results$dendrogram$visible)
    expect_false(analysis$results$dendrogramDescription$visible)
    expect_false(analysis$results$dendrogramStructure$visible)
    expect_false(analysis$results$membership$visible)
    expect_equal(length(analysis$results$summary$rowKeys), 0L)
    expect_equal(length(analysis$results$membership$rowKeys), 0L)
    expect_equal(length(analysis$results$dendrogramStructure$rowKeys), 0L)
    expect_null(cluster_private(analysis)$.state$fit)

    varsOption$.__enclos_env__$private$.value <- paste0("feature_0", 1:4)
    suppressWarnings(suppressMessages(analysis$run()))
    expect_false(analysis$results$guidance$visible)
    expect_true(analysis$results$dendrogram$visible)
    expect_false(is.null(cluster_private(analysis)$.state$fit))
})

test_that("cluster source contracts and fixtures stay byte identical", {
    root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
    if (!file.exists(file.path(root, "jamovi", "cluster.a.yaml")))
        skip("development source files are not present in the package-check tree")
    pairs <- list(
        c("jamovi/cluster.a.yaml", "jamovi/cluster.a.yaml"),
        c("jamovi/cluster.u.yaml", "jamovi/cluster.u.yaml"),
        c("jamovi/cluster.r.yaml", "jamovi/cluster.r.yaml"),
        c("jamovi/js/cluster.js", "jamovi/js/cluster.js"))
    for (pair in pairs) {
        source <- readBin(file.path(root, pair[[1L]]), "raw", n=1e6)
        fixture <- readBin(tofu_fixture_path(pair[[2L]]), "raw", n=1e6)
        expect_identical(source, fixture, info=pair[[1L]])
    }

    header <- paste(
        readLines(file.path(root, "R", "cluster.h.R"), warn=FALSE),
        collapse="\n")
    expect_match(header, "sampleLabels = \"auto\"", fixed=TRUE)
    expect_match(header, "showLabels = \"__tofu_unset__\"", fixed=TRUE)
    expect_match(header, "defineClusters = FALSE", fixed=TRUE)
    expect_match(header, "numberClusters = 3", fixed=TRUE)
    expect_match(header, "dendrogramDescription", fixed=TRUE)
    expect_match(header, "membership", fixed=TRUE)
})
