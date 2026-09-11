permdisp_state_data <- function(two_groups=FALSE) {
    data <- data.frame(
        sp1=c(1, 2, 1, 7, 8, 7, 3, 4, 3),
        sp2=c(2, 1, 2, 8, 7, 8, 4, 3, 4),
        sp3=c(1, 1, 2, 6, 7, 6, 3, 3, 2),
        group=factor(rep(c("A", "B", "C"), each=3))
    )
    if (isTRUE(two_groups))
        data <- droplevels(data[data$group != "C", , drop=FALSE])
    data
}

permdisp_test_fit <- function(data=permdisp_state_data(), centre="median") {
    suppressWarnings(vegan::betadisper(
        vegan::vegdist(data[c("sp1", "sp2", "sp3")], method="bray"),
        data$group,
        type=centre))
}

permdisp_populate_test_ordination <- function(ordination) {
    options <- permdispOptions$new(
        vars=c("sp1", "sp2", "sp3"),
        factor="group",
        showOrdinationPlot=TRUE)
    analysis <- permdispClass$new(
        options=options, data=permdisp_state_data())
    private <- analysis$.__enclos_env__$private
    private$.clearResults()
    private$.state <- list(
        ordination=ordination,
        distanceDiagnostic=NULL)
    private$.populateOrdination()
    private$.showSuccessfulResults(FALSE)
    analysis
}

expect_permdisp_visibility <- function(result, visible, hidden) {
    for (name in visible)
        expect_true(result[[name]]$visible, info=paste(name, "should be visible"))
    fixed <- c("anova", "distances")
    for (name in hidden)
        if (name %in% fixed)
            expect_true(result[[name]]$visible, info=paste(name, "fixed shell should remain visible"))
        else
            expect_false(result[[name]]$visible, info=paste(name, "should be hidden"))
}

find_permdisp_yaml_node <- function(node, name) {
    if (is.list(node) && identical(node$name, name))
        return(node)
    if (is.list(node)) {
        for (child in node) {
            found <- find_permdisp_yaml_node(child, name)
            if (! is.null(found))
                return(found)
        }
    }
    NULL
}

test_that("PERMDISP schema follows the required-first option hierarchy", {
    options <- yaml::read_yaml(miso_fixture_path("jamovi", "permdisp.a.yaml"))$options
    ui <- yaml::read_yaml(miso_fixture_path("jamovi", "permdisp.u.yaml"))
    by_name <- setNames(options, vapply(options, `[[`, character(1), "name"))

    expect_identical(by_name$vars$title, "Feature Variables")
    expect_identical(by_name$factor$title, "Grouping Variable")
    expect_identical(by_name$permRestriction$default, "free")
    expect_identical(
        vapply(by_name$permRestriction$options, `[[`, character(1), "name"),
        c("free", "series")
    )
    expect_true(by_name$permScheme$hidden)
    expect_identical(by_name$permScheme$default, "free")
    expect_identical(by_name$dispType$default, "median")
    expect_identical(by_name$dispAdjust$default, "holm")
    expect_true(by_name$showDistancePlot$default)
    expect_false(by_name$showOrdinationPlot$default)

    expect_false(find_permdisp_yaml_node(ui, "analysisChoices")$collapsed)
    expect_false(find_permdisp_yaml_node(ui, "plots")$collapsed)
    expect_true(find_permdisp_yaml_node(ui, "advancedOptions")$collapsed)
    expect_true(find_permdisp_yaml_node(ui, "reproducibility")$collapsed)
    expect_false(is.null(find_permdisp_yaml_node(ui, "permRestriction")))
    expect_true(is.null(find_permdisp_yaml_node(ui, "permScheme")))
})

test_that("PERMDISP UI nests adjustment under pairwise comparisons", {
    ui <- yaml::read_yaml(miso_fixture_path("jamovi", "permdisp.u.yaml"))
    pairwise <- find_permdisp_yaml_node(ui, "dispPairwise")
    expect_identical(pairwise$style, "list")
    expect_identical(pairwise$children[[1L]]$name, "dispAdjust")
    expect_identical(pairwise$children[[1L]]$enable, "(dispPairwise)")
})

test_that("PERMDISP UI dependencies and progressive disclosure are explicit", {
    source <- paste(
        readLines(miso_fixture_path("jamovi", "js", "permdisp.js")),
        collapse="\n")

    expect_match(
        source,
        "dispAdjust\\.setEnabled\\(ui\\.dispPairwise\\.value\\(\\)\\)")
    expect_match(source, "advancedOptions\\.expand\\(\\)")
    expect_match(source, "reproducibility\\.expand\\(\\)")
    expect_match(source, "permRestriction\\.value\\(\\) !== 'free'")
})

test_that("PERMDISP result schema hides every empty shell", {
    results <- yaml::read_yaml(
        miso_fixture_path("jamovi", "permdisp.r.yaml"))$items
    by_name <- setNames(results, vapply(results, `[[`, character(1), "name"))

    expect_true(all(vapply(results, function(item) identical(item$visible, FALSE), logical(1))))
    expect_identical(by_name$guidance$type, "Html")
    expect_identical(by_name$warnings$type, "Html")
    expect_identical(
        vapply(by_name$distances$columns, `[[`, character(1), "name"),
        c(
            "group", "n", "centre", "distance", "median", "q1", "q3",
            "sd", "min", "max")
    )
    expect_identical(by_name$plot$type, "Image")
    expect_identical(by_name$plot$width, 600L)
    expect_identical(by_name$plot$height, 460L)
    expect_identical(by_name$plotDescription$type, "Html")
    expect_identical(by_name$ordinationPlot$type, "Image")
    expect_identical(by_name$ordinationPlot$width, 600L)
    expect_identical(by_name$ordinationPlot$height, 500L)
    expect_identical(by_name$ordinationDescription$type, "Html")
    expect_identical(by_name$ordinationScores$type, "Table")
    expect_identical(
        vapply(by_name$ordinationScores$columns, `[[`, character(1), "name"),
        c("point", "pointType", "group", "plotKey", "axis1", "axis2"))
    expect_identical(by_name$anova$columns[[6L]]$title, "Permutation p")
    expect_identical(by_name$pairwise$title, "Pairwise Dispersion Comparisons")
})

test_that("new PERMDISP shows only complete getting-started guidance", {
    options <- permdispOptions$new(
        vars=character(),
        factor=NULL)
    analysis <- permdispClass$new(options=options, data=permdisp_state_data())
    analysis$.__enclos_env__$private$.run()
    result <- analysis$results

    expect_match(as.character(result$guidance$asString()), "max-width: 44em", fixed=TRUE)
    expect_match(as.character(result$guidance$asString()), "PERMDISP tests whether groups differ in multivariate spread")
    expect_match(as.character(result$guidance$asString()), "Feature variables")
    expect_match(as.character(result$guidance$asString()), "Grouping variable")
    expect_permdisp_visibility(
        result,
        visible="guidance",
        hidden=c(
            "warnings", "distances", "anova", "pairwise", "plot",
            "plotDescription", "ordinationPlot", "ordinationDescription",
            "ordinationScores")
    )
})

test_that("incomplete PERMDISP inputs show one actionable correction", {
    features_only <- permdisp(
        data=permdisp_state_data(),
        vars=c("sp1", "sp2", "sp3"),
        factor=NULL
    )
    expect_match(as.character(features_only$guidance$asString()), "Grouping variable")
    expect_permdisp_visibility(
        features_only,
        visible="guidance",
        hidden=c(
            "warnings", "distances", "anova", "pairwise", "plot",
            "plotDescription", "ordinationPlot", "ordinationDescription",
            "ordinationScores")
    )

    group_only <- permdisp(
        data=permdisp_state_data(),
        vars=character(),
        factor="group"
    )
    expect_match(as.character(group_only$guidance$asString()), "Feature variables")
    expect_permdisp_visibility(
        group_only,
        visible="guidance",
        hidden=c(
            "warnings", "distances", "anova", "pairwise", "plot",
            "plotDescription", "ordinationPlot", "ordinationDescription",
            "ordinationScores")
    )
})

test_that("PERMDISP preparation failure hides every result shell", {
    data <- permdisp_state_data()
    data$sp1[[1L]] <- -1
    result <- permdisp(
        data=data,
        vars=c("sp1", "sp2", "sp3"),
        factor="group"
    )

    expect_match(as.character(result$guidance$asString()), "Negative values")
    expect_permdisp_visibility(
        result,
        visible="guidance",
        hidden=c(
            "warnings", "distances", "anova", "pairwise", "plot",
            "plotDescription", "ordinationPlot", "ordinationDescription",
            "ordinationScores")
    )
})

test_that("successful PERMDISP hides guidance warnings and pairwise shells", {
    result <- suppressWarnings(suppressMessages(permdisp(
        data=permdisp_state_data(),
        vars=c("sp1", "sp2", "sp3"),
        factor="group",
        permN=19,
        seed=123
    )))

    expect_permdisp_visibility(
        result,
        visible=c(
            "distances", "anova", "plot"),
        hidden=c(
            "guidance", "warnings", "pairwise", "plotDescription", "ordinationPlot",
            "ordinationDescription", "ordinationScores")
    )
})


test_that("PERMDISP pairwise transitions remove stale pairwise rows", {
    data <- permdisp_state_data()
    options <- permdispOptions$new(
        vars=c("sp1", "sp2", "sp3"),
        factor="group",
        dispPairwise=TRUE,
        permN=19,
        seed=123)
    analysis <- permdispClass$new(options=options, data=data)

    suppressWarnings(suppressMessages(analysis$run()))
    expect_true(analysis$results$pairwise$visible)
    expect_gt(length(analysis$results$pairwise$rowKeys), 0L)

    pairwise_option <- options$option("dispPairwise")
    pairwise_option$.__enclos_env__$private$.value <- FALSE
    suppressWarnings(suppressMessages(analysis$run()))
    expect_false(analysis$results$pairwise$visible)
    expect_equal(length(analysis$results$pairwise$rowKeys), 0L)
})


test_that("PERMDISP preprocessing warnings accompany valid results", {
    data <- permdisp_state_data()
    data$sp1[[1L]] <- NA_real_
    result <- suppressWarnings(suppressMessages(permdisp(
        data=data,
        vars=c("sp1", "sp2", "sp3"),
        factor="group",
        permN=19,
        seed=123
    )))

    expect_match(as.character(result$warnings$asString()), "rows excluded")
    expect_permdisp_visibility(
        result,
        visible=c(
            "warnings", "distances", "anova", "plot"),
        hidden=c(
            "guidance", "pairwise", "ordinationPlot",
            "ordinationDescription", "ordinationScores")
    )
})

test_that("PERMDISP structural cells are blank but export as missing", {
    result <- suppressWarnings(suppressMessages(permdisp(
        data=permdisp_state_data(),
        vars=c("sp1", "sp2", "sp3"),
        factor="group",
        permN=19,
        seed=123
    )))
    table <- result$anova$asDF
    residual <- which(table$source %in% c("Residual", "Residuals"))

    expect_length(residual, 1L)
    expect_true(is.na(table$f[[residual]]))
    expect_true(is.na(table$p[[residual]]))
    row_key <- result$anova$rowKeys[[residual]]
    expect_identical(result$anova$getCell(rowKey=row_key, col="f")$value, "")
    expect_identical(result$anova$getCell(rowKey=row_key, col="p")$value, "")
})


test_that("PERMDISP distance summary agrees with vegan distances", {
    data <- permdisp_state_data()
    result <- suppressWarnings(suppressMessages(permdisp(
        data=data,
        vars=c("sp1", "sp2", "sp3"),
        factor="group",
        permN=19,
        seed=123
    )))
    fit <- suppressWarnings(vegan::betadisper(
        vegan::vegdist(data[c("sp1", "sp2", "sp3")], method="bray"),
        data$group,
        type="median"))
    expected <- split(fit$distances, data$group, drop=TRUE)
    actual <- result$distances$asDF

    expect_identical(actual$group, names(expected))
    expect_equal(actual$n, unname(vapply(expected, length, integer(1))))
    expect_equal(actual$distance, unname(vapply(expected, mean, numeric(1))))
    expect_equal(actual$median, unname(vapply(expected, stats::median, numeric(1))))
    expect_equal(actual$q1, unname(vapply(expected, function(x) {
        stats::quantile(x, .25, names=FALSE)
    }, numeric(1))))
    expect_equal(actual$q3, unname(vapply(expected, function(x) {
        stats::quantile(x, .75, names=FALSE)
    }, numeric(1))))
    expect_equal(actual$sd, unname(vapply(expected, stats::sd, numeric(1))))
    expect_equal(actual$min, unname(vapply(expected, min, numeric(1))))
    expect_equal(actual$max, unname(vapply(expected, max, numeric(1))))
})



test_that("PERMDISP plot schemas and generated contracts stay synchronized", {
    root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
    if (!file.exists(file.path(root, "jamovi", "permdisp.a.yaml")))
        skip("development source files are not present in the package-check tree")
    for (name in c("permdisp.a.yaml", "permdisp.u.yaml", "permdisp.r.yaml")) {
        expect_identical(
            readLines(file.path(root, "jamovi", name), warn=FALSE),
            readLines(miso_fixture_path("jamovi", name), warn=FALSE))
    }
    expect_identical(
        readLines(file.path(root, "jamovi", "js", "permdisp.js"), warn=FALSE),
        readLines(miso_fixture_path("jamovi", "js", "permdisp.js"), warn=FALSE))
    generated <- paste(
        readLines(file.path(root, "R", "permdisp.h.R"), warn=FALSE),
        collapse="\n")
    expect_match(generated, "showDistancePlot")
    expect_match(generated, "showOrdinationPlot")
    expect_match(generated, "plotDescription")
    expect_match(generated, "ordinationScores")
})

test_that("distance diagnostic preserves fitted identity and all-value summaries", {
    fit <- permdisp_test_fit()
    diagnostic <- .preparePermdispDistanceDiagnostic(
        fit, centre="median", maxRaw=4L)

    expect_identical(as.numeric(diagnostic$all$distance), as.numeric(fit$distances))
    expect_identical(as.character(diagnostic$all$group), as.character(fit$group))
    expect_identical(diagnostic$groups, levels(fit$group))
    expect_identical(diagnostic$displayed, 4L)
    expect_identical(diagnostic$total, length(fit$distances))
    expected <- split(fit$distances, fit$group, drop=TRUE)
    expect_identical(diagnostic$summaries$group, names(expected))
    expect_identical(
        diagnostic$summaries$n,
        unname(vapply(expected, length, integer(1))))
    expect_true(all(diagnostic$summaries$centre == "Median"))
    expect_equal(
        diagnostic$summaries$median,
        unname(vapply(expected, stats::median, numeric(1))))
    expect_equal(
        diagnostic$summaries$q1,
        unname(vapply(expected, function(x) {
            stats::quantile(x, .25, names=FALSE)
        }, numeric(1))))
    expect_equal(
        diagnostic$summaries$q3,
        unname(vapply(expected, function(x) {
            stats::quantile(x, .75, names=FALSE)
        }, numeric(1))))
})

test_that("distance marks are deterministic capped and RNG-safe", {
    fit <- list(
        distances=seq_len(900L) / 100,
        group=factor(rep(c("A", "B", "C"), each=300L)))
    set.seed(8243)
    before <- .Random.seed
    first <- .preparePermdispDistanceDiagnostic(
        fit, centre="centroid", maxRaw=600L)
    after <- .Random.seed
    second <- .preparePermdispDistanceDiagnostic(
        fit, centre="centroid", maxRaw=600L)

    expect_identical(after, before)
    expect_identical(first$raw, second$raw)
    expect_identical(first$displayed, 600L)
    expect_identical(first$total, 900L)
    expect_identical(first$summaries$n, rep(300L, 3L))
    expect_true(all(first$summaries$centre == "Centroid"))
    expect_match(
        .misoPlotDisclosure(first$displayed, first$total, "site distances"),
        "300.*omitted")
})

test_that("distance plot uses shared ggplot styling and collision-safe n labels", {
    groups <- c(
        "A very long treatment label sharing prefix alpha",
        "A very long treatment label sharing prefix beta")
    fit <- list(
        distances=c(.1, .2, .3, .4),
        group=factor(rep(groups, each=2L), levels=groups))
    diagnostic <- .preparePermdispDistanceDiagnostic(fit, centre="median")
    plot <- .buildPermdispDistancePlot(diagnostic)
    built <- ggplot2::ggplot_build(plot)
    labels <- built$layout$panel_params[[1L]]$x$get_labels()

    expect_s3_class(plot, "ggplot")
    expect_s3_class(plot$theme, "theme")
    expect_identical(plot$labels$y, "Distance to group centre")
    expect_length(unique(labels), 2L)
    expect_true(all(grepl("n = 2", labels, fixed=TRUE)))
    expect_identical(diagnostic$summaries$group, groups)
    expect_true(any(vapply(plot$layers, function(layer) {
        inherits(layer$geom, "GeomBoxplot")
    }, logical(1))))
    expect_true(any(vapply(plot$layers, function(layer) {
        inherits(layer$geom, "GeomPoint")
    }, logical(1))))
    pointLayer <- plot$layers[[which(vapply(
        plot$layers,
        function(layer) inherits(layer$geom, "GeomPoint"),
        logical(1)))[[1L]]]]
    expect_identical(pointLayer$aes_params$size, 2.1)
    expect_identical(pointLayer$aes_params$alpha, .9)
})

test_that("distance and ordination builders degrade safely beyond 64 groups", {
    groups <- sprintf("Group %02d", seq_len(65L))
    fit <- list(
        distances=seq_along(groups) / 100,
        group=factor(groups, levels=groups))
    diagnostic <- .preparePermdispDistanceDiagnostic(fit, centre="median")
    distancePlot <- .buildPermdispDistancePlot(diagnostic)

    centres <- data.frame(
        point=paste("Centre:", groups), pointType="Group centre",
        group=groups, axis1=seq_along(groups), axis2=seq_along(groups) / 2)
    sites <- data.frame(
        point=paste("Row", seq_along(groups)), pointType="Site",
        group=groups, axis1=seq_along(groups) + .1,
        axis2=seq_along(groups) / 2 + .1,
        centre1=centres$axis1, centre2=centres$axis2)
    ordination <- list(
        available=TRUE, tableAvailable=TRUE,
        sites=sites, centres=centres, plotCentres=centres,
        eligibleSites=sites, raw=sites,
        groups=groups, axisNames=c("PCoA1", "PCoA2"),
        displayed=65L, plotEligible=65L, unplottable=0L, total=65L)
    ordinationPlot <- .buildPermdispOrdinationPlot(ordination)

    expect_s3_class(distancePlot, "ggplot")
    expect_s3_class(ordinationPlot, "ggplot")
    for (plot in list(distancePlot, ordinationPlot)) {
        expect_identical(plot$theme$plot.background$fill, "transparent")
        expect_identical(plot$theme$panel.background$fill, "transparent")
    }
    expect_silent(ggplot2::ggplot_build(distancePlot))
    expect_silent(ggplot2::ggplot_build(ordinationPlot))
})

test_that("ordination uses exact fitted site and centre coordinates", {
    fit <- permdisp_test_fit()
    expected <- vegan::scores(
        fit, display=c("sites", "centroids"), choices=1:2)
    ordination <- .preparePermdispOrdination(
        fit, rowIndex=seq(11L, 19L))

    expect_true(ordination$available)
    expect_identical(ordination$axisNames, colnames(expected$sites))
    expect_identical(ordination$sites$point, paste("Row", 11:19))
    expect_identical(ordination$sites$group, as.character(fit$group))
    expect_identical(ordination$sites$plotKey, as.character(fit$group))
    expect_equal(
        unname(as.matrix(ordination$sites[c("axis1", "axis2")])),
        unname(expected$sites), tolerance=0)
    expect_identical(ordination$centres$group, rownames(expected$centroids))
    expect_identical(ordination$centres$plotKey, rownames(expected$centroids))
    expect_equal(
        unname(as.matrix(ordination$centres[c("axis1", "axis2")])),
        unname(expected$centroids), tolerance=0)
    centreRows <- match(ordination$sites$group, ordination$centres$group)
    expect_equal(
        ordination$sites$centre1,
        ordination$centres$axis1[centreRows], tolerance=0)
    expect_equal(
        ordination$sites$centre2,
        ordination$centres$axis2[centreRows], tolerance=0)
})

test_that("ordination plot has equal axes segments and no ellipse layer", {
    ordination <- .preparePermdispOrdination(permdisp_test_fit())
    plot <- .buildPermdispOrdinationPlot(ordination)
    geomClasses <- vapply(plot$layers, function(layer) {
        class(layer$geom)[[1L]]
    }, character(1))

    expect_s3_class(plot, "ggplot")
    expect_identical(plot$coordinates$ratio, 1)
    expect_true("GeomSegment" %in% geomClasses)
    expect_false(any(grepl("Ellipse", geomClasses, ignore.case=TRUE)))
    expect_identical(plot$labels$x, ordination$axisNames[[1L]])
    expect_identical(plot$labels$y, ordination$axisNames[[2L]])

    pointLayers <- Filter(function(layer) {
        inherits(layer$geom, "GeomPoint")
    }, plot$layers)
    centreLayer <- pointLayers[[which(vapply(pointLayers, function(layer) {
        identical(layer$aes_params$shape, 23L)
    }, logical(1)))]]
    siteLayer <- pointLayers[[which(vapply(pointLayers, function(layer) {
        is.null(layer$aes_params$shape)
    }, logical(1)))]]
    expect_false(centreLayer$show.legend)
    expect_false(identical(siteLayer$show.legend, FALSE))
})

test_that("one-axis geometry keeps accessible coordinates without plotting", {
    fit <- permdisp_test_fit()
    fit$vectors <- fit$vectors[, 1L, drop=FALSE]
    fit$centroids <- fit$centroids[, 1L, drop=FALSE]
    fit$eig <- fit$eig[[1L]]
    names(fit$eig) <- "PCoA1"
    ordination <- .preparePermdispOrdination(fit)

    expect_false(ordination$available)
    expect_true(ordination$tableAvailable)
    expect_match(ordination$reason, "fewer than two usable axes")
    expect_identical(ordination$sites$point, paste("Row", seq_len(9L)))
    expect_equal(nrow(ordination$sites), 9L)
    expect_true(all(is.na(ordination$sites$axis2)))
    expect_null(.buildPermdispOrdinationPlot(ordination))

    analysis <- permdisp_populate_test_ordination(ordination)
    expect_false(analysis$results$ordinationPlot$visible)
    expect_true(analysis$results$ordinationDescription$visible)
    expect_true(analysis$results$ordinationScores$visible)
    expect_equal(
        sum(analysis$results$ordinationScores$asDF$pointType == "Site"),
        9L)
})

test_that("non-finite fitted sites remain in coordinates and are disclosed", {
    fit <- permdisp_test_fit()
    fit$vectors[1L, 1L] <- NaN
    ordination <- .preparePermdispOrdination(
        fit, rowIndex=seq(11L, 19L), maxRaw=3L)

    expect_true(ordination$available)
    expect_true(ordination$tableAvailable)
    expect_equal(ordination$total, 9L)
    expect_equal(ordination$plotEligible, 8L)
    expect_equal(ordination$unplottable, 1L)
    expect_equal(ordination$displayed, 3L)
    expect_identical(ordination$sites$point, paste("Row", 11:19))
    expect_identical(ordination$sites$group, as.character(fit$group))
    expect_equal(nrow(ordination$eligibleSites), 8L)
    expect_true(all(is.finite(ordination$raw$axis1)))
    expect_true(all(is.finite(ordination$raw$axis2)))
    expect_true(all(is.finite(ordination$raw$centre1)))
    expect_true(all(is.finite(ordination$raw$centre2)))

    analysis <- permdisp_populate_test_ordination(ordination)
    scores <- analysis$results$ordinationScores$asDF
    siteScores <- scores[scores$pointType == "Site", , drop=FALSE]
    description <- as.character(
        analysis$results$ordinationDescription$asString())
    description <- gsub("\\s+", " ", description)
    expect_equal(nrow(siteScores), 9L)
    expect_identical(siteScores$point, paste("Row", 11:19))
    expect_identical(siteScores$group, as.character(fit$group))
    expect_true(is.na(siteScores$axis1[[1L]]))
    expect_false(any(is.nan(siteScores$axis1)))
    expect_false(grepl("NaN", paste(as.matrix(scores), collapse=" "), fixed=TRUE))
    expect_match(
        description,
        "1 of 9 fitted sites could not be plotted",
        fixed=TRUE)
})

test_that("all non-finite fitted sites fail gracefully without losing rows", {
    fit <- permdisp_test_fit()
    fit$vectors[, 1:2] <- NaN
    ordination <- .preparePermdispOrdination(fit)

    expect_false(ordination$available)
    expect_true(ordination$tableAvailable)
    expect_equal(ordination$total, 9L)
    expect_equal(ordination$plotEligible, 0L)
    expect_equal(ordination$unplottable, 9L)
    expect_equal(nrow(ordination$sites), 9L)
    expect_equal(nrow(ordination$raw), 0L)
    expect_match(ordination$reason, "No fitted sites had finite coordinates")
    expect_null(.buildPermdispOrdinationPlot(ordination))

    analysis <- permdisp_populate_test_ordination(ordination)
    expect_false(analysis$results$ordinationPlot$visible)
    expect_true(analysis$results$ordinationScores$visible)
    expect_equal(
        sum(analysis$results$ordinationScores$asDF$pointType == "Site"),
        9L)
    expect_match(
        as.character(analysis$results$ordinationDescription$asString()),
        "9 of 9 fitted sites could not be plotted",
        fixed=TRUE)
})

test_that("positive and negative fitted axes are preserved exactly", {
    fit <- permdisp_test_fit()
    positive <- which(fit$eig > 0)[[1L]]
    negative <- which(fit$eig < 0)[[1L]]
    choices <- c(positive, negative)
    expectedSites <- fit$vectors[, choices, drop=FALSE]
    expectedCentres <- fit$centroids[, choices, drop=FALSE]
    fit$vectors <- expectedSites
    fit$centroids <- expectedCentres
    fit$eig <- fit$eig[choices]

    ordination <- .preparePermdispOrdination(fit)

    expect_true(ordination$available)
    expect_identical(ordination$axisNames, colnames(expectedSites))
    expect_equal(
        unname(as.matrix(ordination$sites[c("axis1", "axis2")])),
        unname(expectedSites), tolerance=0)
    expect_equal(
        unname(as.matrix(ordination$centres[c("axis1", "axis2")])),
        unname(expectedCentres), tolerance=0)
})

test_that("plot toggles preserve every numerical PERMDISP result", {
    arguments <- list(
        data=permdisp_state_data(),
        vars=c("sp1", "sp2", "sp3"),
        factor="group",
        dispPairwise=TRUE,
        permN=19,
        seed=123)
    default <- suppressWarnings(suppressMessages(do.call(permdisp, arguments)))
    hidden <- suppressWarnings(suppressMessages(do.call(
        permdisp,
        c(arguments, list(
            showDistancePlot=FALSE, showOrdinationPlot=FALSE)))))
    ordination <- suppressWarnings(suppressMessages(do.call(
        permdisp,
        c(arguments, list(
            showDistancePlot=TRUE, showOrdinationPlot=TRUE)))))

    for (name in c("anova", "pairwise", "distances")) {
        expect_equal(hidden[[name]]$asDF, default[[name]]$asDF, tolerance=0)
        expect_equal(ordination[[name]]$asDF, default[[name]]$asDF, tolerance=0)
    }
    expect_false(hidden$plot$visible)
    expect_false(hidden$plotDescription$visible)
    expect_false(hidden$ordinationPlot$visible)
    expect_false(hidden$ordinationDescription$visible)
    expect_false(hidden$ordinationScores$visible)
    expect_true(ordination$plot$visible)
    expect_false(ordination$plotDescription$visible)
    expect_true(ordination$ordinationPlot$visible)
    expect_false(ordination$ordinationDescription$visible)
    expect_true(ordination$ordinationScores$visible)
    expect_identical(ordination$plotDescription$content, "")
    expect_identical(ordination$ordinationDescription$content, "")
})

test_that("plot transitions clear stale descriptions and coordinates", {
    options <- permdispOptions$new(
        vars=c("sp1", "sp2", "sp3"),
        factor="group",
        showOrdinationPlot=TRUE,
        permN=19,
        seed=123)
    analysis <- permdispClass$new(
        options=options, data=permdisp_state_data())
    suppressWarnings(suppressMessages(analysis$run()))
    expect_true(analysis$results$ordinationPlot$visible)
    expect_gt(nrow(analysis$results$ordinationScores$asDF), 0L)

    distanceOption <- options$option("showDistancePlot")
    ordinationOption <- options$option("showOrdinationPlot")
    distanceOption$.__enclos_env__$private$.value <- FALSE
    ordinationOption$.__enclos_env__$private$.value <- FALSE
    suppressWarnings(suppressMessages(analysis$run()))
    expect_false(analysis$results$plot$visible)
    expect_false(analysis$results$plotDescription$visible)
    expect_false(analysis$results$ordinationPlot$visible)
    expect_false(analysis$results$ordinationDescription$visible)
    expect_false(analysis$results$ordinationScores$visible)
    expect_false(grepl(
        "Dispersion Test table",
        as.character(analysis$results$plotDescription$asString())))
    expect_false(grepl(
        "same fitted",
        as.character(analysis$results$ordinationDescription$asString())))
    expect_equal(nrow(analysis$results$ordinationScores$asDF), 0L)
})

test_that("PERMDISP ggplots render to bounded PNG files", {
    distance <- .buildPermdispDistancePlot(
        .preparePermdispDistanceDiagnostic(
            permdisp_test_fit(), centre="median"))
    ordination <- .buildPermdispOrdinationPlot(
        .preparePermdispOrdination(permdisp_test_fit()))
    distanceFile <- tempfile(fileext=".png")
    ordinationFile <- tempfile(fileext=".png")
    on.exit(unlink(c(distanceFile, ordinationFile)), add=TRUE)

    grDevices::png(distanceFile, width=600, height=460)
    print(distance)
    grDevices::dev.off()
    grDevices::png(ordinationFile, width=600, height=500)
    print(ordination)
    grDevices::dev.off()

    expect_gt(file.info(distanceFile)$size, 1000)
    expect_gt(file.info(ordinationFile)$size, 1000)
})

test_that("distance plot renders from serialized Image state alone", {
    options <- permdispOptions$new(
        vars=c("sp1", "sp2", "sp3"), factor="group",
        showDistancePlot=TRUE)
    analysis <- permdispClass$new(options=options, data=permdisp_state_data())
    suppressWarnings(suppressMessages(analysis$run()))
    image <- analysis$results$plot
    state <- image$state
    expect_false(is.null(state))
    expect_s3_class(state$all, "data.frame")

    restored <- unserialize(serialize(state, NULL))
    analysis$.__enclos_env__$private$.state$distanceDiagnostic <- NULL
    image$setState(restored)

    file <- tempfile(fileext=".png")
    on.exit({
        if (grDevices::dev.cur() > 1L)
            grDevices::dev.off()
        unlink(file)
    }, add=TRUE)
    grDevices::png(file, width=600, height=460)
    analysis$.__enclos_env__$private$.plotDistances(image)
    grDevices::dev.off()
    expect_gt(file.info(file)$size, 1000)
})

test_that("ordination plot renders from serialized Image state alone", {
    options <- permdispOptions$new(
        vars=c("sp1", "sp2", "sp3"), factor="group",
        showOrdinationPlot=TRUE)
    analysis <- permdispClass$new(options=options, data=permdisp_state_data())
    suppressWarnings(suppressMessages(analysis$run()))
    image <- analysis$results$ordinationPlot
    state <- image$state
    expect_false(is.null(state))
    expect_true(state$available)

    restored <- unserialize(serialize(state, NULL))
    analysis$.__enclos_env__$private$.state$ordination <- NULL
    image$setState(restored)

    file <- tempfile(fileext=".png")
    on.exit({
        if (grDevices::dev.cur() > 1L)
            grDevices::dev.off()
        unlink(file)
    }, add=TRUE)
    grDevices::png(file, width=600, height=500)
    analysis$.__enclos_env__$private$.plotOrdination(image)
    grDevices::dev.off()
    expect_gt(file.info(file)$size, 1000)
})


test_that("structural sample inputs rebuild coordinate rows while term rows update in place", {
    data <- permdisp_state_data()
    data$sp4 <- c(NA_real_, 3, 1, 9, 7, 8, 2, 5, 4)
    options <- permdispOptions$new(
        vars=c("sp1", "sp2", "sp3", "sp4"),
        factor="group",
        showOrdinationPlot=TRUE,
        permN=19,
        seed=123)
    analysis <- permdispClass$new(options=options, data=data)
    suppressWarnings(suppressMessages(analysis$run()))
    distanceKeys <- analysis$results$distances$rowKeys
    anovaKeys <- analysis$results$anova$rowKeys
    scoreKeys <- analysis$results$ordinationScores$rowKeys
    scoreRowsBefore <- nrow(analysis$results$ordinationScores$asDF)

    varsOption <- options$option("vars")
    varsOption$.__enclos_env__$private$.value <- c("sp1", "sp2", "sp3")
    suppressWarnings(suppressMessages(analysis$run()))
    expect_false(identical(analysis$results$ordinationScores$rowKeys, scoreKeys))
    expect_gt(
        nrow(analysis$results$ordinationScores$asDF), scoreRowsBefore)
    # Group-level tables keep their row structure and refresh values only.
    expect_identical(analysis$results$distances$rowKeys, distanceKeys)
    expect_identical(analysis$results$anova$rowKeys, anovaKeys)
    expect_setequal(
        analysis$results$distances$asDF$group,
        c("A", "B", "C"))
})

test_that("PERMDISP keeps method settings in surviving table notes", {
    result <- suppressWarnings(suppressMessages(permdisp(
        data=permdisp_state_data(), vars=c("sp1", "sp2", "sp3"),
        factor="group", permN=19, seed=123)))
    expect_true(result$anova$visible)
    expect_match(miso_table_note(result$anova, "structuralCells"), "Transformation|Dissimilarity|Permutation")
})

test_that("PERMDISP copied test notes carry effective distance fitting and permutation settings", {
    result <- suppressWarnings(suppressMessages(permdisp(
        data=permdisp_state_data(),
        vars=c("sp1", "sp2", "sp3"),
        factor="group",
        permN=19,
        seed=123)))
    note <- miso_table_note(result$anova, "structuralCells")

    # The published note reports essential methods, without disabled switches.
    expect_match(note, "Bray-Curtis", fixed=TRUE)
    expect_match(note, "Centre: Median", fixed=TRUE)
    expect_match(note, "Bias adjustment: Not applied", fixed=TRUE)
    expect_match(note, "Effective restriction: Free", fixed=TRUE)
    expect_match(note, "Permutations: 19", fixed=TRUE)
    expect_false(grepl("Random seed|Disabled|Residual row|Requested permutation", note))
})

test_that("PERMDISP notes report non-default and legacy-restriction settings", {
    series <- suppressWarnings(suppressMessages(permdisp(
        data=permdisp_state_data(),
        vars=c("sp1", "sp2", "sp3"),
        factor="group",
        distSqrt=TRUE,
        distAdd="cailliez",
        dispType="centroid",
        dispBias=TRUE,
        permRestriction="series",
        permN=19,
        seed=123)))
    note <- miso_table_note(series$anova, "structuralCells")
    expect_match(note, "Square-root distances", fixed=TRUE)
    expect_match(note, "Cailliez correction", fixed=TRUE)
    expect_match(note, "Centre: Centroid", fixed=TRUE)
    expect_match(note, "Bias adjustment: Applied", fixed=TRUE)
    expect_match(note, "Effective restriction: Series (rows in order)",
        fixed=TRUE)
    # Nine rows permit eight non-identity cyclic shifts, not 19 requested.
    expect_match(note, "Permutations: 8.", fixed=TRUE)

    # Saved analyses carrying a legacy scheme normalize to the effective
    # restriction instead of reporting the raw default.
    legacy <- suppressWarnings(suppressMessages(permdisp(
        data=permdisp_state_data(),
        vars=c("sp1", "sp2", "sp3"),
        factor="group",
        permScheme="stratified",
        permN=19,
        seed=123)))
    legacyNote <- miso_table_note(legacy$anova, "structuralCells")
    expect_false(grepl("Requested permutation restriction", legacyNote))
    expect_match(legacyNote, "Effective restriction: Free", fixed=TRUE)
    expect_match(miso_squish_result(legacy$warnings),
        "are implemented as Free permutations", fixed=TRUE)
})

test_that("PERMDISP copied pairwise notes state the adjustment and contrast family", {
    result <- suppressWarnings(suppressMessages(permdisp(
        data=permdisp_state_data(),
        vars=c("sp1", "sp2", "sp3"),
        factor="group",
        dispPairwise=TRUE,
        dispAdjust="BH",
        permN=19,
        seed=123)))
    note <- miso_table_note(result$pairwise, "scope")

    expect_match(note, "Dispersion comparisons for group", fixed=TRUE)
    expect_match(note, "Effective restriction: Free", fixed=TRUE)
    expect_match(note, "Permutations: 19", fixed=TRUE)
    expect_match(note, "P-value adjustment: Benjamini-Hochberg", fixed=TRUE)
    expect_match(note, "across all 3 pairwise contrasts", fixed=TRUE)
})

miso_permdisp_cells <- function(table) {
    table$columns[[1L]]$.__enclos_env__$private$.cells
}

test_that("enabling dispersion diagnostics restores descriptions without changing inference", {
    data <- data.frame(sp1=1:9, sp2=(1:9)*2,
        group=factor(rep(c("A", "B", "C"), each=3)))
    options <- permdispOptions$new(vars=c("sp1", "sp2"), factor="group",
        distance="euclidean", showDistancePlot=FALSE,
        showOrdinationPlot=FALSE, permN=19, seed=123)
    analysis <- permdispClass$new(options=options, data=data)
    suppressWarnings(suppressMessages(analysis$run()))
    before <- analysis$results$anova$asDF
    cells <- miso_permdisp_cells(analysis$results$distances)
    for (name in c("showDistancePlot", "showOrdinationPlot")) {
        option <- options$option(name)
        option$.__enclos_env__$private$.value <- TRUE
    }
    suppressWarnings(suppressMessages(analysis$run()))
    fresh <- suppressWarnings(suppressMessages(permdisp(
        data=data, vars=c("sp1", "sp2"), factor="group",
        distance="euclidean", showDistancePlot=TRUE,
        showOrdinationPlot=TRUE, permN=19, seed=123)))
    for (name in c("plotDescription", "ordinationDescription"))
        expect_identical(analysis$results[[name]]$asString(),
            fresh[[name]]$asString())
    expect_match(miso_squish_result(analysis$results$ordinationDescription),
        "fewer than two usable axes")
    expect_false(analysis$results$ordinationPlot$visible)
    expect_equal(analysis$results$anova$asDF, before, tolerance=0)
    expect_identical(miso_permdisp_cells(analysis$results$distances), cells)
    expect_equal(analysis$results$ordinationScores$asDF,
        fresh$ordinationScores$asDF)
})


test_that("value-only data edits refresh dispersion tables and keep their cells", {
    data <- permdisp_state_data()
    options <- permdispOptions$new(
        vars=c("sp1", "sp2", "sp3"), factor="group",
        dispPairwise=TRUE, permN=19, seed=123)
    analysis <- permdispClass$new(options=options, data=data)
    suppressWarnings(suppressMessages(analysis$run()))
    distanceKeys <- analysis$results$distances$rowKeys
    anovaKeys <- analysis$results$anova$rowKeys
    pairwiseKeys <- analysis$results$pairwise$rowKeys
    distanceCells <- miso_permdisp_cells(analysis$results$distances)
    anovaCells <- miso_permdisp_cells(analysis$results$anova)
    before <- analysis$results$distances$asDF

    # Same samples and groups; only feature values change, so the data
    # signature changes but every row key survives the rerun.
    edited <- permdisp_state_data()
    edited$sp1 <- c(2, 3, 2, 6, 9, 6, 4, 3, 2)
    analysis$.__enclos_env__$private$.data$sp1 <- edited$sp1
    suppressWarnings(suppressMessages(analysis$run()))

    expect_identical(analysis$results$distances$rowKeys, distanceKeys)
    expect_identical(analysis$results$anova$rowKeys, anovaKeys)
    expect_identical(analysis$results$pairwise$rowKeys, pairwiseKeys)
    expect_true(identical(
        miso_permdisp_cells(analysis$results$distances), distanceCells))
    expect_true(identical(miso_permdisp_cells(analysis$results$anova), anovaCells))
    expect_false(isTRUE(all.equal(before, analysis$results$distances$asDF)))

    fresh <- permdispClass$new(options=permdispOptions$new(
        vars=c("sp1", "sp2", "sp3"), factor="group",
        dispPairwise=TRUE, permN=19, seed=123), data=edited)
    suppressWarnings(suppressMessages(fresh$run()))
    expect_equal(
        analysis$results$distances$asDF, fresh$results$distances$asDF,
        tolerance=1e-12)
    expect_equal(
        analysis$results$anova$asDF, fresh$results$anova$asDF, tolerance=1e-12)
})

test_that("data edits that remove a group rebuild dispersion rows and stay fresh", {
    data <- permdisp_state_data()
    options <- permdispOptions$new(
        vars=c("sp1", "sp2", "sp3"), factor="group", permN=19, seed=123)
    analysis <- permdispClass$new(options=options, data=data)
    suppressWarnings(suppressMessages(analysis$run()))
    distanceKeys <- analysis$results$distances$rowKeys

    edited <- permdisp_state_data(two_groups=TRUE)
    analysis$.__enclos_env__$private$.data <- edited
    suppressWarnings(suppressMessages(analysis$run()))

    expect_false(identical(analysis$results$distances$rowKeys, distanceKeys))
    expect_identical(analysis$results$distances$asDF$group, c("A", "B"))

    fresh <- permdispClass$new(options=permdispOptions$new(
        vars=c("sp1", "sp2", "sp3"), factor="group", permN=19, seed=123),
        data=edited)
    suppressWarnings(suppressMessages(fresh$run()))
    expect_equal(
        analysis$results$distances$asDF, fresh$results$distances$asDF,
        tolerance=1e-12)
})


test_that("PERMDISP publication output omits routine prose", {
    result <- suppressWarnings(suppressMessages(permdisp(
        data=permdisp_state_data(), vars=c("sp1", "sp2", "sp3"),
        factor="group", seed=123, permN=19, showOrdinationPlot=TRUE)))
    note <- miso_table_note(result$anova, "structuralCells")
    expect_false(grepl("Random seed|Execution:|Disabled|Requested permutation restriction|R compares|not applicable to the Residual", note))
    expect_false(result$plotDescription$visible)
    expect_identical(result$plotDescription$content, "")
    expect_false(result$ordinationDescription$visible)
    expect_identical(result$ordinationDescription$content, "")
})
