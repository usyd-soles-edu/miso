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
    private$.resetResults()
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
    for (name in hidden)
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
    options <- yaml::read_yaml(tofu_fixture_path("jamovi", "permdisp.a.yaml"))$options
    ui <- yaml::read_yaml(tofu_fixture_path("jamovi", "permdisp.u.yaml"))
    by_name <- setNames(options, vapply(options, `[[`, character(1), "name"))

    expect_identical(by_name$vars$title, "Feature variables (required)")
    expect_identical(by_name$factor$title, "Grouping variable (required)")
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

test_that("PERMDISP UI dependencies and progressive disclosure are explicit", {
    source <- paste(
        readLines(tofu_fixture_path("jamovi", "js", "permdisp.js")),
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
        tofu_fixture_path("jamovi", "permdisp.r.yaml"))$items
    by_name <- setNames(results, vapply(results, `[[`, character(1), "name"))

    expect_true(all(vapply(results, function(item) identical(item$visible, FALSE), logical(1))))
    expect_identical(by_name$guidance$type, "Html")
    expect_identical(by_name$warnings$type, "Html")
    expect_identical(by_name$note$type, "Html")
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
            "summary", "warnings", "distances", "anova", "pairwise", "plot",
            "plotDescription", "ordinationPlot", "ordinationDescription",
            "ordinationScores", "note", "settings")
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
            "summary", "warnings", "distances", "anova", "pairwise", "plot",
            "plotDescription", "ordinationPlot", "ordinationDescription",
            "ordinationScores", "note", "settings")
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
            "summary", "warnings", "distances", "anova", "pairwise", "plot",
            "plotDescription", "ordinationPlot", "ordinationDescription",
            "ordinationScores", "note", "settings")
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
            "summary", "warnings", "distances", "anova", "pairwise", "plot",
            "plotDescription", "ordinationPlot", "ordinationDescription",
            "ordinationScores", "note", "settings")
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
            "summary", "distances", "anova", "plot", "plotDescription",
            "note", "settings"),
        hidden=c(
            "guidance", "warnings", "pairwise", "ordinationPlot",
            "ordinationDescription", "ordinationScores")
    )
})

test_that("PERMDISP valid invalid valid transitions clear stale results", {
    data <- permdisp_state_data()
    options <- permdispOptions$new(
        vars=c("sp1", "sp2", "sp3"),
        factor="group",
        permN=19,
        seed=123)
    analysis <- permdispClass$new(options=options, data=data)

    suppressWarnings(suppressMessages(analysis$run()))
    expect_true(analysis$results$anova$visible)
    expect_gt(length(analysis$results$anova$rowKeys), 0L)

    factor_option <- options$option("factor")
    factor_option$.__enclos_env__$private$.value <- NULL
    analysis$run()
    expect_true(analysis$results$guidance$visible)
    expect_false(analysis$results$anova$visible)
    expect_false(analysis$results$plot$visible)
    expect_false(analysis$results$plotDescription$visible)
    expect_false(analysis$results$ordinationPlot$visible)
    expect_false(analysis$results$ordinationDescription$visible)
    expect_false(analysis$results$ordinationScores$visible)
    expect_equal(length(analysis$results$summary$rowKeys), 0L)
    expect_equal(length(analysis$results$distances$rowKeys), 0L)
    expect_equal(length(analysis$results$anova$rowKeys), 0L)
    expect_equal(length(analysis$results$pairwise$rowKeys), 0L)

    factor_option$.__enclos_env__$private$.value <- "group"
    suppressWarnings(suppressMessages(analysis$run()))
    expect_false(analysis$results$guidance$visible)
    expect_true(analysis$results$anova$visible)
    expect_gt(length(analysis$results$anova$rowKeys), 0L)
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

test_that("two-group PERMDISP never shows an empty pairwise table", {
    result <- suppressWarnings(suppressMessages(permdisp(
        data=permdisp_state_data(two_groups=TRUE),
        vars=c("sp1", "sp2", "sp3"),
        factor="group",
        dispPairwise=TRUE,
        permN=19,
        seed=123
    )))

    expect_false(result$pairwise$visible)
    expect_equal(nrow(result$pairwise$asDF), 0L)
    expect_match(
        as.character(result$note$asString()),
        paste0(
            "overall[[:space:]|]+dispersion test is[[:space:]|]+",
            "the only group contrast"))
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
            "summary", "warnings", "distances", "anova", "plot",
            "plotDescription", "note", "settings"),
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

test_that("PERMDISP reports requested and effective settings", {
    result <- suppressWarnings(suppressMessages(permdisp(
        data=permdisp_state_data(),
        vars=c("sp1", "sp2", "sp3"),
        factor="group",
        permN=19,
        seed=123
    )))
    settings <- setNames(result$settings$asDF$value, result$settings$asDF$setting)

    expect_identical(settings[["Requested permutation restriction"]], "Free")
    expect_identical(settings[["Effective permutation restriction"]], "Free")
    expect_identical(settings[["Group centre"]], "Median")
    expect_identical(settings[["Random seed"]], "123")
    expect_identical(settings[["Pairwise comparisons"]], "Disabled")
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

test_that("PERMDISP reports advanced and current Series settings", {
    result <- suppressWarnings(suppressMessages(permdisp(
        data=permdisp_state_data(),
        vars=c("sp1", "sp2", "sp3"),
        factor="group",
        transform="sqrt",
        distSqrt=TRUE,
        distAdd="cailliez",
        dispType="centroid",
        dispBias=TRUE,
        permRestriction="series",
        permN=19,
        seed=123
    )))
    settings <- setNames(result$settings$asDF$value, result$settings$asDF$setting)

    expect_identical(settings[["Transformation"]], "Square root")
    expect_identical(settings[["Group centre"]], "Centroid")
    expect_identical(settings[["Bias adjustment"]], "Enabled")
    expect_identical(settings[["Binary dissimilarity"]], "Disabled")
    expect_identical(settings[["Square-root distances"]], "Enabled")
    expect_identical(settings[["Additive constant"]], "Cailliez")
    expect_identical(settings[["Requested permutation restriction"]], "Series (rows in order)")
    expect_identical(settings[["Effective permutation restriction"]], "Series (rows in order)")
    expect_identical(settings[["Sequence order"]], "Current data-row order")
    expect_false(result$warnings$visible)

    binary <- suppressWarnings(suppressMessages(permdisp(
        data=permdisp_state_data(),
        vars=c("sp1", "sp2", "sp3"),
        factor="group",
        distBinary=TRUE,
        permN=19,
        seed=123
    )))
    binary_settings <- setNames(
        binary$settings$asDF$value,
        binary$settings$asDF$setting)
    expect_identical(binary_settings[["Binary dissimilarity"]], "Enabled")
})

test_that("legacy no-block Stratified PERMDISP is equivalent to Free", {
    free <- suppressWarnings(suppressMessages(permdisp(
        data=permdisp_state_data(),
        vars=c("sp1", "sp2", "sp3"),
        factor="group",
        permScheme="free",
        permN=19,
        seed=123
    )))
    legacy <- suppressWarnings(suppressMessages(permdisp(
        data=permdisp_state_data(),
        vars=c("sp1", "sp2", "sp3"),
        factor="group",
        permScheme="stratified",
        permN=19,
        seed=123
    )))

    expect_equal(legacy$anova$asDF, free$anova$asDF, tolerance=0)
    settings <- setNames(legacy$settings$asDF$value, legacy$settings$asDF$setting)
    expect_identical(settings[["Requested permutation restriction"]], "Stratified (legacy)")
    expect_identical(settings[["Effective permutation restriction"]], "Free")
    expect_true(legacy$warnings$visible)
    expect_match(
        as.character(legacy$warnings$asString()),
        "equivalent for that design")
})

test_that("PERMDISP plot schemas and generated contracts stay synchronized", {
    root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
    if (!file.exists(file.path(root, "jamovi", "permdisp.a.yaml")))
        skip("development source files are not present in the package-check tree")
    for (name in c("permdisp.a.yaml", "permdisp.u.yaml", "permdisp.r.yaml")) {
        expect_identical(
            readLines(file.path(root, "jamovi", name), warn=FALSE),
            readLines(tofu_fixture_path("jamovi", name), warn=FALSE))
    }
    expect_identical(
        readLines(file.path(root, "jamovi", "js", "permdisp.js"), warn=FALSE),
        readLines(tofu_fixture_path("jamovi", "js", "permdisp.js"), warn=FALSE))
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
        .tofuPlotDisclosure(first$displayed, first$total, "site distances"),
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
    expect_match(description, "1 of 9 fitted sites could not be plotted", fixed=TRUE)
    expect_match(description, "3 of 8 plot-eligible sites", fixed=TRUE)
    expect_match(
        description,
        "Small colour-and-shape points are sites, open diamonds are fitted group centres",
        fixed=TRUE)
    expect_match(
        description,
        "segments connect each displayed site to its own centre",
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
    expect_true(ordination$plotDescription$visible)
    expect_true(ordination$ordinationPlot$visible)
    expect_true(ordination$ordinationDescription$visible)
    expect_true(ordination$ordinationScores$visible)
    expect_match(
        as.character(ordination$plotDescription$asString()),
        "Dispersion Test table")
    expect_match(
        as.character(ordination$ordinationDescription$asString()),
        "same fitted")
    expect_match(
        gsub(
            "\\s+", " ",
            as.character(ordination$ordinationDescription$asString())),
        "open diamonds are fitted group centres",
        fixed=TRUE)
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
