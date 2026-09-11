pcoa_small_data <- function() {
    path <- testthat::test_path("..", "manual", "miso-small.csv")
    utils::read.csv(path, check.names=FALSE)
}

pcoa_feature_names <- function(data) {
    grep("^feature_[0-9]+$", names(data), value=TRUE)
}

align_pcoa_columns <- function(actual, expected) {
    actual <- as.matrix(actual)
    expected <- as.matrix(expected)
    for (i in seq_len(min(ncol(actual), ncol(expected)))) {
        direction <- sum(actual[, i] * expected[, i])
        if (is.finite(direction) && direction < 0)
            actual[, i] <- -actual[, i]
    }
    actual
}

run_pcoa_private <- function(data, vars, ...) {
    options <- pcoaOptions$new(vars=vars, ...)
    analysis <- pcoaClass$new(options=options, data=data)
    suppressWarnings(suppressMessages(
        analysis$.__enclos_env__$private$.run()))
    analysis
}

pcoa_plot_stub <- function(groups) {
    groups <- as.character(groups)
    siteNames <- sprintf("site_%04d", seq_along(groups))
    points <- cbind(
        PCoA1=seq_along(groups) / max(1L, length(groups)),
        PCoA2=sin(seq_along(groups)))
    rownames(points) <- siteNames
    groupFactor <- factor(groups, levels=sort(unique(groups)))
    names(groupFactor) <- siteNames
    centroids <- t(vapply(levels(groupFactor), function(group) {
        colMeans(points[groupFactor == group, , drop=FALSE])
    }, numeric(2L)))
    rownames(centroids) <- levels(groupFactor)
    list(
        error=FALSE,
        points=points,
        groups=groupFactor,
        siteNames=siteNames,
        centroids=centroids,
        axisLabels=c("PCoA1 (60.0%)", "PCoA2 (25.0%)"))
}

test_that("PCoA core matches direct wcmdscale for every correction path", {
    data <- pcoa_small_data()
    vars <- pcoa_feature_names(data)
    prepared <- miso_prepare_resemblance(
        data=data, vars=vars, factor=NULL, transform="none",
        distance="bray", seed=0, requireFactor=FALSE,
        distBinary=FALSE)

    cases <- list(
        list(sqrt=FALSE, correction="none", add=FALSE),
        list(sqrt=TRUE, correction="none", add=FALSE),
        list(sqrt=FALSE, correction="lingoes", add="lingoes"),
        list(sqrt=FALSE, correction="cailliez", add="cailliez"))

    for (case in cases) {
        expectedDistance <- prepared$dist
        if (case$sqrt)
            expectedDistance[] <- sqrt(expectedDistance[])
        expected <- vegan::wcmdscale(
            expectedDistance,
            k=attr(expectedDistance, "Size") - 1L,
            eig=TRUE, add=case$add, x.ret=TRUE)
        actual <- .misoPcoa(
            prepared$dist,
            correction=case$correction,
            sqrtDist=case$sqrt)

        expect_false(actual$error)
        expect_equal(
            as.vector(actual$transformedDistance),
            as.vector(expectedDistance), tolerance=0)
        expect_equal(
            unname(align_pcoa_columns(actual$points, expected$points)),
            unname(expected$points), tolerance=1e-12)
        expect_equal(actual$eigenvalues, unname(expected$eig),
            tolerance=0)
        expect_identical(colnames(actual$points),
            paste0("PCoA", seq_len(ncol(expected$points))))
        expectedLabels <- attr(prepared$dist, "Labels")
        if (is.null(expectedLabels))
            expectedLabels <- as.character(seq_len(attr(prepared$dist, "Size")))
        expect_identical(rownames(actual$points), expectedLabels)
        if (identical(case$correction, "none")) {
            expect_true(is.na(actual$correctionConstant))
            expect_equal(as.vector(actual$correctedDistance),
                as.vector(expectedDistance), tolerance=0)
        } else {
            expect_equal(actual$correctionConstant, expected$ac,
                tolerance=0)
            expectedCorrected <- expectedDistance
            expectedCorrected[] <- if (
                    identical(case$correction, "lingoes")) {
                sqrt(expectedDistance[]^2 + 2 * expected$ac)
            } else {
                expectedDistance[] + expected$ac
            }
            expect_equal(as.vector(actual$correctedDistance),
                as.vector(expectedCorrected), tolerance=0)
        }

        positive <- expected$eig > 0
        expectedExplained <- rep(NA_real_, length(expected$eig))
        expectedExplained[positive] <-
            100 * expected$eig[positive] / sum(expected$eig[positive])
        expect_equal(actual$explained, expectedExplained, tolerance=0)
        expect_equal(sum(actual$explained[positive]), 100,
            tolerance=1e-12)
        expect_true(all(is.na(actual$explained[expected$eig <= 0])))
    }
})

test_that("PCoA core preserves identities and computes exact group centres", {
    data <- pcoa_small_data()[1:12, ]
    vars <- pcoa_feature_names(data)
    rownames(data) <- paste0("site_", seq_len(nrow(data)))
    prepared <- miso_prepare_resemblance(
        data=data, vars=vars, factor="group", transform="none",
        distance="bray", seed=0, requireFactor=FALSE,
        distBinary=FALSE)
    actual <- .misoPcoa(prepared$dist, groups=prepared$group)

    expect_identical(actual$siteNames, as.character(prepared$rowIndex))
    expect_identical(names(actual$groups), actual$siteNames)
    for (group in levels(actual$groups)) {
        expect_equal(
            actual$centroids[group, ],
            colMeans(actual$points[actual$groups == group, , drop=FALSE]),
            tolerance=0)
        expect_identical(actual$groupSizes[[group]],
            sum(actual$groups == group))
    }

    expect_true(.misoPcoa(prepared$dist,
        groups=prepared$group[-1L])$error)
    badGroups <- as.character(prepared$group)
    badGroups[[1L]] <- NA_character_
    expect_true(.misoPcoa(prepared$dist, groups=badGroups)$error)
})

test_that("PCoA preserves long, colliding, and duplicate site identities", {
    matrix <- cbind(seq_len(6), c(2, 4, 1, 6, 3, 5))
    labels <- c(
        "a very long site identity that shares its beginning 001",
        "a very long site identity that shares its beginning 002",
        "duplicate site", "duplicate site", "short", "another")
    distance <- stats::dist(matrix)
    attr(distance, "Labels") <- labels
    result <- .misoPcoa(distance)
    expect_false(result$error)
    expect_identical(result$siteNames, labels)
    expect_identical(rownames(result$points), labels)
})

test_that("PCoA analysis retains source rows after filtering", {
    data <- pcoa_small_data()[1:10, ]
    vars <- pcoa_feature_names(data)
    data[[vars[[1L]]]][[4L]] <- NA_real_
    result <- do.call(pcoa, list(data=data, vars=vars))
    expect_identical(result$sites$asDF$sourceRow,
        as.integer(setdiff(seq_len(nrow(data)), 4L)))
    expect_identical(result$sites$asDF$site,
        as.character(setdiff(seq_len(nrow(data)), 4L)))
})

test_that("PCoA core reports non-Euclidean and degenerate states honestly", {
    data <- pcoa_small_data()
    prepared <- miso_prepare_resemblance(
        data=data, vars=pcoa_feature_names(data), factor=NULL,
        transform="none", distance="bray", seed=0,
        requireFactor=FALSE, distBinary=FALSE)
    result <- .misoPcoa(prepared$dist)
    expect_true(any(result$negative))
    expect_true(any(grepl("negative eigenvalue", result$warnings)))
    expect_true(all(is.na(result$explained[result$negative])))

    nonFinite <- prepared$dist
    nonFinite[[1L]] <- NA_real_
    expect_match(.misoPcoa(nonFinite)$message, "non-finite")
    negative <- prepared$dist
    negative[[1L]] <- -1
    expect_match(.misoPcoa(negative)$message, "non-negative")
    zero <- stats::as.dist(matrix(0, 4, 4))
    expect_match(.misoPcoa(zero)$message, "All dissimilarities are zero")
    expect_match(.misoPcoa(matrix(1, 2, 2))$message,
        "prepared dissimilarity")

    oneAxis <- .misoPcoa(stats::dist(matrix(seq_len(5), ncol=1L)))
    expect_false(oneAxis$error)
    expect_identical(oneAxis$positiveAxisCount, 1L)
    expect_false(.misoPreparePcoaPlot(oneAxis)$available)
})

test_that("PCoA plot is equal-scaled, accessible, deterministic, and honest", {
    data <- pcoa_small_data()
    prepared <- miso_prepare_resemblance(
        data=data, vars=pcoa_feature_names(data), factor="group",
        transform="none", distance="bray", seed=0,
        requireFactor=FALSE, distBinary=FALSE)
    result <- .misoPcoa(prepared$dist, groups=prepared$group)

    set.seed(781)
    before <- .Random.seed
    plotData <- .misoPreparePcoaPlot(
        result, showCentroids=TRUE, showSpiders=TRUE, maxPoints=10L)
    plot <- .misoBuildPcoaPlot(plotData)
    expect_identical(.Random.seed, before)
    expect_s3_class(plot, "ggplot")
    expect_identical(plot$theme$plot.background$fill, "transparent")
    expect_identical(plot$theme$panel.background$fill, "transparent")
    expect_identical(plot$coordinates$ratio, 1)
    expect_identical(plot$theme, .misoPlotTheme())
    expect_identical(plotData$displayed, 10L)
    expect_identical(plotData$total, nrow(result$points))
    expect_identical(plotData$sites$centroid1,
        unname(result$centroids[match(plotData$sites$group,
            rownames(result$centroids)), 1L]))
    expect_identical(plotData$sites$centroid2,
        unname(result$centroids[match(plotData$sites$group,
            rownames(result$centroids)), 2L]))
    geomClasses <- vapply(plot$layers,
        function(layer) class(layer$geom)[[1L]], character(1))
    expect_true("GeomSegment" %in% geomClasses)
    expect_false(any(grepl("Ellipse", geomClasses)))
    scaleAesthetics <- unlist(lapply(plot$scales$scales,
        function(scale) scale$aesthetics))
    expect_true("colour" %in% scaleAesthetics)
    expect_true("shape" %in% scaleAesthetics)

    ungrouped <- .misoPcoa(prepared$dist)
    ungroupedData <- .misoPreparePcoaPlot(
        ungrouped, showCentroids=TRUE, showSpiders=TRUE)
    expect_false(ungroupedData$grouped)
    expect_false(ungroupedData$showCentroids)
    expect_false(ungroupedData$showSpiders)
    expect_s3_class(.misoBuildPcoaPlot(ungroupedData), "ggplot")

    set.seed(782)
    beforeCore <- .Random.seed
    invisible(.misoPcoa(prepared$dist, groups=prepared$group))
    expect_identical(.Random.seed, beforeCore)
})

test_that("PCoA capped selection is exact, balanced, and identity-stable", {
    groups <- rep(c("A", "B", "C", "D", "E"), each=10L)
    identities <- sprintf("site_%03d", seq_along(groups))

    set.seed(783)
    before <- .Random.seed
    selected <- .misoPcoaBalancedIndices(
        groups, maxPoints=12L, identities=identities)
    expect_identical(.Random.seed, before)
    expect_length(selected, 12L)
    expect_length(unique(selected), 12L)
    expect_identical(
        unname(attr(selected, "allocation")),
        c(3L, 3L, 2L, 2L, 2L))
    expect_identical(attr(selected, "displayedGroups"),
        c("A", "B", "C", "D", "E"))
    expect_identical(attr(selected, "omittedGroups"), character())

    reordered <- rev(seq_along(groups))
    selectedReordered <- .misoPcoaBalancedIndices(
        groups[reordered], maxPoints=12L,
        identities=identities[reordered])
    expect_identical(
        identities[selected],
        identities[reordered][selectedReordered])

    constrained <- .misoPcoaBalancedIndices(
        groups, maxPoints=3L, identities=identities)
    expect_length(constrained, 3L)
    expect_identical(attr(constrained, "displayedGroups"),
        c("A", "B", "C"))
    expect_identical(attr(constrained, "omittedGroups"), c("D", "E"))
    expect_identical(unname(attr(constrained, "allocation")),
        c(1L, 1L, 1L))
})

test_that("PCoA cap handles more singleton groups than the point budget", {
    groups <- sprintf("group_%04d", seq_len(1001L))
    result <- pcoa_plot_stub(groups)

    set.seed(784)
    before <- .Random.seed
    first <- .misoPreparePcoaPlot(
        result, showCentroids=TRUE, showSpiders=TRUE, maxPoints=1000L)
    second <- .misoPreparePcoaPlot(
        result, showCentroids=TRUE, showSpiders=TRUE, maxPoints=1000L)
    expect_identical(.Random.seed, before)
    expect_identical(first, second)
    expect_identical(first$displayed, 1000L)
    expect_length(unique(first$sites$site), 1000L)
    expect_identical(first$omittedGroupCount, 1L)
    expect_length(first$allGroups, 1001L)
    expect_length(first$displayedGroups, 1000L)
    expect_identical(first$omittedGroups, "group_1001")
    expect_true(first$neutral)
    expect_false(first$showCentroids)
    expect_false(first$showSpiders)
    expect_null(first$centroids)
    expect_false(any(c("centroid1", "centroid2") %in% names(first$sites)))

    description <- paste(.misoPcoaSamplingDisclosure(first), collapse=" ")
    expect_match(description, "1000 of 1001 sites are shown", fixed=TRUE)
    expect_match(description, "1 is omitted from the image", fixed=TRUE)
    expect_match(description, "1 group is omitted from the image", fixed=TRUE)

    plot <- .misoBuildPcoaPlot(first)
    expect_s3_class(plot, "ggplot")
    geomClasses <- vapply(plot$layers,
        function(layer) class(layer$geom)[[1L]], character(1))
    expect_identical(unname(geomClasses), "GeomPoint")
})

test_that("PCoA capped grouped overlays cover only represented groups", {
    groups <- rep(c("A", "B", "C", "D", "E"), each=3L)
    result <- pcoa_plot_stub(groups)
    plotData <- .misoPreparePcoaPlot(
        result, showCentroids=TRUE, showSpiders=TRUE, maxPoints=3L)

    expect_false(plotData$neutral)
    expect_identical(plotData$displayedGroups, c("A", "B", "C"))
    expect_identical(plotData$omittedGroups, c("D", "E"))
    expect_identical(plotData$centroids$group, c("A", "B", "C"))
    expect_true(all(plotData$sites$group %in% plotData$centroids$group))
    expectedRows <- match(plotData$sites$group, rownames(result$centroids))
    expect_identical(plotData$sites$centroid1,
        unname(result$centroids[expectedRows, 1L]))
    expect_identical(plotData$sites$centroid2,
        unname(result$centroids[expectedRows, 2L]))
    expect_match(
        paste(.misoPcoaSamplingDisclosure(plotData), collapse=" "),
        "2 groups are omitted from the image", fixed=TRUE)

    plot <- .misoBuildPcoaPlot(plotData)
    expect_s3_class(plot, "ggplot")
    geomClasses <- vapply(plot$layers,
        function(layer) class(layer$geom)[[1L]], character(1))
    expect_true("GeomSegment" %in% geomClasses)
})

test_that("PCoA uses neutral styling beyond the supported group palette", {
    values <- cbind(seq_len(65), (seq_len(65)^2) %% 17)
    groups <- factor(paste0("group_", sprintf("%02d", seq_len(65))))
    result <- .misoPcoa(stats::dist(values), groups=groups)
    plotData <- .misoPreparePcoaPlot(
        result, showCentroids=TRUE, showSpiders=TRUE)
    plot <- .misoBuildPcoaPlot(plotData)
    expect_true(plotData$neutral)
    expect_false(plotData$showCentroids)
    expect_false(plotData$showSpiders)
    expect_null(plotData$centroids)
    expect_s3_class(plot, "ggplot")
    expect_false(any(vapply(plot$scales$scales, function(scale) {
        any(scale$aesthetics %in% c("colour", "shape"))
    }, logical(1))))
})

test_that("PCoA analysis hides empty output and clears stale output", {
    data <- pcoa_small_data()
    vars <- pcoa_feature_names(data)
    emptyAnalysis <- run_pcoa_private(data, character())
    empty <- emptyAnalysis$results
    expect_true(empty$guidance$visible)
    for (name in c("warnings", "ordination",
            "ordinationDescription", "centroids"))
        expect_false(empty[[name]]$visible, info=name)
    for (name in c("sites", "eigenvalues"))
        expect_true(empty[[name]]$visible, info=name)

    analysis <- run_pcoa_private(data, vars, factor="group",
        showCentroids=TRUE, showSpiders=TRUE)
    result <- analysis$results
    expect_false(result$guidance$visible)
    for (name in c("ordination",
            "sites", "centroids", "eigenvalues"))
        expect_true(result[[name]]$visible, info=name)
    expect_identical(nrow(result$sites$asDF), nrow(data))
    expect_identical(nrow(result$centroids$asDF),
        length(unique(data$group)))
    expect_false(any(is.nan(as.matrix(result$sites$asDF[, c(
        "PCoA1", "PCoA2")]))))
    expect_false(any(is.nan(result$eigenvalues$asDF$explained)))
    nonPositiveRows <- which(result$eigenvalues$asDF$sign != "Positive")
    expect_true(all(vapply(nonPositiveRows, function(i) {
        identical(result$eigenvalues$getCell(
            rowKey=as.character(i), col="explained")$value, "")
    }, logical(1))))
    expect_false(grepl(
        "Maps the main dimensions of dissimilarity among samples",
        miso_squish_result(result$ordinationDescription), fixed=TRUE))

    varsOption <- analysis$options$option("vars")
    varsOption$.__enclos_env__$private$.value <- character()
    analysis$.__enclos_env__$private$.run()
    result <- analysis$results
    expect_true(result$guidance$visible)
    expect_identical(nrow(result$sites$asDF), 0L)
    expect_identical(nrow(result$eigenvalues$asDF), 0L)
    expect_false(result$ordination$visible)
})


test_that("successful PCoA hides the empty description and keeps its native figure", {
    data <- pcoa_small_data()
    analysis <- run_pcoa_private(data, pcoa_feature_names(data), factor="group")
    result <- analysis$results
    expect_false(result$ordinationDescription$visible)
    expect_true(result$ordination$visible)
    expect_identical(trimws(miso_squish_result(
        result$ordinationDescription)), "character(0)")
})

test_that("unavailable PCoA ordination keeps its explanation without success prose", {
    data <- pcoa_small_data()
    analysis <- run_pcoa_private(data, "feature_01", distance="euclidean")
    result <- analysis$results
    expect_false(result$ordination$visible)
    expect_true(result$ordinationDescription$visible)
    description <- miso_squish_result(result$ordinationDescription)
    expect_match(description, "A two-dimensional plot is unavailable",
        fixed=TRUE)
    expect_match(description, "retain the fitted result", fixed=TRUE)
    expect_false(grepl(
        "Maps the main dimensions of dissimilarity among samples",
        description, fixed=TRUE))
})

test_that("PCoA plot options do not change numerical results", {
    data <- pcoa_small_data()
    vars <- pcoa_feature_names(data)
    plain <- do.call(pcoa, list(data=data, vars=vars, factor="group",
        showCentroids=FALSE, showSpiders=FALSE))
    decorated <- do.call(pcoa, list(data=data, vars=vars, factor="group",
        showCentroids=TRUE, showSpiders=TRUE))
    expect_equal(plain$sites$asDF, decorated$sites$asDF, tolerance=0)
    expect_equal(plain$centroids$asDF, decorated$centroids$asDF,
        tolerance=0)
    expect_equal(plain$eigenvalues$asDF,
        decorated$eigenvalues$asDF, tolerance=0)
    expect_false(plain$centroids$visible)
    expect_true(decorated$centroids$visible)
})

test_that("PCoA schema and menu follow the approved student contract", {
    analysis <- yaml::read_yaml(miso_fixture_path("jamovi", "pcoa.a.yaml"))
    ui <- yaml::read_yaml(miso_fixture_path("jamovi", "pcoa.u.yaml"))
    results <- yaml::read_yaml(miso_fixture_path("jamovi", "pcoa.r.yaml"))
    byName <- setNames(analysis$options,
        vapply(analysis$options, `[[`, character(1), "name"))
    optionOrder <- vapply(analysis$options, `[[`, character(1), "name")
    expect_identical(optionOrder[1:3], c("data", "vars", "factor"))
    expect_identical(byName$correction$default, "none")
    expect_false(byName$sqrtDist$default)
    expect_false(byName$showCentroids$default)
    expect_false(byName$showSpiders$default)
    expect_identical(analysis$menuSubtitle, "Visualise distance structure")

    findNode <- function(node, name) {
        if (is.list(node) && identical(node$name, name))
            return(node)
        if (is.list(node)) {
            for (child in node) {
                found <- findNode(child, name)
                if (!is.null(found)) return(found)
            }
        }
        NULL
    }
    expect_false(findNode(ui, "analysisChoices")$collapsed)
    expect_false(findNode(ui, "plotChoices")$collapsed)
    expect_true(findNode(ui, "corrections")$collapsed)
    expect_identical(findNode(ui, "factor")$maxItemCount, 1L)
    expect_identical(findNode(ui, "vars")$isTarget, TRUE)

    itemOrder <- vapply(results$items, `[[`, character(1), "name")
    expect_identical(itemOrder,
        c("guidance", "warnings",
            "ordinationDescription", "ordination", "sites",
            "centroids",
            "eigenvalues"))
    image <- results$items[[match("ordination", itemOrder)]]
    expect_identical(image$width, 600L)
    expect_identical(image$height, 500L)
    expect_identical(image$renderFun, ".plotPcoa")

    module <- yaml::read_yaml(miso_fixture_path("jamovi", "0000.yaml"))
    expect_match(module$description, "using vegan and ggplot2", fixed=TRUE)
    expect_false(grepl("base R", module$description, fixed=TRUE))
    names <- vapply(module$analyses, `[[`, character(1), "name")
    expect_identical(names[match("nmds", names) + 1L], "pcoa")
    pcoaMenu <- module$analyses[[match("pcoa", names)]]
    expect_identical(pcoaMenu$menuSubtitle, "Visualise distance structure")

    namespacePath <- testthat::test_path("..", "..", "NAMESPACE")
    if (file.exists(namespacePath))
        expect_true("export(pcoa)" %in% readLines(namespacePath, warn=FALSE))

    js <- readLines(miso_fixture_path("jamovi", "js", "pcoa.js"),
        warn=FALSE)
    expect_match(paste(js, collapse="\n"),
        "showCentroids\\.setEnabled\\(grouped\\)")
    expect_match(paste(js, collapse="\n"),
        "showSpiders\\.setEnabled\\(grouped\\)")
    expect_false(grepl("vars\\.focus\\(\\)", paste(js, collapse="\n")))
})

test_that("PCoA grouped and ungrouped plots render to PNG", {
    data <- pcoa_small_data()
    vars <- pcoa_feature_names(data)
    for (factorName in list(NULL, "group")) {
        analysis <- run_pcoa_private(data, vars,
            factor=factorName,
            showCentroids=!is.null(factorName),
            showSpiders=!is.null(factorName))
        plot <- .misoBuildPcoaPlot(
            analysis$.__enclos_env__$private$.state$plotData)
        path <- tempfile(fileext=".png")
        on.exit(unlink(path), add=TRUE)
        grDevices::png(path, width=600, height=500)
        print(plot)
        grDevices::dev.off()
        expect_gt(file.info(path)$size, 1000)
    }
})



test_that("PCoA exported API has durable user-facing documentation", {
    analysisPath <- testthat::test_path("..", "..", "jamovi", "pcoa.a.yaml")
    wrapperPath <- testthat::test_path("..", "..", "R", "pcoa.h.R")
    manualPath <- testthat::test_path("..", "..", "man", "pcoa.Rd")
    skip_if_not(file.exists(analysisPath))
    skip_if_not(file.exists(wrapperPath))
    skip_if_not(file.exists(manualPath))

    analysis <- yaml::read_yaml(analysisPath)
    expect_match(analysis$description$main,
        "descriptive principal coordinates analysis", ignore.case=TRUE)
    expect_match(analysis$description$main,
        "does not test group differences", fixed=TRUE)
    optionDescriptions <- vapply(analysis$options, function(option) {
        description <- option$description
        if (is.list(description)) description <- description$R
        if (is.null(description)) "" else description
    }, character(1))
    expect_length(optionDescriptions, 10L)
    expect_true(all(nzchar(trimws(optionDescriptions))))

    wrapper <- readLines(wrapperPath, warn=FALSE)
    manual <- readLines(manualPath, warn=FALSE)
    expect_false(any(grepl("^#' @param [A-Za-z][A-Za-z0-9]* \\.$",
        wrapper)))
    expect_false(any(grepl("^\\\\item\\{[^}]+\\}\\{\\.\\}$", manual)))
    expect_match(paste(manual, collapse="\n"),
        "returns the jamovi analysis results object", ignore.case=TRUE)
})

test_that("PCoA ordination renders from serialized Image state alone", {
    data <- pcoa_small_data()
    vars <- pcoa_feature_names(data)
    analysis <- run_pcoa_private(
        data, vars,
        factor="group",
        showCentroids=TRUE,
        showSpiders=TRUE)
    image <- analysis$results$ordination
    state <- image$state
    expect_false(is.null(state))
    expect_true(state$available)

    restored <- unserialize(serialize(state, NULL))
    analysis$.__enclos_env__$private$.state$plotData <- NULL
    image$setState(restored)

    file <- tempfile(fileext=".png")
    on.exit({
        if (grDevices::dev.cur() > 1L)
            grDevices::dev.off()
        unlink(file)
    }, add=TRUE)
    grDevices::png(file, width=600, height=500)
    analysis$.__enclos_env__$private$.plotPcoa(image)
    grDevices::dev.off()
    expect_gt(file.info(file)$size, 1000)
})


test_that("structural sample inputs rebuild site rows while centroid rows update in place", {
    index <- seq_len(9L)
    data <- data.frame(
        feature_01=1 + index %% 4,
        feature_02=2 + (index * 3) %% 5,
        feature_03=1 + (index * 2) %% 3,
        feature_04=4 + (index * 5) %% 7,
        feature_05=c(2, 4, 6, 8, 10, 12, 14, 16, NA),
        group=factor(rep(c("A", "B", "C"), each=3L)))
    options <- pcoaOptions$new(
        vars=paste0("feature_0", 1:5),
        factor="group",
        showCentroids=TRUE)
    analysis <- pcoaClass$new(options=options, data=data)
    suppressWarnings(suppressMessages(analysis$.__enclos_env__$private$.run()))
    siteKeys <- analysis$results$sites$rowKeys
    centroidKeys <- analysis$results$centroids$rowKeys
    siteRowsBefore <- nrow(analysis$results$sites$asDF)

    varsOption <- options$option("vars")
    varsOption$.__enclos_env__$private$.value <- paste0("feature_0", 1:4)
    suppressWarnings(suppressMessages(analysis$.__enclos_env__$private$.run()))
    expect_false(identical(analysis$results$sites$rowKeys, siteKeys))
    expect_gt(nrow(analysis$results$sites$asDF), siteRowsBefore)
    # Centroids remain one row per retained group: structure is stable.
    expect_identical(analysis$results$centroids$rowKeys, centroidKeys)
    expect_setequal(analysis$results$centroids$asDF$group, c("A", "B", "C"))
})

test_that("PCoA reports square-root and correction choices in table notes", {
    data <- pcoa_small_data()
    analysis <- run_pcoa_private(data,
        vars=pcoa_feature_names(data), factor="group", sqrtDist=TRUE,
        correction="lingoes")
    expect_true(analysis$results$sites$visible)
    expect_match(miso_table_note(analysis$results$sites, "method"), "Square-root")
    expect_match(miso_table_note(analysis$results$eigenvalues, "denominator"), "correction")
})

test_that("publication output omits routine explanatory prose", {
    analysis <- run_pcoa_private(pcoa_small_data(),
        vars=pcoa_feature_names(pcoa_small_data()))
    expect_false(analysis$results$ordinationDescription$visible)
})

test_that("PCoA notes identify presence/absence distances", {
    data <- pcoa_small_data()
    analysis <- run_pcoa_private(data, vars=pcoa_feature_names(data),
        distBinary=TRUE)
    expect_match(miso_table_note(analysis$results$sites, "method"),
        "presence/absence", fixed=TRUE)
})
