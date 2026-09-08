nmds_state_data <- function(n=18L, groups=3L) {
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

run_nmds_private <- function(data, ...) {
    options <- nmdsOptions$new(...)
    analysis <- nmdsClass$new(options=options, data=data)
    suppressWarnings(suppressMessages(
        analysis$.__enclos_env__$private$.run()))
    analysis
}

test_that("nMDS lifecycle seams retain the fitted state contract", {
    analysis <- run_nmds_private(
        nmds_state_data(),
        vars=paste0("feature_0", 1:4),
        seed=123,
        nmdsTrymax=2)
    private <- analysis$.__enclos_env__$private
    expect_true(all(c(".prepareNmds", ".fitNmds",
        ".assembleNmdsResults") %in% names(private)))
    expect_true(is.list(private$.state$fitArguments))
    expect_identical(private$.state$effectiveK, 2L)
    expect_true(is.finite(private$.state$fit$stress))
    expect_equal(nrow(analysis$results$sites$asDF), nrow(nmds_state_data()))
})

nmds_squish <- function(value) {
    trimws(gsub("[[:space:]]+", " ", as.character(value)))
}

nmds_fixture <- function(name) {
    read.csv(
        test_path("..", "manual", name),
        stringsAsFactors=FALSE,
        check.names=FALSE)
}

independent_fixture_nmds <- function(data, k=2L, featureScores=TRUE) {
    vars <- grep("^feature_[0-9]+$", names(data), value=TRUE)
    RNGkind("Mersenne-Twister", "Inversion", "Rejection")
    RNGversion("4.0.0")
    set.seed(123)
    fit <- suppressWarnings(suppressMessages(vegan::metaMDS(
        data[vars], distance="bray", k=k, trymax=20, maxit=200,
        wascores=featureScores, autotransform=FALSE, trace=FALSE)))
    postFitRng <- .Random.seed
    sites <- as.matrix(vegan::scores(
        fit, display="sites", choices=seq_len(k)))
    features <- if (featureScores) {
        as.matrix(vegan::scores(
            fit, display="species", choices=seq_len(k)))
    } else {
        NULL
    }
    environment <- lapply(c("temperature", "pH"), function(name) {
        assign(".Random.seed", postFitRng, envir=.GlobalEnv)
        fitted <- vegan::envfit(
            sites,
            data.frame(value=data[[name]], row.names=seq_len(nrow(data))),
            permutations=99L,
            choices=seq_len(k))
        list(
            r2=unname(fitted$vectors$r[[1L]]),
            p=unname(fitted$vectors$pvals[[1L]]),
            endpoint=as.numeric(vegan::scores(
                fitted, display="vectors", choices=seq_len(k))[1L, ]))
    })
    names(environment) <- c("temperature", "pH")
    list(
        fit=fit,
        sites=sites,
        features=features,
        environment=environment)
}

expect_nmds_matches_independent <- function(data, k=2L) {
    vars <- grep("^feature_[0-9]+$", names(data), value=TRUE)
    actual <- run_nmds_private(
        data,
        vars=vars,
        factor="group",
        nmdsEnv=c("temperature", "pH"),
        nmdsEnvPerm=99,
        nmdsSpecies=TRUE,
        nmdsK=k,
        seed=123,
        nmdsTrymax=20,
        nmdsMaxit=200)
    expected <- independent_fixture_nmds(data, k=k)
    private <- actual$.__enclos_env__$private
    siteTable <- actual$results$sites$asDF
    featureTable <- actual$results$features$asDF
    envTable <- actual$results$envfit$asDF

    expect_equal(private$.state$fit$stress, expected$fit$stress, tolerance=0)
    aligned <- vegan::procrustes(
        expected$sites, private$.state$sites, symmetric=TRUE)
    expect_lt(sqrt(sum(aligned$residuals^2)), 1e-8)
    expect_equal(
        sort(as.numeric(stats::dist(expected$sites))),
        sort(as.numeric(stats::dist(private$.state$sites))),
        tolerance=1e-10)
    expect_equal(
        unname(as.matrix(siteTable[paste0("NMDS", seq_len(k))])),
        unname(private$.state$sites),
        tolerance=0)
    expect_equal(
        unname(as.matrix(featureTable[paste0("NMDS", seq_len(k))])),
        unname(private$.state$features),
        tolerance=0)
    expect_equal(
        unname(private$.state$features),
        unname(expected$features),
        tolerance=0)
    expect_equal(
        unname(as.matrix(featureTable[paste0("NMDS", seq_len(k))])),
        unname(expected$features),
        tolerance=0)
    expect_identical(nrow(siteTable), nrow(data))
    expect_identical(nrow(featureTable), length(vars))
    expect_identical(nrow(envTable), 2L)
    expect_equal(
        unname(as.matrix(envTable[paste0("NMDS", seq_len(k))])),
        unname(private$.state$vectorEndpoints),
        tolerance=0)
    for (name in names(expected$environment)) {
        row <- match(name, envTable$variable)
        expect_equal(envTable$r2[[row]], expected$environment[[name]]$r2,
                     tolerance=0)
        expect_equal(envTable$p[[row]], expected$environment[[name]]$p,
                     tolerance=0)
        expect_equal(
            unname(private$.state$vectorEndpoints[name, seq_len(k)]),
            expected$environment[[name]]$endpoint,
            tolerance=0)
        expect_equal(
            unname(as.numeric(envTable[row, paste0("NMDS", seq_len(k))])),
            expected$environment[[name]]$endpoint,
            tolerance=0)
    }
    invisible(actual)
}

expect_nmds_plot_inside_frame <- function(data) {
    vars <- grep("^feature_[0-9]+$", names(data), value=TRUE)
    analysis <- run_nmds_private(
        data,
        vars=vars,
        nmdsSpecies=TRUE,
        nmdsEnv=c("temperature", "pH"),
        nmdsEnvPerm=99,
        seed=123,
        nmdsTrymax=20,
        nmdsMaxit=200)
    private <- analysis$.__enclos_env__$private
    before <- unserialize(serialize(private$.state, NULL))
    path <- tempfile(fileext=".png")
    on.exit(unlink(path), add=TRUE)

    plot <- private$.buildNmdsPlot()
    shepard <- private$.buildShepardPlot()
    expect_s3_class(plot, "ggplot")
    expect_identical(plot$theme$plot.background$fill, "transparent")
    expect_identical(plot$theme$panel.background$fill, "transparent")
    expect_s3_class(shepard, "ggplot")
    expect_identical(shepard$theme$plot.background$fill, "transparent")
    expect_identical(shepard$theme$panel.background$fill, "transparent")
    expect_true(
        inherits(plot$coordinates, "CoordFixed") ||
            identical(plot$coordinates$ratio, 1),
        info="coord_equal must keep equal physical units on both axes")
    expect_identical(plot$theme, .misoPlotTheme())

    limits <- plot$coordinates$limits
    expect_true(all(is.finite(limits$x)))
    expect_true(all(is.finite(limits$y)))
    expect_gt(diff(limits$x), 0)
    expect_gt(diff(limits$y), 0)

    built <- ggplot2::ggplot_build(plot)
    displayed <- do.call(rbind, lapply(built$data, function(layer) {
        x <- intersect(c("x", "xend"), names(layer))
        y <- intersect(c("y", "yend"), names(layer))
        if (length(x) == 0L || length(y) == 0L)
            return(NULL)
        data.frame(
            x=unlist(layer[x], use.names=FALSE),
            y=unlist(layer[y], use.names=FALSE))
    }))
    displayed <- displayed[is.finite(displayed$x) & is.finite(displayed$y), ]
    expect_true(all(displayed$x >= limits$x[[1L]] & displayed$x <= limits$x[[2L]]))
    expect_true(all(displayed$y >= limits$y[[1L]] & displayed$y <= limits$y[[2L]]))

    grDevices::png(path, width=900, height=700)
    rendered <- private$.plotNmds(analysis$results$ordination)
    grDevices::dev.off()
    expect_true(isTRUE(rendered))
    expect_identical(private$.state, before)
    expect_gt(file.info(path)$size, 0)
    invisible(analysis)
}

expect_nmds_visibility <- function(result, visible, hidden) {
    for (name in visible)
        expect_true(result[[name]]$visible, info=paste(name, "should be visible"))
    fixed <- c("sites", "stress")
    for (name in hidden)
        if (name %in% fixed)
            expect_true(result[[name]]$visible, info=paste(name, "fixed shell should remain visible"))
        else
            expect_false(result[[name]]$visible, info=paste(name, "should be hidden"))
}

expect_only_nmds_guidance <- function(result) {
    expect_nmds_visibility(
        result,
        "guidance",
        c(
            "warnings", "ordination", "ordinationDescription",
            "stress", "shepard", "shepardDescription", "envfit",
            "sites", "features"))
}

find_nmds_yaml_node <- function(node, name) {
    if (is.list(node) && identical(node$name, name))
        return(node)
    if (is.list(node)) {
        for (child in node) {
            found <- find_nmds_yaml_node(child, name)
            if (! is.null(found))
                return(found)
        }
    }
    NULL
}

test_that("nMDS schema preserves the API and exposes the approved student contract", {
    analysis <- yaml::read_yaml(miso_fixture_path("jamovi", "nmds.a.yaml"))
    options <- analysis$options
    by_name <- setNames(options, vapply(options, `[[`, character(1), "name"))
    names_in_order <- vapply(options, `[[`, character(1), "name")

    expect_identical(by_name$vars$title, "Feature Variables")
    expect_identical(by_name$factor$title, "Grouping Variable")
    expect_identical(by_name$nmdsEnv$title, "Environmental Variables")
    expect_true(by_name$distBinary$hidden)
    expect_true(by_name$nmdsK$hidden)
    expect_identical(by_name$nmdsK$default, 2L)
    expect_identical(by_name$nmdsK$min, 2L)
    expect_identical(by_name$nmdsK$max, 3L)
    expect_identical(by_name$nmdsEnvPerm$default, 99L)
    expect_true(by_name$nmdsShepard$default)
    expect_true(by_name$nmdsOverlay$default)
    expect_false(by_name$nmdsSpecies$default)
    expect_false(by_name$nmdsHull$default)
    expect_false(by_name$nmdsEllipse$default)
    expect_false(by_name$nmdsSpider$default)
    expect_identical(
        names_in_order[seq_len(match("nmdsSpider", names_in_order))],
        c(
            "data", "vars", "factor", "transform", "distance", "distBinary",
            "seed", "nmdsK", "nmdsTrymax", "nmdsMaxit", "nmdsShepard",
            "nmdsOverlay", "nmdsEnv", "nmdsSpecies", "nmdsHull",
            "nmdsEllipse", "nmdsSpider"))
    expect_identical(names_in_order[match("nmdsSpider", names_in_order) + 1L],
                     "nmdsEnvPerm")
})

test_that("nMDS UI uses required-first progressive disclosure", {
    ui_path <- miso_fixture_path("jamovi", "nmds.u.yaml")
    ui <- yaml::read_yaml(ui_path)
    expect_false(find_nmds_yaml_node(ui, "analysisChoices")$collapsed)
    expect_false(find_nmds_yaml_node(ui, "plots")$collapsed)
    expect_true(find_nmds_yaml_node(ui, "groupOptions")$collapsed)
    expect_true(find_nmds_yaml_node(ui, "environmentAssessment")$collapsed)
    expect_true(find_nmds_yaml_node(ui, "reproducibility")$collapsed)
    expect_true(is.null(find_nmds_yaml_node(ui, "distBinary")))
    expect_true(is.null(find_nmds_yaml_node(ui, "nmdsK")))
    ui_source <- paste(readLines(ui_path), collapse="\n")
    expect_match(ui_source, "Overlays are descriptive only")
    expect_match(ui_source, "P-values are unadjusted")
    expect_false(grepl("Styles the existing ordination", ui_source, fixed=TRUE))
    expect_false(grepl("Fits descriptive vectors", ui_source, fixed=TRUE))
    expect_false(grepl("Dimensions: 2 for new analyses", ui_source, fixed=TRUE))

    source_path <- miso_fixture_path("jamovi", "js", "nmds.js")
    expect_true(file.exists(source_path), info="compiled nMDS UI controller is missing")
    if (! file.exists(source_path))
        return(invisible())
    source <- paste(readLines(source_path), collapse="\n")
    expect_match(source, "nmdsOverlay\\.setEnabled")
    expect_match(source, "nmdsEnvPerm\\.setEnabled")
    expect_match(source, "groupOptions\\.expand")
    expect_match(source, "environmentAssessment\\.expand")
    expect_match(source, "reproducibility\\.expand")
    expect_match(source, "containsFocus")
    expect_match(source, "focusControl\\(ui\\.factor\\)")
    expect_match(source, "focusControl\\(ui\\.nmdsEnv\\)")
})

test_that("nMDS result schema contains no initially visible shell", {
    items <- yaml::read_yaml(
        miso_fixture_path("jamovi", "nmds.r.yaml"))$items
    by_name <- setNames(items, vapply(items, `[[`, character(1), "name"))

    expect_true(all(vapply(items, function(x) identical(x$visible, FALSE), logical(1))))
    expected_clear_with <- c(
        "vars", "factor", "transform", "distance", "distBinary", "seed",
        "nmdsK", "nmdsTrymax", "nmdsMaxit", "nmdsEnv", "nmdsEnvPerm")
    expect_true(all(vapply(
        items, function(x) identical(x$clearWith, expected_clear_with),
        logical(1))))
    expect_identical(
        vapply(items, `[[`, character(1), "name"),
        c(
            "guidance", "warnings",
            "ordinationDescription", "ordination", "sites",
            "stress", "shepardDescription", "shepard",
            "shepardPairs",
            "envfit", "features"
            ))
    expect_identical(by_name$guidance$type, "Html")
    expect_identical(by_name$ordinationDescription$type, "Html")
    expect_identical(by_name$shepardDescription$type, "Html")
    expect_identical(by_name$shepardPairs$type, "Table")
    expect_identical(
        vapply(by_name$envfit$columns, `[[`, character(1), "name"),
        c("variable", "r2", "p", "samples", "permutations", "NMDS1", "NMDS2", "NMDS3"))
    envfit_columns <- setNames(
        by_name$envfit$columns,
        vapply(by_name$envfit$columns, `[[`, character(1), "name"))
    expect_identical(
        envfit_columns$permutations$title,
        "Permutations")
    expect_identical(
        vapply(by_name$sites$columns, `[[`, character(1), "name"),
        c("row", "NMDS1", "NMDS2", "NMDS3", "group"))
    expect_identical(
        vapply(by_name$features$columns, `[[`, character(1), "name"),
        c("feature", "NMDS1", "NMDS2", "NMDS3"))
})

test_that("new and incomplete nMDS analyses show one actionable state", {
    data <- nmds_state_data()

    noneAnalysis <- run_nmds_private(data, vars=character())
    none <- noneAnalysis$results
    expect_match(
        nmds_squish(none$guidance$asString()),
        "at least two numeric Feature variables")
    expect_only_nmds_guidance(none)
    expect_null(noneAnalysis$.__enclos_env__$private$.buildNmdsPlot())

    oneAnalysis <- run_nmds_private(data, vars="feature_01")
    one <- oneAnalysis$results
    expect_match(
        nmds_squish(one$guidance$asString()),
        "at least two usable numeric Feature variables")
    expect_only_nmds_guidance(one)
    expect_null(oneAnalysis$.__enclos_env__$private$.buildNmdsPlot())
})


test_that("optional metadata never changes the fixed-seed core fit", {
    data <- nmds_state_data()
    data$group[c(2, 7)] <- NA
    data$temperature[c(3, 11)] <- NA
    base <- suppressWarnings(nmds(
        data=data,
        vars=paste0("feature_0", 1:4),
        seed=123,
        nmdsTrymax=20))
    layered <- suppressWarnings(nmds(
        data=data,
        vars=paste0("feature_0", 1:4),
        factor="group",
        nmdsEnv="temperature",
        seed=123,
        nmdsTrymax=20))

    expect_equal(
        layered$sites$asDF[c("NMDS1", "NMDS2")],
        base$sites$asDF[c("NMDS1", "NMDS2")],
        tolerance=0)
    expect_identical(layered$sites$asDF$row, base$sites$asDF$row)
    expect_identical(
        layered$sites$asDF$group[c(2, 7)],
        rep("Unassigned (missing)", 2))
    expect_equal(
        layered$stress$asDF$value[1],
        base$stress$asDF$value[1])
})

test_that("site scores retain original rows and preparation exclusion counts", {
    data <- nmds_state_data()
    data$feature_02[3] <- NA
    data[5, paste0("feature_0", 1:4)] <- 0
    data$group[7] <- NA

    analysis <- run_nmds_private(
        data,
        vars=paste0("feature_0", 1:4),
        factor="group",
        distance="euclidean",
        seed=123)
    prep <- analysis$.__enclos_env__$private$.state$prep

    expect_identical(
        analysis$results$sites$asDF$row,
        as.integer(setdiff(seq_len(nrow(data)), c(3L, 5L))))
    expect_identical(prep$rowIndex, setdiff(seq_len(nrow(data)), c(3L, 5L)))
    expect_identical(prep$rowsMissingExcluded, 1L)
    expect_identical(prep$rowsZeroExcluded, 1L)
    expect_identical(prep$featuresZeroExcluded, 0L)
    expect_identical(
        analysis$results$sites$asDF$group[
            analysis$results$sites$asDF$row == 7L],
        "Unassigned (missing)")
})

test_that("observed missing-label text remains distinct from missing groups", {
    data <- nmds_state_data()
    data$group <- as.character(data$group)
    data$group[1] <- "Unassigned (missing)"
    data$group[2] <- NA
    data$group[3] <- "Unassigned (missing) (observed level)"

    result <- run_nmds_private(
        data,
        vars=paste0("feature_0", 1:4),
        factor="group",
        seed=123)$results

    expect_identical(
        result$sites$asDF$group[1],
        "Unassigned (missing) (observed level 2)")
    expect_identical(result$sites$asDF$group[2], "Unassigned (missing)")
    expect_identical(
        result$sites$asDF$group[3],
        "Unassigned (missing) (observed level)")
})

test_that("too few usable sites gives dimensionality-specific guidance", {
    data <- nmds_state_data(n=2L, groups=2L)
    result <- run_nmds_private(
        data,
        vars=paste0("feature_0", 1:4),
        distance="euclidean")$results
    guidance <- nmds_squish(result$guidance$asString())

    expect_match(guidance, "2-dimensional nMDS")
    expect_match(guidance, "at least 3 usable sites")
    expect_match(guidance, "missing\\s+feature values")
    expect_match(guidance, "Feature variable assignments")
    expect_only_nmds_guidance(result)
})

test_that("fewer than two retained features returns one correction state", {
    data <- nmds_state_data()
    data$empty_feature <- 0
    result <- run_nmds_private(
        data,
        vars=c("feature_01", "empty_feature"),
        distance="euclidean")$results

    expect_match(
        nmds_squish(result$guidance$asString()),
        "at least two usable numeric Feature variables")
    expect_only_nmds_guidance(result)
})

test_that("signed transformations name incompatible dissimilarities", {
    data <- nmds_state_data()
    result <- run_nmds_private(
        data,
        vars=paste0("feature_0", 1:4),
        transform="standardize",
        distance="bray")$results
    guidance <- nmds_squish(result$guidance$asString())

    expect_match(guidance, "Bray-Curtis")
    expect_match(guidance, "Standardize")
    expect_match(
        guidance,
        "Euclidean, Manhattan, Canberra, Gower, or\\s+Mahalanobis")
    expect_only_nmds_guidance(result)
})

test_that("signed transformations retain compatible Euclidean workflows", {
    result <- run_nmds_private(
        nmds_state_data(),
        vars=paste0("feature_0", 1:4),
        transform="standardize",
        distance="euclidean",
        seed=123)$results

    expect_false(result$guidance$visible)
    expect_true(result$ordination$visible)
    expect_true(all(is.finite(result$sites$asDF$NMDS1)))
    expect_true(all(is.finite(result$sites$asDF$NMDS2)))
})

test_that("non-finite transformations give transformation-specific guidance", {
    data <- nmds_state_data()
    data$feature_02 <- 5
    result <- run_nmds_private(
        data,
        vars=paste0("feature_0", 1:4),
        transform="standardize",
        distance="euclidean")$results
    guidance <- as.character(result$guidance$asString())

    expect_match(guidance, "Standardize")
    expect_match(guidance, "empty samples")
    expect_match(guidance, "feature variation")
    expect_only_nmds_guidance(result)
})

test_that("constant dissimilarities give one actionable failure state", {
    data <- nmds_state_data()
    data[paste0("feature_0", 1:4)] <- lapply(
        data[paste0("feature_0", 1:4)],
        function(x) rep(x[[1L]], length(x)))
    result <- run_nmds_private(
        data,
        vars=paste0("feature_0", 1:4),
        distance="euclidean")$results
    guidance <- as.character(result$guidance$asString())

    expect_match(guidance, "constant\\s+dissimilarities")
    expect_match(guidance, "empty samples")
    expect_match(guidance, "transformation")
    expect_match(guidance, "feature\\s+variation")
    expect_only_nmds_guidance(result)
})

test_that("model errors return escaped correction-oriented guidance", {
    testthat::local_mocked_bindings(
        metaMDS=function(...) stop("model <failure> & detail"),
        .package="vegan")
    result <- run_nmds_private(
        nmds_state_data(),
        vars=paste0("feature_0", 1:4),
        distance="euclidean")$results
    guidance <- as.character(result$guidance$asString())

    expect_match(guidance, "nMDS could not find a solution")
    expect_match(guidance, "Technical detail")
    expect_match(guidance, "<failure> &amp; detail", fixed=TRUE)
    expect_only_nmds_guidance(result)
})


test_that("legacy 3D remains 3D and reports the projection honestly", {
    data <- nmds_state_data(n=24L)
    result <- suppressWarnings(nmds(
        data=data,
        vars=paste0("feature_0", 1:4),
        nmdsK=3,
        nmdsSpecies=TRUE,
        seed=123,
        nmdsTrymax=20))

    expect_true(all(is.finite(result$sites$asDF$NMDS3)))
    expect_true(all(is.finite(result$features$asDF$NMDS3)))
    expect_match(
        result$ordination$title,
        "three-dimensional nMDS solution")
})

test_that("diagnostics report fitted-object fields rather than requested settings", {
    data <- nmds_state_data()
    analysis <- run_nmds_private(
        data,
        vars=paste0("feature_0", 1:4),
        seed=123,
        nmdsTrymax=5,
        nmdsMaxit=80)
    result <- analysis$results
    fit <- analysis$.__enclos_env__$private$.state$fit
    values <- setNames(result$stress$asDF$value, result$stress$asDF$item)

    expect_identical(
        values[["Random starts tried"]],
        as.character(fit$tries[[1L]]))
    expect_identical(
        values[["Similar best solution repeats"]],
        as.character(fit$converged[[1L]]))
    expect_identical(
        values[["Iterations in retained solution"]],
        as.character(fit$iters[[1L]]))
    expect_identical(values[["Engine"]], as.character(fit$engine[[1L]]))
    expect_identical(
        names(values),
        c(
            "Stress",
            "Effective dimensions",
            "Similar best solution repeats",
            "Retained optimization stopping reason",
            "Random starts tried",
            "Best solution first found",
            "Iterations in retained solution",
            "Engine"))
})


test_that("small retained samples receive a stress warning", {
    result <- suppressWarnings(nmds(
        data=nmds_state_data(n=5L),
        vars=paste0("feature_0", 1:4),
        distance="euclidean",
        seed=123,
        nmdsTrymax=5))
    warnings <- as.character(result$warnings$asString())

    expect_match(
        warnings,
        "stress may be\\s+artificially low or uninformative")
})


test_that("unusable grouping disables only requested optional layers", {
    data <- nmds_state_data()
    data$one <- factor("A")
    analysis <- run_nmds_private(
        data,
        vars=paste0("feature_0", 1:4),
        factor="one",
        nmdsOverlay=TRUE,
        nmdsHull=TRUE,
        nmdsEllipse=TRUE,
        nmdsSpider=TRUE,
        seed=123,
        nmdsTrymax=5)
    result <- analysis$results
    overlays <- analysis$.__enclos_env__$private$.state$overlays
    warnings <- as.character(result$warnings$asString())

    expect_true(result$ordination$visible)
    expect_true(result$stress$visible)
    expect_identical(nrow(result$sites$asDF), nrow(data))
    expect_identical(
        overlays$requested,
        c(points=TRUE, hull=TRUE, ellipse=TRUE, spider=TRUE))
    expect_identical(
        overlays$effective,
        c(points=FALSE, hull=FALSE, ellipse=FALSE, spider=FALSE))
    expect_match(warnings, "point styling")
    expect_match(warnings, "hull layer")
    expect_match(warnings, "ellipse layer")
    expect_match(warnings, "spider layer")
})

test_that("missing and colliding group labels receive distinct styles", {
    data <- nmds_state_data()
    data$group <- as.character(data$group)
    data$group[1] <- "Unassigned (missing)"
    data$group[2] <- NA
    data$group[3] <- "Unassigned (missing) (observed level)"
    analysis <- run_nmds_private(
        data,
        vars=paste0("feature_0", 1:4),
        factor="group",
        seed=123,
        nmdsTrymax=5)
    styles <- analysis$.__enclos_env__$private$.state$overlays$styles

    expect_identical(
        analysis$results$sites$asDF$group[1:3],
        c(
            "Unassigned (missing) (observed level 2)",
            "Unassigned (missing)",
            "Unassigned (missing) (observed level)"))
    missingStyle <- styles[styles$group == "Unassigned (missing)", ]
    assignedStyles <- styles[styles$assigned, ]
    expect_identical(nrow(missingStyle), 1L)
    expect_identical(missingStyle$colour, "#7F7F7F")
    expect_identical(missingStyle$shape, 1L)
    expect_false(any(
        assignedStyles$colour == missingStyle$colour &
        assignedStyles$shape == missingStyle$shape))
})

test_that("group overlays use unique styles and cached validated geometry", {
    data <- nmds_state_data(n=30L, groups=5L)
    data$group[c(2L, 7L)] <- NA
    analysis <- run_nmds_private(
        data,
        vars=paste0("feature_0", 1:4),
        factor="group",
        nmdsOverlay=TRUE,
        nmdsHull=TRUE,
        nmdsEllipse=TRUE,
        nmdsSpider=TRUE,
        seed=123,
        nmdsTrymax=5)
    overlays <- analysis$.__enclos_env__$private$.state$overlays
    assignedStyles <- overlays$styles[overlays$styles$assigned, ]

    expect_identical(
        overlays$requested,
        c(points=TRUE, hull=TRUE, ellipse=TRUE, spider=TRUE))
    expect_true(all(overlays$effective))
    expect_identical(nrow(assignedStyles), 5L)
    expect_identical(
        sum(analysis$results$sites$asDF$group == "Unassigned (missing)"),
        2L)
    expect_identical(
        length(unique(paste(assignedStyles$colour, assignedStyles$shape))),
        5L)
    expect_identical(length(overlays$hull), 5L)
    expect_identical(length(overlays$ellipse), 5L)
    expect_identical(length(overlays$spider), 5L)
    expect_identical(
        overlays$lineTypes,
        c(hull=1L, ellipse=2L, spider=3L))
    expect_identical(length(unique(overlays$lineTypes)), 3L)
    expect_true(all(vapply(
        overlays$hull,
        function(x) is.matrix(x) && ncol(x) == 2L && nrow(x) >= 4L,
        logical(1))))
    expect_true(all(vapply(
        overlays$ellipse,
        function(x) identical(dim(x), c(101L, 2L)),
        logical(1))))
    expect_true(all(vapply(
        overlays$spider,
        function(x) {
            length(x$centre) == 2L && is.matrix(x$segments) &&
                ncol(x$segments) == 4L && nrow(x$segments) >= 1L
        },
        logical(1))))
})

test_that("more than 64 groups retain results and use a neutral plot", {
    n <- 130L
    groupIndex <- rep(seq_len(65L), each=2L)
    index <- seq_len(n)
    data <- data.frame(
        feature_01=1 + groupIndex + index %% 5L,
        feature_02=2 + 2 * groupIndex + index %% 7L,
        feature_03=3 + groupIndex + rev(index %% 6L),
        feature_04=1 + (index %% 3L) * (1 + groupIndex %% 5L),
        group=factor(sprintf("group-%02d", groupIndex)),
        temperature=10 + index,
        pH=6 + (index %% 5L) / 10,
        check.names=FALSE)
    analysis <- run_nmds_private(
        data,
        vars=paste0("feature_0", 1:4),
        factor="group",
        nmdsOverlay=TRUE,
        nmdsHull=TRUE,
        nmdsEllipse=TRUE,
        nmdsSpider=TRUE,
        nmdsSpecies=TRUE,
        nmdsEnv=c("temperature", "pH"),
        nmdsEnvPerm=99,
        seed=123,
        nmdsTrymax=5)
    private <- analysis$.__enclos_env__$private
    plot <- private$.buildNmdsPlot()
    pointLayer <- plot$layers[[which(vapply(
        plot$layers,
        function(layer) inherits(layer$geom, "GeomPoint"),
        logical(1)))[[1L]]]]
    description <- nmds_squish(
        analysis$results$ordinationDescription$asString())

    expect_identical(nrow(analysis$results$sites$asDF), n)
    expect_identical(nrow(analysis$results$features$asDF), 4L)
    expect_identical(nrow(analysis$results$envfit$asDF), 2L)
    expect_true(nrow(analysis$results$stress$asDF) > 0L)
    expect_identical(length(private$.state$assignedGroupLevels), 65L)
    expect_false(any(private$.state$overlays$effective))
    expect_identical(nrow(private$.state$overlays$styles), 0L)
    expect_false(any(c("colour", "shape") %in% names(pointLayer$mapping)))
    expect_true(any(grepl(
        "at most 64 distinguishable group styles",
        private$.state$warnings,
        fixed=TRUE)))
})

test_that("all plot-only toggles preserve exact numerical results", {
    data <- nmds_state_data(n=30L, groups=5L)
    common <- list(
        data=data,
        vars=paste0("feature_0", 1:4),
        factor="group",
        nmdsEnv=c("temperature", "pH"),
        nmdsEnvPerm=99,
        nmdsSpecies=TRUE,
        seed=123,
        nmdsTrymax=5)
    baseline <- suppressWarnings(do.call(nmds, common))
    toggles <- list(
        list(nmdsOverlay=TRUE, nmdsShepard=FALSE),
        list(nmdsOverlay=FALSE, nmdsHull=TRUE),
        list(nmdsOverlay=FALSE, nmdsEllipse=TRUE),
        list(nmdsOverlay=FALSE, nmdsSpider=TRUE),
        list(
            nmdsOverlay=TRUE,
            nmdsHull=TRUE,
            nmdsEllipse=TRUE,
            nmdsSpider=TRUE))

    for (toggle in toggles) {
        layered <- suppressWarnings(do.call(
            nmds,
            c(common, toggle)))
        for (table in c("stress", "sites", "features", "envfit"))
            expect_identical(layered[[table]]$asDF, baseline[[table]]$asDF)
    }
})

test_that("ellipse helper matches vegan scaling for non-unit scale", {
    analysis <- run_nmds_private(
        nmds_state_data(n=18L),
        vars=paste0("feature_0", 1:4),
        seed=123,
        nmdsTrymax=5)
    private <- analysis$.__enclos_env__$private
    item <- list(
        cov=diag(c(4, 1)),
        center=c(2, 3),
        scale=2)
    actual <- private$.ellipsePoints(item, n=101L)
    expected <- getFromNamespace("veganCovEllipse", "vegan")(
        item$cov, item$center, item$scale, npoints=101L)

    expect_equal(
        apply(actual, 2L, range),
        apply(expected, 2L, range),
        tolerance=3e-3)
})

test_that("overlay-only plots use independent group and layer encodings", {
    analysis <- run_nmds_private(
        nmds_state_data(n=30L, groups=5L),
        vars=paste0("feature_0", 1:4),
        factor="group",
        nmdsOverlay=FALSE,
        nmdsHull=TRUE,
        nmdsEllipse=TRUE,
        nmdsSpider=TRUE,
        seed=123,
        nmdsTrymax=5)
    private <- analysis$.__enclos_env__$private
    overlays <- private$.state$overlays
    before <- unserialize(serialize(overlays, NULL))
    assignedStyles <- overlays$styles[overlays$styles$assigned, ]
    plot <- private$.buildNmdsPlot()
    geomClasses <- vapply(
        plot$layers,
        function(layer) class(layer$geom)[[1L]],
        character(1))
    shared <- .misoGroupAesthetics(assignedStyles$group)
    colourScale <- plot$scales$get_scales("colour")
    linetypeScale <- plot$scales$get_scales("linetype")
    linewidthScale <- plot$scales$get_scales("linewidth")

    expect_false(overlays$effective[["points"]])
    expect_true(all(overlays$effective[c("hull", "ellipse", "spider")]))
    expect_true(all(overlays$styles$assigned))
    expect_identical(nrow(assignedStyles), 5L)
    expect_identical(length(unique(assignedStyles$lineType)), 5L)
    expect_identical(length(unique(overlays$lineWidths)), 3L)
    expect_true("GeomSegment" %in% geomClasses)
    expect_gte(sum(geomClasses == "GeomPath"), 2L)
    expect_identical(
        stats::setNames(assignedStyles$colour, assignedStyles$group),
        shared$colour[assignedStyles$group])
    expect_identical(colourScale$name, "Group")
    expect_identical(linetypeScale$name, "Group")
    expect_identical(linewidthScale$name, "Layer")
    expect_identical(colourScale$guide, "legend")
    expect_identical(linetypeScale$guide, "legend")
    expect_identical(linewidthScale$guide, "legend")
    expect_s3_class(plot$guides$guides$colour, "GuideLegend")
    expect_s3_class(plot$guides$guides$linetype, "GuideLegend")
    expect_true(all(vapply(
        plot$layers[geomClasses %in% c("GeomSegment", "GeomPath")],
        function(layer) all(c("group", "layer") %in% names(layer$data)),
        logical(1))))

    expect_identical(private$.state$overlays, before)
})

test_that("overlay-only group colour and linetype pairs are unique to 64", {
    analysis <- run_nmds_private(
        nmds_state_data(),
        vars=paste0("feature_0", 1:4),
        seed=123,
        nmdsTrymax=5)
    groups <- sprintf("group-%02d", seq_len(64L))
    styles <- analysis$.__enclos_env__$private$.groupStyles(groups)
    pairs <- paste(styles$colour, styles$lineType)

    expect_length(unique(styles$lineType), 8L)
    expect_false(identical(pairs[[1L]], pairs[[25L]]))
    expect_length(unique(pairs), 64L)
})

test_that("nMDS plot is a non-empty read-only rendering of cached state", {
    analysis <- run_nmds_private(
        nmds_state_data(n=30L, groups=5L),
        vars=paste0("feature_0", 1:4),
        factor="group",
        nmdsOverlay=TRUE,
        nmdsSpecies=TRUE,
        nmdsEnv=c("temperature", "pH"),
        nmdsHull=TRUE,
        nmdsEllipse=TRUE,
        nmdsSpider=TRUE,
        seed=123,
        nmdsTrymax=5)
    private <- analysis$.__enclos_env__$private
    before <- list(
        sites=private$.state$sites,
        features=private$.state$features,
        vectorEndpoints=private$.state$vectorEndpoints,
        overlays=private$.state$overlays)
    plotBody <- paste(deparse(body(private$.buildNmdsPlot)), collapse="\n")
    plot <- private$.buildNmdsPlot()
    built <- ggplot2::ggplot_build(plot)
    geomClasses <- vapply(
        plot$layers,
        function(layer) class(layer$geom)[[1L]],
        character(1))
    path <- tempfile(fileext=".png")
    on.exit(unlink(path), add=TRUE)

    grDevices::png(path, width=900, height=700)
    private$.plotNmds(analysis$results$ordination)
    grDevices::dev.off()

    expect_gt(file.info(path)$size, 0)
    expect_identical(private$.state$sites, before$sites)
    expect_identical(private$.state$features, before$features)
    expect_identical(private$.state$vectorEndpoints, before$vectorEndpoints)
    expect_identical(private$.state$overlays, before$overlays)
    expect_null(plot$labels$title)
    expect_true("GeomPoint" %in% geomClasses)
    expect_true("GeomPath" %in% geomClasses)
    expect_gte(sum(geomClasses == "GeomSegment"), 3L)
    expect_gte(sum(geomClasses == "GeomText"), 2L)
    expect_identical(plot$scales$get_scales("colour")$name, "Group")
    expect_identical(plot$scales$get_scales("shape")$name, "Group")
    expect_identical(plot$scales$get_scales("linetype")$name, "Layer")
    expect_identical(plot$scales$get_scales("linewidth")$name, "Layer")
    expect_s3_class(plot$guides$guides$colour, "GuideLegend")
    expect_s3_class(plot$guides$guides$shape, "GuideLegend")
    expect_s3_class(plot$guides$guides$linetype, "GuideLegend")
    expect_true(length(built$plot$guides$guides) >= 2L)
    expect_false(grepl(
        "ordihull|ordiellipse|ordispider|vegan::scores",
        plotBody))
})

test_that("complete per-variable envfit preserves the current joint 2D calculation", {
    data <- nmds_state_data(n=24L)
    vars <- paste0("feature_0", 1:4)
    analysis <- run_nmds_private(
        data, vars=vars, nmdsEnv=c("temperature", "pH"),
        nmdsEnvPerm=99, seed=123, nmdsTrymax=20)

    set.seed(123)
    fit <- suppressWarnings(suppressMessages(vegan::metaMDS(
        data[vars], distance="bray", k=2, trymax=20, maxit=200,
        wascores=FALSE, autotransform=FALSE, trace=FALSE)))
    expected <- vegan::envfit(
        fit, data[c("temperature", "pH")], permutations=99L,
        choices=1:2)
    actual <- analysis$results$envfit$asDF

    expect_true(analysis$results$envfit$visible)
    expect_equal(
        actual$r2,
        unname(expected$vectors$r[actual$variable]),
        tolerance=0)
    expect_equal(
        actual$p,
        unname(expected$vectors$pvals[actual$variable]),
        tolerance=0)
})

test_that("environmental missingness and invalid variables fail locally", {
    data <- nmds_state_data(n=24L)
    data$temperature[c(1, 4, 8)] <- NA_real_
    data$pH[c(2, 7)] <- NA_real_
    data$constant <- 1
    data$all_missing <- NA_real_
    data$label <- rep(c("low", "high"), length.out=nrow(data))
    vars <- paste0("feature_0", 1:4)
    baseline <- run_nmds_private(data, vars=vars, seed=123)
    analysis <- run_nmds_private(
        data, vars=vars,
        nmdsEnv=c(
            "temperature", "pH", "constant", "all_missing", "label",
            "not_present"),
        seed=123)
    result <- analysis$results
    actual <- result$envfit$asDF
    samples <- setNames(actual$samples, actual$variable)

    expect_identical(actual$variable, c("temperature", "pH"))
    expect_identical(samples[["temperature"]], 21L)
    expect_identical(samples[["pH"]], 22L)
    expect_equal(result$sites$asDF, baseline$results$sites$asDF, tolerance=0)
    expect_equal(result$stress$asDF, baseline$results$stress$asDF, tolerance=0)
    expect_identical(nrow(result$sites$asDF), 24L)
    warning_text <- as.character(result$warnings$asString())
    expect_match(warning_text, "constant")
    expect_match(warning_text, "all_missing")
    expect_match(warning_text, "label")
    expect_match(warning_text, "not_present")
    expect_true(result$envfit$visible)
})

test_that("no usable environmental variable leaves only envfit hidden", {
    data <- nmds_state_data(n=18L)
    data$constant <- 1
    data$label <- rep(c("low", "high"), length.out=nrow(data))
    result <- run_nmds_private(
        data, vars=paste0("feature_0", 1:4),
        nmdsEnv=c("constant", "label", "not_present"), seed=123)$results

    expect_false(result$envfit$visible)
    expect_identical(nrow(result$envfit$asDF), 0L)
    expect_true(result$ordination$visible)
    expect_true(result$sites$visible)
    expect_true(result$warnings$visible)
})

test_that("environmental fits are option-order invariant and report evaluated permutations", {
    data <- nmds_state_data(n=24L)
    vars <- paste0("feature_0", 1:4)
    first <- run_nmds_private(
        data, vars=vars, nmdsEnv=c("temperature", "pH"),
        nmdsEnvPerm=99, seed=123)$results$envfit$asDF
    second <- run_nmds_private(
        data, vars=vars, nmdsEnv=c("pH", "temperature"),
        nmdsEnvPerm=99, seed=123)$results$envfit$asDF
    first <- first[match(sort(first$variable), first$variable), ]
    second <- second[match(sort(second$variable), second$variable), ]

    expect_equal(first$r2, second$r2, tolerance=0)
    expect_equal(first$p, second$p, tolerance=0)
    expect_equal(
        first[c("NMDS1", "NMDS2")],
        second[c("NMDS1", "NMDS2")],
        tolerance=0)
    expect_identical(first$permutations, rep(99L, 2L))

    small <- nmds_state_data(n=4L, groups=2L)
    exhaustive <- run_nmds_private(
        small, vars=vars, nmdsEnv="temperature",
        nmdsEnvPerm=999, seed=123)$results$envfit$asDF
    expect_identical(exhaustive$permutations, 23L)
})

test_that("legacy 3D environmental endpoints are authoritative in table and plot state", {
    data <- nmds_state_data(n=24L)
    analysis <- run_nmds_private(
        data, vars=paste0("feature_0", 1:4),
        nmdsEnv=c("temperature", "pH"), nmdsK=3, seed=123)
    private <- analysis$.__enclos_env__$private
    actual <- analysis$results$envfit$asDF
    endpoints <- private$.state$vectorEndpoints
    plotBody <- paste(deparse(body(private$.buildNmdsPlot)), collapse="\n")

    expect_true(analysis$results$envfit$getColumn("NMDS3")$visible)
    expect_true(all(is.finite(actual$NMDS3)))
    expect_identical(rownames(endpoints), actual$variable)
    expect_equal(
        unname(as.matrix(actual[c("NMDS1", "NMDS2", "NMDS3")])),
        unname(endpoints),
        tolerance=0)
    expect_match(plotBody, "plotData\\$vectorEndpoints")
    expect_false(grepl("vegan::envfit|vegan::scores", plotBody))
})


test_that("legacy 3D image language identifies the projection and all-dimension alternatives", {
    result <- run_nmds_private(
        nmds_state_data(n=24L),
        vars=paste0("feature_0", 1:4),
        nmdsK=3,
        nmdsSpecies=TRUE,
        nmdsEnv=c("temperature", "pH"),
        seed=123,
        nmdsTrymax=5)$results
    description <- nmds_squish(result$ordinationDescription$asString())

    expect_identical(
        result$ordination$title,
        "NMDS1-NMDS2 view of a three-dimensional nMDS solution")
    expect_lt(nchar(description), 850L)
})


test_that("Shepard visibility is prevalidated and its renderer is read-only", {
    shown <- run_nmds_private(
        nmds_state_data(n=18L),
        vars=paste0("feature_0", 1:4),
        nmdsShepard=TRUE,
        seed=123,
        nmdsTrymax=5)
    shownPrivate <- shown$.__enclos_env__$private
    description <- nmds_squish(shown$results$shepardDescription$asString())
    before <- list(
        fit=shownPrivate$.state$fit,
        data=shownPrivate$.state$shepardData,
        valid=shownPrivate$.state$shepardValid,
        warnings=shownPrivate$.state$warnings)
    plot <- shownPrivate$.buildShepardPlot()
    geomClasses <- vapply(
        plot$layers,
        function(layer) class(layer$geom)[[1L]],
        character(1))
    lineIndex <- which(geomClasses %in% c("GeomLine", "GeomPath"))[[1L]]
    expectedData <- data.frame(
        dissimilarity=as.numeric(shownPrivate$.state$fit$diss),
        ordinationDistance=as.numeric(shownPrivate$.state$fit$dist),
        monotonicFit=as.numeric(shownPrivate$.state$fit$dhat))
    expectedLine <- expectedData[order(
        expectedData$dissimilarity,
        expectedData$monotonicFit,
        method="radix"), , drop=FALSE]
    path <- tempfile(fileext=".png")
    on.exit(unlink(path), add=TRUE)
    grDevices::png(path, width=900, height=700)
    rendered <- shownPrivate$.plotShepard(shown$results$shepard)
    grDevices::dev.off()

    expect_true(shownPrivate$.state$shepardValid)
    expect_s3_class(plot, "ggplot")
    expect_identical(plot$theme, .misoPlotTheme())
    expect_true("GeomPoint" %in% geomClasses)
    expect_true(any(geomClasses %in% c("GeomLine", "GeomPath")))
    expect_identical(shownPrivate$.state$shepardData, expectedData)
    expect_identical(plot$data, expectedData)
    expect_identical(plot$layers[[lineIndex]]$data, expectedLine)
    expect_true(all(diff(plot$layers[[lineIndex]]$data$dissimilarity) >= 0))
    expect_false(grepl(
        "stressplot",
        paste(deparse(body(shownPrivate$.plotShepard)), collapse="\n"),
        fixed=TRUE))
    expect_true(isTRUE(rendered))
    expect_true(shown$results$shepard$visible)
    expect_true(shown$results$shepardDescription$visible)
    expect_true(shown$results$shepardPairs$visible)
    shownPairs <- shown$results$shepardPairs$asDF
    rownames(shownPairs) <- NULL
    expect_equal(shownPairs, expectedData, tolerance=0)
    expect_match(description, "ordination distances")
    expect_match(description, "preserve ranked dissimilarities")
    expect_lt(nchar(description), 600L)
    expect_gt(file.info(path)$size, 0)
    expect_identical(shownPrivate$.state$fit, before$fit)
    expect_identical(shownPrivate$.state$shepardData, before$data)
    expect_identical(shownPrivate$.state$shepardValid, before$valid)
    expect_identical(shownPrivate$.state$warnings, before$warnings)

    hidden <- run_nmds_private(
        nmds_state_data(n=18L),
        vars=paste0("feature_0", 1:4),
        nmdsShepard=FALSE,
        seed=123,
        nmdsTrymax=5)
    expect_false(hidden$results$shepard$visible)
    expect_false(hidden$results$shepardDescription$visible)
    expect_false(hidden$results$shepardPairs$visible)
    expect_equal(length(hidden$results$shepardPairs$rowKeys), 0L)
    expect_false(grepl(
        "observed dissimilarities",
        nmds_squish(hidden$results$shepardDescription$asString())))

    shownPrivate$.state$fit$stress <- NaN
    expect_false(shownPrivate$.validateShepard())
})

test_that("Shepard point and table cap is deterministic and RNG-safe", {
    analysis <- run_nmds_private(
        nmds_state_data(n=18L),
        vars=paste0("feature_0", 1:4),
        nmdsShepard=TRUE,
        seed=123,
        nmdsTrymax=5)
    private <- analysis$.__enclos_env__$private
    synthetic <- data.frame(
        dissimilarity=seq(0, 1, length.out=1500L),
        ordinationDistance=rev(seq(0, 1, length.out=1500L)),
        monotonicFit=seq(.1, .9, length.out=1500L))
    set.seed(923L)
    before <- .Random.seed
    first <- private$.capShepardData(synthetic)
    after <- .Random.seed
    second <- private$.capShepardData(synthetic)

    expect_identical(first, second)
    expect_identical(nrow(first), 1000L)
    expect_identical(after, before)
    expect_true(all(first$dissimilarity %in% synthetic$dissimilarity))
})

test_that("Shepard data rejects fit vectors that do not match site pairs", {
    analysis <- run_nmds_private(
        nmds_state_data(n=18L),
        vars=paste0("feature_0", 1:4),
        nmdsShepard=TRUE,
        seed=123,
        nmdsTrymax=5)
    private <- analysis$.__enclos_env__$private
    original <- private$.state$fit
    expectedLength <- as.integer(choose(nrow(private$.state$sites), 2L))

    expect_identical(nrow(private$.state$shepardData), expectedLength)
    for (field in c("diss", "dist", "dhat")) {
        private$.state$fit <- original
        private$.state$fit[[field]] <- original[[field]][-1L]
        expect_null(private$.shepardData(), info=field)
        expect_false(private$.validateShepard(), info=field)
    }
})





test_that("small and large fixtures match independent rotation-invariant nMDS fits", {
    expect_nmds_matches_independent(nmds_fixture("miso-small.csv"), k=2L)
    expect_nmds_matches_independent(nmds_fixture("miso-large.csv"), k=2L)
})

test_that("small and large fixture renderers keep features, arrows, and labels inside the plot frame", {
    expect_nmds_plot_inside_frame(nmds_fixture("miso-small.csv"))
    expect_nmds_plot_inside_frame(nmds_fixture("miso-large.csv"))
})

test_that("legacy 3D fixture matches an independent full configuration", {
    actual <- expect_nmds_matches_independent(
        nmds_fixture("miso-small.csv"), k=3L)
    runtimeTitle <- actual$results$ordination$title
    scenarios <- read.csv(
        test_path("..", "manual", "scenarios.csv"),
        stringsAsFactors=FALSE,
        check.names=FALSE)
    references <- read.csv(
        test_path("..", "manual", "reference-results.csv"),
        stringsAsFactors=FALSE,
        check.names=FALSE)
    scenarioOutputs <- strsplit(
        scenarios$result_slot[
            scenarios$scenario_id == "nmds-small-legacy-3d"],
        "|", fixed=TRUE)[[1L]]
    referenceTitle <- unique(references$result_slot[
        references$scenario_id == "nmds-small-legacy-3d" &
            references$metric == "configuration reference"])

    expect_true(all(is.finite(actual$results$sites$asDF$NMDS3)))
    expect_true(all(is.finite(actual$results$features$asDF$NMDS3)))
    expect_identical(
        runtimeTitle,
        "NMDS1-NMDS2 view of a three-dimensional nMDS solution")
    expect_identical(scenarioOutputs[[2L]], runtimeTitle)
    expect_identical(referenceTitle, runtimeTitle)
})

test_that("legacy Binary is exactly inert for the fixture baseline", {
    data <- nmds_fixture("miso-small.csv")
    vars <- grep("^feature_[0-9]+$", names(data), value=TRUE)
    options <- list(
        data=data,
        vars=vars,
        factor="group",
        nmdsEnv=c("temperature", "pH"),
        nmdsEnvPerm=99,
        nmdsSpecies=TRUE,
        seed=123,
        nmdsTrymax=20,
        nmdsMaxit=200)
    baseline <- do.call(run_nmds_private, options)
    binary <- do.call(run_nmds_private, c(options, list(distBinary=TRUE)))

    expect_equal(
        binary$.__enclos_env__$private$.state$fit$stress,
        baseline$.__enclos_env__$private$.state$fit$stress,
        tolerance=0)
    expect_equal(
        binary$.__enclos_env__$private$.state$sites,
        baseline$.__enclos_env__$private$.state$sites,
        tolerance=0)
    expect_equal(binary$results$stress$asDF, baseline$results$stress$asDF,
                 tolerance=0)
    expect_equal(binary$results$sites$asDF, baseline$results$sites$asDF,
                 tolerance=0)
    expect_equal(binary$results$features$asDF, baseline$results$features$asDF,
                 tolerance=0)
    expect_equal(binary$results$envfit$asDF, baseline$results$envfit$asDF,
                 tolerance=0)
})

test_that("nMDS migration scenarios use current result names", {
    scenarios <- read.csv(
        test_path("..", "manual", "scenarios.csv"),
        stringsAsFactors=FALSE,
        check.names=FALSE)
    scenarios <- scenarios[scenarios$analysis == "nMDS", ]
    expectedOutputs <- paste(
        "Result Tables",
        "Two-dimensional nMDS ordination",
        "Stress and convergence diagnostics",
        "Shepard Diagram",
        "Environmental Fit",
        "Interpretation",
        "Site Scores",
        "Table Notes",
        sep="|")

    expect_identical(
        scenarios$result_slot[scenarios$scenario_id == "nmds-small-baseline"],
        expectedOutputs)
    expect_identical(
        scenarios$result_slot[scenarios$scenario_id == "nmds-large-baseline"],
        expectedOutputs)
    expect_true(all(c(
        "nmds-small-features-only", "nmds-small-binary-noop",
        "nmds-small-legacy-3d", "nmds-small-standardize-bray-recovery",
        "nmds-small-standardize-euclidean",
        "nmds-small-missing-optional-metadata") %in% scenarios$scenario_id))
})

test_that("nMDS ordination renders from serialized Image state alone", {
    analysis <- run_nmds_private(
        nmds_state_data(n=24L),
        vars=paste0("feature_0", 1:4),
        factor="group",
        nmdsOverlay=TRUE,
        nmdsHull=TRUE,
        nmdsSpider=TRUE,
        seed=123,
        nmdsTrymax=5)
    private <- analysis$.__enclos_env__$private
    image <- analysis$results$ordination
    state <- image$state
    expect_false(is.null(state))
    expect_identical(nrow(state$sites), 24L)
    expect_false(is.null(state$overlays))

    restored <- unserialize(serialize(state, NULL))
    private$.state$sites <- NULL
    private$.state$overlays <- NULL
    private$.state$features <- NULL
    private$.state$vectorEndpoints <- NULL
    image$setState(restored)

    file <- tempfile(fileext=".png")
    on.exit({
        if (grDevices::dev.cur() > 1L)
            grDevices::dev.off()
        unlink(file)
    }, add=TRUE)
    grDevices::png(file, width=580, height=450)
    private$.plotNmds(image)
    grDevices::dev.off()
    expect_gt(file.info(file)$size, 1000)
})

test_that("Shepard Diagram renders from serialized Image state alone", {
    analysis <- run_nmds_private(
        nmds_state_data(n=18L),
        vars=paste0("feature_0", 1:4),
        nmdsShepard=TRUE,
        seed=123,
        nmdsTrymax=5)
    private <- analysis$.__enclos_env__$private
    image <- analysis$results$shepard
    state <- image$state
    expect_false(is.null(state))
    expect_s3_class(state$display, "data.frame")
    expect_s3_class(state$all, "data.frame")

    restored <- unserialize(serialize(state, NULL))
    private$.state$shepardData <- NULL
    private$.state$shepardDisplayData <- NULL
    image$setState(restored)

    file <- tempfile(fileext=".png")
    on.exit({
        if (grDevices::dev.cur() > 1L)
            grDevices::dev.off()
        unlink(file)
    }, add=TRUE)
    grDevices::png(file, width=580, height=450)
    private$.plotShepard(image)
    grDevices::dev.off()
    expect_gt(file.info(file)$size, 1000)
})


test_that("structural environmental inputs rebuild fit rows while display toggles do not", {
    index <- seq_len(9L)
    data <- data.frame(
        feature_01=1 + index %% 4,
        feature_02=2 + (index * 3) %% 5,
        feature_03=1 + (index * 2) %% 3,
        feature_04=4 + (index * 5) %% 7,
        env1=index + sin(index),
        env2=10 - index + cos(index),
        group=factor(rep(c("A", "B", "C"), each=3L)))
    options <- nmdsOptions$new(
        vars=paste0("feature_0", 1:4),
        nmdsEnv=c("env1", "env2"),
        nmdsEnvPerm=19,
        nmdsTrymax=2,
        seed=123)
    analysis <- nmdsClass$new(options=options, data=data)
    suppressWarnings(suppressMessages(analysis$run()))
    keys <- analysis$results$envfit$rowKeys

    envOption <- options$option("nmdsEnv")
    envOption$.__enclos_env__$private$.value <- "env1"
    suppressWarnings(suppressMessages(analysis$run()))
    expect_false(identical(analysis$results$envfit$rowKeys, keys))
    expect_setequal(analysis$results$envfit$asDF$variable, "env1")
})

test_that("nMDS reports effective choices in surviving table notes", {
    analysis <- run_nmds_private(nmds_state_data(),
        vars=paste0("feature_0", 1:4), factor="group", nmdsTrymax=5, seed=123)
    expect_true(analysis$results$sites$visible)
    expect_match(miso_table_note(analysis$results$sites, "method"), "Transformation")
    expect_match(miso_table_note(analysis$results$stress, "method"), "Dimensions")
})
