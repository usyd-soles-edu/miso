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
    before <- list(
        features=private$.state$features,
        vectorEndpoints=private$.state$vectorEndpoints)
    renderCalls <- new.env(parent=emptyenv())
    renderCalls$arrows <- list()
    renderCalls$text <- list()
    testthat::local_mocked_bindings(
        arrows=function(...) {
            renderCalls$arrows[[length(renderCalls$arrows) + 1L]] <- list(...)
            invisible(NULL)
        },
        text=function(...) {
            renderCalls$text[[length(renderCalls$text) + 1L]] <- list(...)
            invisible(NULL)
        },
        .package="graphics")
    path <- tempfile(fileext=".png")
    on.exit(unlink(path), add=TRUE)

    grDevices::png(path, width=900, height=700)
    rendered <- private$.plotNmds(NULL)
    usr <- graphics::par("usr")
    rawVectors <- private$.state$vectorEndpoints[, 1:2, drop=FALSE]
    expectedMultiplier <- vegan::ordiArrowMul(
        rawVectors, fill=0.75 / 1.12)
    grDevices::dev.off()
    expect_true(isTRUE(rendered))

    inside <- function(values, limits)
        all(values >= min(limits) & values <= max(limits))
    features <- private$.state$features[, 1:2, drop=FALSE]
    featureRows <- is.finite(features[, 1L]) & is.finite(features[, 2L])
    expect_true(inside(features[featureRows, 1L], usr[1:2]))
    expect_true(inside(features[featureRows, 2L], usr[3:4]))
    expect_length(renderCalls$arrows, 1L)
    arrowCall <- renderCalls$arrows[[1L]]
    renderedArrows <- cbind(x=arrowCall[[3L]], y=arrowCall[[4L]])
    expect_equal(
        unname(renderedArrows),
        unname(rawVectors * expectedMultiplier),
        tolerance=0)
    expect_true(inside(renderedArrows[, 1L], usr[1:2]))
    expect_true(inside(renderedArrows[, 2L], usr[3:4]))
    vectorText <- Filter(
        function(call) identical(call$col, "darkred"),
        renderCalls$text)
    expect_length(vectorText, 1L)
    renderedLabels <- cbind(
        x=vectorText[[1L]][[1L]],
        y=vectorText[[1L]][[2L]])
    expect_equal(
        unname(renderedLabels),
        unname(renderedArrows[
            private$.state$vectorLabelSelection$shown, , drop=FALSE] * 1.12),
        tolerance=0)
    expect_true(inside(renderedLabels[, 1L], usr[1:2]))
    expect_true(inside(renderedLabels[, 2L], usr[3:4]))
    expect_identical(private$.state$features, before$features)
    expect_identical(private$.state$vectorEndpoints, before$vectorEndpoints)
    expect_gt(file.info(path)$size, 0)
    invisible(analysis)
}

expect_nmds_visibility <- function(result, visible, hidden) {
    for (name in visible)
        expect_true(result[[name]]$visible, info=paste(name, "should be visible"))
    for (name in hidden)
        expect_false(result[[name]]$visible, info=paste(name, "should be hidden"))
}

expect_only_nmds_guidance <- function(result) {
    expect_nmds_visibility(
        result,
        "guidance",
        c(
            "summary", "warnings", "ordination", "ordinationDescription",
            "stress", "shepard", "shepardDescription", "envfit", "note",
            "sites", "features", "settings"))
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
    analysis <- yaml::read_yaml(tofu_fixture_path("jamovi", "nmds.a.yaml"))
    options <- analysis$options
    by_name <- setNames(options, vapply(options, `[[`, character(1), "name"))
    names_in_order <- vapply(options, `[[`, character(1), "name")

    expect_identical(by_name$vars$title, "Required: Feature variables")
    expect_match(by_name$factor$title, "optional", ignore.case=TRUE)
    expect_match(by_name$nmdsEnv$title, "optional", ignore.case=TRUE)
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
    ui_path <- tofu_fixture_path("jamovi", "nmds.u.yaml")
    ui <- yaml::read_yaml(ui_path)
    expect_false(find_nmds_yaml_node(ui, "analysisChoices")$collapsed)
    expect_false(find_nmds_yaml_node(ui, "outputChoices")$collapsed)
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

    source_path <- tofu_fixture_path("jamovi", "js", "nmds.js")
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
        tofu_fixture_path("jamovi", "nmds.r.yaml"))$items
    by_name <- setNames(items, vapply(items, `[[`, character(1), "name"))

    expect_true(all(vapply(items, function(x) identical(x$visible, FALSE), logical(1))))
    expected_clear_with <- c(
        "vars", "factor", "transform", "distance", "distBinary", "seed",
        "nmdsK", "nmdsTrymax", "nmdsMaxit", "nmdsShepard", "nmdsOverlay",
        "nmdsEnv", "nmdsSpecies", "nmdsHull", "nmdsEllipse", "nmdsSpider",
        "nmdsEnvPerm")
    expect_true(all(vapply(
        items, function(x) identical(x$clearWith, expected_clear_with), logical(1))))
    expect_identical(
        vapply(items, `[[`, character(1), "name"),
        c(
            "guidance", "summary", "warnings", "ordination",
            "ordinationDescription", "stress", "shepard", "shepardDescription",
            "envfit", "note", "sites", "features", "settings"))
    expect_identical(by_name$guidance$type, "Html")
    expect_identical(by_name$ordinationDescription$type, "Html")
    expect_identical(by_name$shepardDescription$type, "Html")
    expect_identical(by_name$note$type, "Html")
    expect_identical(
        vapply(by_name$summary$columns, `[[`, character(1), "name"),
        c("item", "value"))
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

    none <- run_nmds_private(data, vars=character())$results
    expect_match(
        nmds_squish(none$guidance$asString()),
        "at least two numeric Feature variables")
    expect_only_nmds_guidance(none)

    one <- run_nmds_private(data, vars="feature_01")$results
    expect_match(
        nmds_squish(one$guidance$asString()),
        "at least two usable numeric Feature variables")
    expect_only_nmds_guidance(one)
})

test_that("nMDS report guidance stays compact", {
    result <- run_nmds_private(
        nmds_state_data(),
        vars=paste0("feature_0", 1:4),
        factor="group",
        nmdsEnv="temperature",
        nmdsSpecies=TRUE,
        seed=123,
        nmdsTrymax=5)$results
    description <- as.character(result$ordinationDescription$asString())
    interpretation <- as.character(result$note$asString())
    report_text <- paste(description, interpretation)

    expect_lt(nchar(description), 700L)
    expect_lt(nchar(interpretation), 700L)
    expect_lte(nrow(result$settings$asDF), 15L)
    expect_false(grepl(
        paste(
            "Non-visual and exact-value alternatives|context-dependent guides|",
            "global optimum|weighted-average positions|requested|effective"),
        report_text,
        ignore.case=TRUE))
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

test_that("legacy Binary is inert and disclosed only when requested", {
    data <- nmds_state_data()
    args <- list(
        data=data,
        vars=paste0("feature_0", 1:4),
        seed=123,
        nmdsTrymax=20)
    normal <- suppressWarnings(do.call(nmds, args))
    binary <- suppressWarnings(do.call(
        nmds, c(args, list(distBinary=TRUE))))

    expect_equal(
        binary$sites$asDF[c("NMDS1", "NMDS2")],
        normal$sites$asDF[c("NMDS1", "NMDS2")],
        tolerance=0)
    expect_equal(binary$stress$asDF, normal$stress$asDF)
    expect_match(
        as.character(binary$warnings$asString()),
        "legacy Binary")
    expect_false(grepl(
        "legacy Binary",
        as.character(normal$warnings$asString())))
    expect_false(grepl(
        "legacy Binary",
        as.character(normal$settings$asString())))
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
    expect_match(
        as.character(result$ordinationDescription$asString()),
        "projection")
    expect_match(
        as.character(result$ordinationDescription$asString()),
        "NMDS3")
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

test_that("unavailable fitted-object diagnostics use guarded fallbacks", {
    analysis <- run_nmds_private(
        nmds_state_data(),
        vars=paste0("feature_0", 1:4),
        seed=123,
        nmdsTrymax=5)
    private <- analysis$.__enclos_env__$private
    fit <- private$.state$fit
    fit$stress <- NaN
    fit$converged <- NA_real_
    fit$tries <- Inf
    fit$bestry <- NA_real_
    fit$iters <- NULL
    fit$engine <- NULL
    fit$icause <- Inf

    guarded <- c(
        private$.fitValue(fit, "converged"),
        private$.searchStatus(fit),
        private$.fitValue(fit, "tries"),
        private$.bestStart(fit),
        private$.fitValue(fit, "iters"),
        private$.fitValue(fit, "engine"),
        private$.stressGuide(fit, private$.state$prep, 2L))
    expect_false(any(grepl("NaN|Inf", guarded)))
    expect_true(all(grepl("Unavailable", guarded)))
    expect_identical(
        private$.stoppingReason(fit),
        "Not reported by this engine")

    private$.state$fit <- fit
    private$.populateCoreResults()
    visible <- paste(
        as.character(analysis$results$note$asString()),
        analysis$results$stress$asDF$value,
        collapse=" ")
    expect_false(grepl("NaN|Inf", visible))
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

test_that("maximum random starts is effective and actual starts are reported", {
    data <- nmds_state_data(n=24L)
    for (requested in c(1L, 5L, 20L, 25L)) {
        analysis <- run_nmds_private(
            data,
            vars=paste0("feature_0", 1:4),
            seed=123,
            nmdsTrymax=requested)
        result <- analysis$results
        values <- setNames(result$stress$asDF$value, result$stress$asDF$item)
        settings <- setNames(
            result$settings$asDF$value,
            result$settings$asDF$setting)

        expect_identical(
            analysis$.__enclos_env__$private$.state$fitArguments$trymax,
            requested)
        expect_lte(
            as.integer(values[["Random starts tried"]]),
            requested)
        expect_identical(
            settings[["Random starts"]],
            sprintf(
                "%s tried; %d maximum",
                values[["Random starts tried"]],
                requested))
    }
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

test_that("all group presentation toggles preserve exact core coordinates", {
    data <- nmds_state_data(n=30L, groups=5L)
    common <- list(
        data=data,
        vars=paste0("feature_0", 1:4),
        seed=123,
        nmdsTrymax=5)
    baseline <- suppressWarnings(do.call(nmds, common))
    toggles <- list(
        list(nmdsOverlay=TRUE),
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
            c(common, list(factor="group"), toggle)))
        expect_equal(
            layered$sites$asDF[c("NMDS1", "NMDS2")],
            baseline$sites$asDF[c("NMDS1", "NMDS2")],
            tolerance=0)
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
    renderCalls <- new.env(parent=emptyenv())
    renderCalls$lines <- list()
    renderCalls$segments <- list()
    renderCalls$legends <- list()
    testthat::local_mocked_bindings(
        lines=function(...) {
            renderCalls$lines[[length(renderCalls$lines) + 1L]] <- list(...)
            invisible(NULL)
        },
        segments=function(...) {
            renderCalls$segments[[length(renderCalls$segments) + 1L]] <- list(...)
            invisible(NULL)
        },
        legend=function(...) {
            renderCalls$legends[[length(renderCalls$legends) + 1L]] <- list(...)
            invisible(NULL)
        },
        .package="graphics")
    path <- tempfile(fileext=".png")
    on.exit(unlink(path), add=TRUE)

    grDevices::png(path, width=900, height=700)
    private$.plotNmds(NULL)
    grDevices::dev.off()

    expect_false(overlays$effective[["points"]])
    expect_true(all(overlays$effective[c("hull", "ellipse", "spider")]))
    expect_true(all(overlays$styles$assigned))
    expect_identical(nrow(assignedStyles), 5L)
    expect_identical(length(unique(assignedStyles$lineType)), 5L)
    expect_identical(length(unique(overlays$lineWidths)), 3L)
    expect_identical(length(renderCalls$segments), 5L)
    expect_identical(length(renderCalls$lines), 10L)

    drawn <- c(renderCalls$segments, renderCalls$lines)
    expectedGroupLineTypes <- setNames(
        assignedStyles$lineType, assignedStyles$colour)
    expect_identical(
        vapply(drawn, function(call) as.integer(call$lty), integer(1)),
        as.integer(expectedGroupLineTypes[vapply(
            drawn, function(call) as.character(call$col), character(1))]))
    expect_identical(
        unique(vapply(
            renderCalls$segments,
            function(call) as.numeric(call$lwd), numeric(1))),
        unname(overlays$lineWidths[["spider"]]))
    expect_identical(
        unique(vapply(
            renderCalls$lines[seq_len(5L)],
            function(call) as.numeric(call$lwd), numeric(1))),
        unname(overlays$lineWidths[["hull"]]))
    expect_identical(
        unique(vapply(
            renderCalls$lines[6:10],
            function(call) as.numeric(call$lwd), numeric(1))),
        unname(overlays$lineWidths[["ellipse"]]))

    legendPosition <- function(call)
        if (is.null(call$x)) call[[1L]] else call$x
    positions <- vapply(
        renderCalls$legends, legendPosition, character(1))
    groupLegend <- renderCalls$legends[[match("topright", positions)]]
    layerLegend <- renderCalls$legends[[match("bottomright", positions)]]
    expect_identical(groupLegend$legend, assignedStyles$group)
    expect_identical(groupLegend$col, assignedStyles$colour)
    expect_identical(groupLegend$lty, assignedStyles$lineType)
    expect_identical(groupLegend$lwd, max(overlays$lineWidths))
    expect_null(groupLegend$pch)
    expect_identical(
        unname(layerLegend$lwd),
        unname(overlays$lineWidths[c("hull", "ellipse", "spider")]))
    expect_identical(as.integer(layerLegend$lty), rep(1L, 3L))

    expect_identical(private$.state$overlays, before)
    expect_gt(file.info(path)$size, 0)
})

test_that("nMDS plot is a non-empty read-only rendering of cached state", {
    analysis <- run_nmds_private(
        nmds_state_data(n=30L, groups=5L),
        vars=paste0("feature_0", 1:4),
        factor="group",
        nmdsOverlay=TRUE,
        nmdsSpecies=TRUE,
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
    plotBody <- paste(deparse(body(private$.plotNmds)), collapse="\n")
    path <- tempfile(fileext=".png")
    on.exit(unlink(path), add=TRUE)

    grDevices::png(path, width=900, height=700)
    private$.plotNmds(NULL)
    grDevices::dev.off()

    expect_gt(file.info(path)$size, 0)
    expect_identical(private$.state$sites, before$sites)
    expect_identical(private$.state$features, before$features)
    expect_identical(private$.state$vectorEndpoints, before$vectorEndpoints)
    expect_identical(private$.state$overlays, before$overlays)
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
    plotBody <- paste(deparse(body(private$.plotNmds)), collapse="\n")

    expect_true(analysis$results$envfit$getColumn("NMDS3")$visible)
    expect_true(all(is.finite(actual$NMDS3)))
    expect_identical(rownames(endpoints), actual$variable)
    expect_equal(
        unname(as.matrix(actual[c("NMDS1", "NMDS2", "NMDS3")])),
        unname(endpoints),
        tolerance=0)
    expect_match(plotBody, "\\.state\\$vectorEndpoints")
    expect_false(grepl("vegan::envfit|vegan::scores", plotBody))
})

test_that("Data Summary and semantic plot descriptions report the complete 2D contract", {
    data <- nmds_state_data(n=24L)
    data$feature_01[[2L]] <- NA_real_
    data[4L, paste0("feature_0", 1:4)] <- 0
    data$feature_all_zero <- 0
    data$group[[3L]] <- NA
    vars <- c(paste0("feature_0", 1:4), "feature_all_zero")
    result <- run_nmds_private(
        data,
        vars=vars,
        factor="group",
        nmdsEnv=c("temperature", "pH"),
        nmdsSpecies=TRUE,
        nmdsHull=TRUE,
        nmdsEllipse=TRUE,
        nmdsSpider=TRUE,
        seed=123,
        nmdsTrymax=5)$results
    summary <- setNames(result$summary$asDF$value, result$summary$asDF$item)
    description <- nmds_squish(result$ordinationDescription$asString())

    expect_identical(
        names(summary),
        c(
            "Core samples used", "Feature variables used",
            "Rows excluded: missing feature values",
            "Rows excluded: all-zero feature values",
            "All-zero feature variables excluded", "Transformation",
            "Dissimilarity index", "Effective dimensions",
            "Grouping assignment", "Environmental variables requested",
            "Seed"))
    expect_identical(summary[["Core samples used"]], "22")
    expect_identical(summary[["Feature variables used"]], "4")
    expect_identical(
        summary[["Rows excluded: missing feature values"]], "1")
    expect_identical(
        summary[["Rows excluded: all-zero feature values"]], "1")
    expect_identical(
        summary[["All-zero feature variables excluded"]], "1")
    expect_identical(summary[["Transformation"]], "None")
    expect_identical(summary[["Dissimilarity index"]], "Bray-Curtis")
    expect_identical(summary[["Effective dimensions"]], "2")
    expect_identical(
        summary[["Grouping assignment"]],
        "group (21 assigned; 1 unassigned)")
    expect_identical(
        summary[["Environmental variables requested"]],
        "temperature, pH (2 requested)")
    expect_identical(summary[["Seed"]], "Fixed (123)")

    expect_identical(result$ordination$title, "Two-dimensional nMDS ordination")
    expect_match(description, "22 sites")
    expect_match(description, "stress")
    expect_match(description, "Bray-Curtis")
    expect_match(description, "after None")
    expect_match(description, "Display:")
    expect_match(description, "group colour and shape")
    expect_match(description, "group hull")
    expect_match(description, "1-SD dispersion ellipse")
    expect_match(description, "group spider")
    expect_match(description, "2 environmental vector\\(s\\)")
    expect_false(grepl("uniformly rescaled", description, fixed=TRUE))
    expect_lt(nchar(description), 700L)
    expect_match(description, "overflow-wrap: anywhere")
    expect_match(description, "word-break: normal")
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
    expect_match(description, "projection")
    expect_match(description, "NMDS3 is not shown")
    expect_match(description, "all three coordinates")
    expect_match(description, "score and fit tables")
    expect_lt(nchar(description), 700L)
})

test_that("bounded HTML escapes plain text and Interpretation stays compact", {
    analysis <- run_nmds_private(
        nmds_state_data(n=5L),
        vars=paste0("feature_0", 1:4),
        distance="euclidean",
        factor="group",
        nmdsHull=TRUE,
        nmdsEllipse=TRUE,
        nmdsSpider=TRUE,
        nmdsEnv="temperature",
        seed=123,
        nmdsTrymax=5)
    private <- analysis$.__enclos_env__$private
    escaped <- private$.htmlBlock(c("<script>alert('x')</script>", "A & B"))
    interpretation <- nmds_squish(analysis$results$note$asString())

    expect_match(escaped, "max-width: 44em")
    expect_match(escaped, "overflow-wrap: anywhere")
    expect_match(escaped, "word-break: normal")
    expect_false(grepl("<script>", escaped, fixed=TRUE))
    expect_match(escaped, "&lt;script&gt;")
    expect_match(escaped, "A &amp; B")
    expect_match(interpretation, "overflow-wrap: anywhere")
    expect_match(interpretation, "Closer sites")
    expect_match(interpretation, "Axis direction")
    expect_match(interpretation, "Stress =")
    expect_match(interpretation, "stress may be uninformative")
    expect_match(interpretation, "Group displays are descriptive")
    expect_match(interpretation, "not confidence regions")
    expect_match(interpretation, "not group differences")
    expect_match(interpretation, "p-values are unadjusted")
    expect_lt(nchar(interpretation), 700L)
    expect_match(interpretation, "white-space: normal", fixed=TRUE)
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
        valid=shownPrivate$.state$shepardValid,
        warnings=shownPrivate$.state$warnings)
    path <- tempfile(fileext=".png")
    on.exit(unlink(path), add=TRUE)
    grDevices::png(path, width=900, height=700)
    rendered <- shownPrivate$.plotShepard(NULL)
    grDevices::dev.off()

    expect_true(shownPrivate$.state$shepardValid)
    expect_true(isTRUE(rendered))
    expect_true(shown$results$shepard$visible)
    expect_true(shown$results$shepardDescription$visible)
    expect_match(description, "observed dissimilarities", ignore.case=TRUE)
    expect_match(description, "ordination distances")
    expect_match(description, "stress")
    expect_lt(nchar(description), 350L)
    expect_gt(file.info(path)$size, 0)
    expect_identical(shownPrivate$.state$fit, before$fit)
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
    expect_false(grepl(
        "observed dissimilarities",
        nmds_squish(hidden$results$shepardDescription$asString())))

    shownPrivate$.state$fit$stress <- NaN
    expect_false(shownPrivate$.validateShepard())
})

test_that("Analysis settings report effective choices compactly", {
    data <- nmds_state_data(n=24L)
    data$temperature[c(1L, 4L)] <- NA_real_
    data$group[[2L]] <- NA
    analysis <- run_nmds_private(
        data,
        vars=paste0("feature_0", 1:4),
        factor="group",
        nmdsOverlay=TRUE,
        nmdsHull=TRUE,
        nmdsEllipse=TRUE,
        nmdsSpider=TRUE,
        nmdsEnv=c("temperature", "pH"),
        nmdsEnvPerm=99,
        nmdsSpecies=TRUE,
        nmdsShepard=TRUE,
        seed=123,
        nmdsTrymax=5,
        nmdsMaxit=80)
    settings <- setNames(
        analysis$results$settings$asDF$value,
        analysis$results$settings$asDF$setting)
    required <- c(
        "Transformation", "Dissimilarity", "Dimensions", "Samples used",
        "Features used", "Grouping", "Group display",
        "Environmental variables", "Environmental permutations",
        "Feature Scores", "Shepard diagram", "Seed", "Random starts",
        "Maximum iterations")

    expect_identical(names(settings), required)
    expect_false("Legacy Binary request" %in% names(settings))
    expect_identical(settings[["Dimensions"]], "2")
    expect_identical(settings[["Grouping"]], "group (23 assigned; 1 unassigned)")
    expect_match(settings[["Group display"]], "1-SD dispersion ellipse")
    expect_identical(
        settings[["Environmental variables"]], "2 fitted of 2 requested")
    expect_identical(settings[["Environmental permutations"]], "99")
    expect_identical(settings[["Feature Scores"]], "Shown")
    expect_identical(settings[["Shepard diagram"]], "Shown")
    expect_identical(settings[["Seed"]], "Fixed (123)")
    expect_match(settings[["Random starts"]], "5 maximum")
    expect_identical(settings[["Maximum iterations"]], "80")

    legacy <- run_nmds_private(
        nmds_state_data(n=24L),
        vars=paste0("feature_0", 1:4),
        nmdsK=3,
        distBinary=TRUE,
        seed=123,
        nmdsTrymax=5)$results$settings$asDF
    legacy <- setNames(legacy$value, legacy$setting)
    expect_match(legacy[["Legacy Binary request"]], "Ignored")
    expect_match(legacy[["Dimensions"]], "3 \\(legacy")
    expect_match(legacy[["Dimensions"]], "NMDS1-NMDS2")
})

test_that("valid-invalid-valid reruns clear all stale result state", {
    data <- nmds_state_data(n=24L)
    options <- nmdsOptions$new(
        vars=paste0("feature_0", 1:4),
        factor="group",
        nmdsEnv=c("temperature", "pH"),
        nmdsSpecies=TRUE,
        nmdsHull=TRUE,
        seed=123,
        nmdsTrymax=5)
    analysis <- nmdsClass$new(options=options, data=data)
    private <- analysis$.__enclos_env__$private
    private$.run()
    expect_true(analysis$results$ordination$visible)
    expect_gt(nrow(analysis$results$sites$asDF), 0L)
    expect_gt(nrow(analysis$results$envfit$asDF), 0L)

    varsOption <- options$option("vars")
    varsOption$value <- "feature_01"
    private$.run()
    expect_only_nmds_guidance(analysis$results)
    expect_identical(nrow(analysis$results$summary$asDF), 0L)
    expect_identical(nrow(analysis$results$stress$asDF), 0L)
    expect_identical(nrow(analysis$results$envfit$asDF), 0L)
    expect_identical(nrow(analysis$results$sites$asDF), 0L)
    expect_identical(nrow(analysis$results$features$asDF), 0L)
    expect_identical(nrow(analysis$results$settings$asDF), 0L)
    expect_false(grepl(
        "retained sites",
        nmds_squish(analysis$results$ordinationDescription$asString())))
    expect_false(grepl(
        "observed dissimilarities",
        nmds_squish(analysis$results$shepardDescription$asString())))
    expect_false(grepl(
        "axis directions",
        nmds_squish(analysis$results$note$asString())))
    expect_null(private$.state$fit)
    expect_null(private$.state$sites)
    expect_null(private$.state$vectorEndpoints)
    expect_identical(private$.state$overlays, list())
    expect_false(private$.state$shepardValid)

    varsOption$value <- paste0("feature_0", 1:4)
    private$.run()
    expect_false(analysis$results$guidance$visible)
    expect_true(analysis$results$ordination$visible)
    expect_true(analysis$results$shepard$visible)
    expect_true(analysis$results$envfit$visible)
    expect_true(analysis$results$features$visible)
    expect_gt(nrow(analysis$results$summary$asDF), 0L)
    expect_gt(nrow(analysis$results$sites$asDF), 0L)
    expect_gt(nrow(analysis$results$settings$asDF), 0L)
})

test_that("deterministic label fallback retains all feature and vector table rows", {
    index <- seq_len(30L)
    data <- data.frame(group=factor(rep(LETTERS[1:3], length.out=30L)))
    featureNames <- sprintf(
        "feature_with_a_deliberately_long_accessible_name_%02d", 1:16)
    envNames <- sprintf(
        "environmental_variable_with_a_long_name_%02d", 1:14)
    for (j in seq_along(featureNames))
        data[[featureNames[[j]]]] <-
            1 + ((index * (j + 1L) + j^2L) %% (7L + (j %% 5L)))
    for (j in seq_along(envNames))
        data[[envNames[[j]]]] <-
            index * (j + 1) + sin(index * (j + 0.5))

    first <- run_nmds_private(
        data,
        vars=featureNames,
        nmdsSpecies=TRUE,
        nmdsEnv=envNames,
        nmdsEnvPerm=19,
        seed=123,
        nmdsTrymax=5)
    second <- run_nmds_private(
        data,
        vars=featureNames,
        nmdsSpecies=TRUE,
        nmdsEnv=envNames,
        nmdsEnvPerm=19,
        seed=123,
        nmdsTrymax=5)
    firstPrivate <- first$.__enclos_env__$private
    secondPrivate <- second$.__enclos_env__$private
    description <- nmds_squish(first$results$ordinationDescription$asString())
    settings <- setNames(
        first$results$settings$asDF$value,
        first$results$settings$asDF$setting)

    expect_identical(nrow(first$results$features$asDF), length(featureNames))
    expect_identical(nrow(first$results$envfit$asDF), length(envNames))
    expect_lte(length(firstPrivate$.state$featureLabelsShown), 12L)
    expect_lte(length(firstPrivate$.state$vectorLabelsShown), 12L)
    expect_gt(length(firstPrivate$.state$featureLabelsOmitted), 0L)
    expect_gt(length(firstPrivate$.state$vectorLabelsOmitted), 0L)
    expect_identical(
        firstPrivate$.state$featureLabelSelection,
        secondPrivate$.state$featureLabelSelection)
    expect_identical(
        firstPrivate$.state$vectorLabelSelection,
        secondPrivate$.state$vectorLabelSelection)
    expect_match(
        description,
        sprintf("Feature labels: %d of %d shown",
            length(firstPrivate$.state$featureLabelsShown),
            length(featureNames)))
    expect_match(
        description,
        sprintf("Environmental vector labels: %d of %d shown",
            length(firstPrivate$.state$vectorLabelsShown),
            length(envNames)))
    expect_match(description, "all values are in Feature Scores")
    expect_match(description, "all values are in Environmental Fit")
    expect_false(grepl(
        firstPrivate$.state$featureLabelsOmitted[[1L]],
        description,
        fixed=TRUE))
    expect_false(grepl(
        firstPrivate$.state$vectorLabelsOmitted[[1L]],
        description,
        fixed=TRUE))
    expect_false("Feature labels shown in image" %in% names(settings))
    expect_false("Environmental vector labels shown in image" %in% names(settings))

    directPoints <- cbind(
        x=seq(-1, 1, length.out=16),
        y=rep(c(-0.01, 0.01), 8L))
    firstSelection <- firstPrivate$.selectPlotLabels(
        directPoints, featureNames)
    secondSelection <- firstPrivate$.selectPlotLabels(
        directPoints, featureNames)
    expect_identical(firstSelection, secondSelection)
    expect_identical(
        sort(c(firstSelection$shown, firstSelection$omitted)),
        seq_along(featureNames))

    before <- list(
        featureSelection=firstPrivate$.state$featureLabelSelection,
        vectorSelection=firstPrivate$.state$vectorLabelSelection,
        features=firstPrivate$.state$features,
        vectors=firstPrivate$.state$vectorEndpoints)
    path <- tempfile(fileext=".png")
    on.exit(unlink(path), add=TRUE)
    grDevices::png(path, width=900, height=700)
    firstPrivate$.plotNmds(NULL)
    grDevices::dev.off()
    expect_gt(file.info(path)$size, 0)
    expect_identical(
        firstPrivate$.state$featureLabelSelection,
        before$featureSelection)
    expect_identical(
        firstPrivate$.state$vectorLabelSelection,
        before$vectorSelection)
    expect_identical(firstPrivate$.state$features, before$features)
    expect_identical(firstPrivate$.state$vectorEndpoints, before$vectors)
})

test_that("visible nMDS output never exposes NaN or Inf text", {
    analysis <- run_nmds_private(
        nmds_state_data(n=24L),
        vars=paste0("feature_0", 1:4),
        factor="group",
        nmdsEnv=c("temperature", "pH"),
        nmdsSpecies=TRUE,
        nmdsHull=TRUE,
        seed=123,
        nmdsTrymax=5)
    result <- analysis$results
    visibleText <- paste(
        result$summary$asDF$item,
        result$summary$asDF$value,
        result$stress$asDF$item,
        result$stress$asDF$value,
        result$settings$asDF$setting,
        result$settings$asDF$value,
        as.character(result$warnings$asString()),
        as.character(result$ordinationDescription$asString()),
        as.character(result$shepardDescription$asString()),
        as.character(result$note$asString()),
        result$envfit$asDF$variable,
        result$envfit$asDF$r2,
        result$envfit$asDF$p,
        result$sites$asDF$NMDS1,
        result$sites$asDF$NMDS2,
        result$features$asDF$NMDS1,
        result$features$asDF$NMDS2,
        collapse=" ")

    expect_false(grepl(
        "(^|[^A-Za-z])(?:NaN|[-+]?Inf)([^A-Za-z]|$)",
        visibleText,
        perl=TRUE))
})

test_that("small and large fixtures match independent rotation-invariant nMDS fits", {
    expect_nmds_matches_independent(nmds_fixture("tofu-small.csv"), k=2L)
    expect_nmds_matches_independent(nmds_fixture("tofu-large.csv"), k=2L)
})

test_that("small and large fixture renderers keep features, arrows, and labels inside the plot frame", {
    expect_nmds_plot_inside_frame(nmds_fixture("tofu-small.csv"))
    expect_nmds_plot_inside_frame(nmds_fixture("tofu-large.csv"))
})

test_that("legacy 3D fixture matches an independent full configuration", {
    actual <- expect_nmds_matches_independent(
        nmds_fixture("tofu-small.csv"), k=3L)
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
    expect_match(
        as.character(actual$results$ordinationDescription$asString()),
        "projection")
})

test_that("legacy Binary is exactly inert for the fixture baseline", {
    data <- nmds_fixture("tofu-small.csv")
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
        "Data Summary",
        "Two-dimensional nMDS ordination",
        "Stress and convergence diagnostics",
        "Shepard diagram",
        "Environmental Fit",
        "Interpretation",
        "Site Scores",
        "Analysis settings",
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
