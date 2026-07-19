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
    options <- yaml::read_yaml(test_path("..", "..", "jamovi", "permdisp.a.yaml"))$options
    ui <- yaml::read_yaml(test_path("..", "..", "jamovi", "permdisp.u.yaml"))
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

    expect_false(find_permdisp_yaml_node(ui, "analysisChoices")$collapsed)
    expect_true(find_permdisp_yaml_node(ui, "advancedOptions")$collapsed)
    expect_true(find_permdisp_yaml_node(ui, "reproducibility")$collapsed)
    expect_false(is.null(find_permdisp_yaml_node(ui, "permRestriction")))
    expect_true(is.null(find_permdisp_yaml_node(ui, "permScheme")))
})

test_that("PERMDISP UI dependencies and progressive disclosure are explicit", {
    source <- paste(
        readLines(test_path("..", "..", "jamovi", "js", "permdisp.js")),
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
        test_path("..", "..", "jamovi", "permdisp.r.yaml"))$items
    by_name <- setNames(results, vapply(results, `[[`, character(1), "name"))

    expect_true(all(vapply(results, function(item) identical(item$visible, FALSE), logical(1))))
    expect_identical(by_name$guidance$type, "Html")
    expect_identical(by_name$warnings$type, "Html")
    expect_identical(by_name$note$type, "Html")
    expect_identical(
        vapply(by_name$distances$columns, `[[`, character(1), "name"),
        c("group", "n", "distance", "median", "sd", "min", "max")
    )
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
        hidden=c("summary", "warnings", "distances", "anova", "pairwise", "plot", "note", "settings")
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
        hidden=c("summary", "warnings", "distances", "anova", "pairwise", "plot", "note", "settings")
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
        hidden=c("summary", "warnings", "distances", "anova", "pairwise", "plot", "note", "settings")
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
        hidden=c("summary", "warnings", "distances", "anova", "pairwise", "plot", "note", "settings")
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
        visible=c("summary", "distances", "anova", "plot", "note", "settings"),
        hidden=c("guidance", "warnings", "pairwise")
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
        visible=c("summary", "warnings", "distances", "anova", "plot", "note", "settings"),
        hidden=c("guidance", "pairwise")
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
