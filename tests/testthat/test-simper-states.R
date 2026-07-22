simper_state_data <- function(counts=c(3L, 3L, 3L), observed=c("B", "A", "C")) {
    groups <- rep(observed, counts)
    index <- seq_along(groups)
    effect <- c(A=0, B=4, C=8)[groups]
    data.frame(
        sp1=1 + effect + index %% 2,
        sp2=2 + effect / 2 + index %% 3,
        sp3=1 + rev(effect) / 4 + index %% 2,
        group=factor(groups, levels=c("A", "B", "C"))
    )
}

simper_many_feature_data <- function(groups=LETTERS[1:2], per_group=4L) {
    group <- rep(groups, each=per_group)
    group_index <- match(group, groups) - 1L
    row_index <- seq_along(group)
    data <- as.data.frame(setNames(lapply(seq_len(12L), function(feature) {
        1 + feature + group_index * (13 - feature) * 0.7 +
            ((row_index * feature) %% 5) / 10
    }), paste0("feature_", seq_len(12L))))
    names(data)[[12L]] <- paste0("feature_12_", paste(rep("full_long_label", 5L), collapse="_"))
    data$group <- factor(group, levels=rev(groups))
    data
}

expect_simper_visibility <- function(result, visible, hidden) {
    for (name in visible)
        expect_true(result[[name]]$visible, info=paste(name, "should be visible"))
    for (name in hidden)
        expect_false(result[[name]]$visible, info=paste(name, "should be hidden"))
}

find_simper_yaml_node <- function(node, name) {
    if (is.list(node) && identical(node$name, name))
        return(node)
    if (is.list(node)) {
        for (child in node) {
            found <- find_simper_yaml_node(child, name)
            if (! is.null(found))
                return(found)
        }
    }
    NULL
}

test_that("SIMPER schema follows the approved required-first hierarchy", {
    options <- yaml::read_yaml(tofu_fixture_path("jamovi", "simper.a.yaml"))$options
    ui <- yaml::read_yaml(tofu_fixture_path("jamovi", "simper.u.yaml"))
    by_name <- setNames(options, vapply(options, `[[`, character(1), "name"))
    option_names <- vapply(options, `[[`, character(1), "name")

    expect_identical(by_name$vars$title, "Feature variables (required)")
    expect_identical(by_name$factor$title, "Grouping variable (required)")
    expect_identical(by_name$simperCum$title, "Cumulative contribution (%)")
    expect_true(by_name$distance$hidden)
    expect_true(by_name$distBinary$hidden)

    schema_transforms <- setNames(
        by_name$transform$options,
        vapply(by_name$transform$options, `[[`, character(1), "name"))
    expect_true(all(c("standardize", "rclr") %in% names(schema_transforms)))
    transform_control <- find_simper_yaml_node(ui, "transform")
    expect_identical(
        vapply(transform_control$options, `[[`, character(1), "name"),
        c(
            "none", "sqrt", "fourthroot", "log", "pa", "wisconsin",
            "hellinger", "total", "max", "frequency", "normalize", "range",
            "chi.square")
    )

    expect_identical(
        option_names[seq.int(match("vars", option_names), match("simperCum", option_names))],
        c(
            "vars", "factor", "transform", "distance", "distBinary", "seed",
            "simperN", "simperTop", "simperCum")
    )
    expect_identical(option_names[match("simperCum", option_names) + 1:3],
                     c("simperHeatmap", "simperAssess", "simperAdjust"))
    expect_identical(tail(option_names, 1L), "simperDetails")
    expect_false(by_name$simperAssess$default)
    expect_identical(by_name$simperAdjust$default, "holm")
    expect_false(by_name$simperDetails$default)
    expect_false(by_name$simperHeatmap$default)

    expect_false(find_simper_yaml_node(ui, "analysisChoices")$collapsed)
    expect_false(find_simper_yaml_node(ui, "plots")$collapsed)
    expect_true(find_simper_yaml_node(ui, "permutationAssessment")$collapsed)
    expect_true(is.null(find_simper_yaml_node(ui, "distance")))
    expect_true(is.null(find_simper_yaml_node(ui, "distBinary")))

    ui_source <- paste(
        readLines(tofu_fixture_path("jamovi", "simper.u.yaml")),
        collapse="\n")
    compact_labels <- c(
        "Feature variables (required)",
        "Grouping variable (required)",
        "Dissimilarity: Bray-Curtis",
        "Tip",
        "Transforms affect contributions.",
        "Means use transformed values.",
        "Features shown",
        "Stop at Top N or cumulative %.",
        "Keep the threshold-crossing feature.")
    expect_true(all(vapply(compact_labels, grepl, logical(1), x=ui_source, fixed=TRUE)))
    expect_lte(max(nchar(compact_labels)), 38L)
    expect_false(grepl(" — ", ui_source, fixed=TRUE))
    expect_false(grepl("Whichever limit is reached first", ui_source, fixed=TRUE))
    expect_false(grepl("Transformations can change which features dominate", ui_source, fixed=TRUE))
    expect_false(grepl("Stop at the first limit", ui_source, fixed=TRUE))
    expect_false(grepl("Transformations affect Bray-Curtis", ui_source, fixed=TRUE))

    choices <- find_simper_yaml_node(ui, "analysisChoices")$children
    child_names <- vapply(
        choices, function(child) if (is.null(child$name)) "" else child$name,
        character(1))
    transform_index <- match("transform", child_names)
    expect_identical(choices[[transform_index + 1L]]$type, "LayoutBox")
    transform_tip <- choices[[transform_index + 1L]]$children
    expect_identical(
        vapply(transform_tip, `[[`, character(1), "label"),
        c("Tip", "Transforms affect contributions.", "Means use transformed values."))
    expect_true(transform_tip[[1L]]$heading)

    plots <- find_simper_yaml_node(ui, "plots")$children
    expect_identical(plots[[1L]]$label, "Contrast shown")
    expect_match(plots[[2L]]$label, "Each observed contrast")
    features_group <- plots[[3L]]
    expect_identical(features_group$type, "LayoutBox")
    expect_identical(features_group$children[[1L]]$label, "Features shown")
    expect_true(features_group$children[[1L]]$heading)
    expect_identical(
        vapply(features_group$children[2:3], `[[`, character(1), "name"),
        c("simperTop", "simperCum"))
    limits_tip <- features_group$children[[4L]]$children
    expect_identical(
        vapply(limits_tip, `[[`, character(1), "label"),
        c("Tip", "Stop at Top N or cumulative %.",
          "Keep the threshold-crossing feature."))
    expect_true(limits_tip[[1L]]$heading)
    expect_identical(plots[[4L]]$name, "simperHeatmap")
    expect_identical(choices[[transform_index + 3L]]$name, "simperDetails")
})

test_that("SIMPER UI dependencies and progressive disclosure are explicit", {
    source <- paste(
        readLines(tofu_fixture_path("jamovi", "js", "simper.js")),
        collapse="\n")

    expect_match(source, "simperN\\.setEnabled\\(ui\\.simperAssess\\.value\\(\\)\\)")
    expect_match(source, "simperAdjust\\.setEnabled\\(ui\\.simperAssess\\.value\\(\\)\\)")
    expect_match(source, "seed\\.setEnabled\\(ui\\.simperAssess\\.value\\(\\)\\)")
    expect_match(source, "permutationAssessment\\.expand\\(\\)")
    expect_match(source, "plots\\.expand\\(\\)")
    expect_match(source, "List choices are fixed by the analysis schema", fixed=TRUE)
})

test_that("SIMPER transform reference scenario requests the optional mean tables", {
    scenarios <- read.csv(
        test_path("..", "manual", "scenarios.csv"),
        stringsAsFactors=FALSE,
        check.names=FALSE)
    scenario <- scenarios[scenarios$scenario_id == "simper-small-transform", ]
    expect_equal(nrow(scenario), 1L)
    expect_match(scenario$model_or_options, "details=true", fixed=TRUE)
    expect_match(scenario$result_slot, "Contribution variability", fixed=TRUE)
    expect_match(scenario$result_slot, "Group means", fixed=TRUE)

    generator <- paste(
        readLines(test_path("..", "manual", "generate-reference-results.R")),
        collapse="\n")
    expect_match(
        generator,
        '"simper-small-transform", transform = "sqrt", details = TRUE',
        fixed=TRUE)
})

test_that("SIMPER result schema hides every empty shell and uses approved order", {
    results <- yaml::read_yaml(
        tofu_fixture_path("jamovi", "simper.r.yaml"))$items
    by_name <- setNames(results, vapply(results, `[[`, character(1), "name"))

    expect_true(all(vapply(results, function(item) identical(item$visible, FALSE), logical(1))))
    expect_identical(
        vapply(results, `[[`, character(1), "name"),
        c(
            "guidance", "summary", "warnings", "contrasts", "contributions",
            "variability", "means", "table", "contributionPlots", "heatmap",
            "heatmapDescription", "heatmapValues", "assessment", "note", "settings")
    )
    expect_identical(by_name$guidance$type, "Html")
    expect_identical(by_name$warnings$type, "Html")
    expect_identical(by_name$warnings$title, "Data handling warnings")
    expect_identical(by_name$contrasts$title, "Contrast summary")
    expect_identical(by_name$contributions$title, "Descriptive feature contributions")
    expect_identical(by_name$variability$title, "Contribution variability")
    expect_identical(by_name$means$title, "Group means")
    expect_identical(by_name$table$title, "Descriptive feature contributions")
    expect_identical(by_name$contributionPlots$title, "Contribution plots by contrast")
    expect_identical(by_name$contributionPlots$type, "Array")
    expect_identical(by_name$contributionPlots$template$type, "Group")
    expect_identical(
        vapply(by_name$contributionPlots$template$items, `[[`, character(1), "name"),
        c("plot", "description", "values"))
    expect_identical(by_name$heatmap$title, "Contrast overview heatmap")
    expect_identical(by_name$assessment$title, "Exploratory permutation assessment")
    expect_identical(by_name$note$type, "Html")
    expect_identical(by_name$settings$title, "Analysis settings")

    expect_identical(
        vapply(by_name$contrasts$columns, `[[`, character(1), "title"),
        c("Contrast", "n (first)", "n (second)", "Mean dissimilarity")
    )
    expect_identical(
        vapply(by_name$contributions$columns, `[[`, character(1), "name"),
        c("contrast", "feature", "contribution", "cumulative")
    )
    expect_identical(
        vapply(by_name$variability$columns, `[[`, character(1), "name"),
        c("contrast", "feature", "average", "sd", "ratio")
    )
    expect_identical(
        vapply(by_name$means$columns, `[[`, character(1), "name"),
        c("contrast", "feature", "meanFirst", "meanSecond")
    )
    expect_identical(
        vapply(by_name$table$columns, `[[`, character(1), "name"),
        c(
            "contrast", "feature", "average", "sd", "ratio", "meanFirst",
            "meanSecond", "contribution", "cumulative")
    )
    default_visible <- c("summary", "contrasts", "contributions",
                         "contributionPlots", "note", "settings")
    expect_lte(max(vapply(by_name[default_visible], function(item) {
        if (is.null(item$columns)) 0L else length(item$columns)
    }, integer(1))), 5L)
    expect_lte(length(by_name$variability$columns), 5L)
    expect_lte(length(by_name$means$columns), 4L)
    expect_lte(by_name$contributionPlots$template$items[[1L]]$width, 600L)
    expect_lte(by_name$contributionPlots$template$items[[1L]]$height, 650L)
    expect_lte(by_name$heatmap$width, 600L)
    expect_lte(by_name$heatmap$height, 650L)
})

test_that("new and incomplete SIMPER analyses show one actionable state", {
    data <- simper_state_data()
    options <- simperOptions$new(vars=character(), factor=NULL)
    analysis <- simperClass$new(options=options, data=data)
    analysis$.__enclos_env__$private$.run()
    new <- analysis$results
    expect_match(as.character(new$guidance$asString()), "max-width: 44em", fixed=TRUE)
    expect_match(as.character(new$guidance$asString()), "SIMPER \\(similarity percentages\\)")
    expect_match(as.character(new$guidance$asString()), "Feature variables")
    expect_match(as.character(new$guidance$asString()), "Grouping variable")
    expect_simper_visibility(
        new,
        visible="guidance",
        hidden=c(
            "summary", "warnings", "contrasts", "contributions", "variability",
            "means", "table", "contributionPlots", "heatmap",
            "heatmapDescription", "heatmapValues", "assessment", "note", "settings")
    )

    features_only <- simper(
        data=data,
        vars=c("sp1", "sp2", "sp3"),
        factor=NULL)
    expect_match(as.character(features_only$guidance$asString()), "Grouping variable")
    expect_simper_visibility(
        features_only,
        visible="guidance",
        hidden=c(
            "summary", "warnings", "contrasts", "contributions", "variability",
            "means", "table", "contributionPlots", "heatmap",
            "heatmapDescription", "heatmapValues", "assessment", "note", "settings")
    )
})

test_that("default SIMPER is descriptive and hides optional empty output", {
    result <- suppressWarnings(suppressMessages(tofu::simper(
        data=simper_state_data(),
        vars=c("sp1", "sp2", "sp3"),
        factor="group",
        simperTop=10,
        simperCum=70
    )))

    expect_simper_visibility(
        result,
        visible=c(
            "summary", "contrasts", "contributions", "contributionPlots",
            "note", "settings"),
        hidden=c(
            "guidance", "warnings", "variability", "means", "table",
            "heatmap", "heatmapDescription", "heatmapValues", "assessment")
    )
    expect_gt(nrow(result$contrasts$asDF), 0L)
    expect_gt(nrow(result$contributions$asDF), 0L)
    expect_gt(nrow(result$table$asDF), 0L)
    expect_false(result$table$visible)
    settings <- setNames(result$settings$asDF$value, result$settings$asDF$setting)
    expect_identical(settings[["Permutation assessment"]], "Disabled")
    expect_false(any(c("Requested permutations", "Effective permutations") %in%
                     names(settings)))
})

test_that("compact SIMPER rows stay filtered while detailed tables are complete", {
    result <- suppressWarnings(suppressMessages(simper(
        data=simper_state_data(),
        vars=c("sp1", "sp2", "sp3"),
        factor="group",
        simperDetails=TRUE)))
    legacy <- result$table$asDF

    expect_false(result$table$visible)
    expect_true(result$contributions$visible)
    expect_true(result$variability$visible)
    expect_true(result$means$visible)
    compact <- result$contributions$asDF
    expect_true(nrow(compact) <= nrow(legacy))
    compact_key <- paste(compact$contrast, compact$feature, sep="\r")
    legacy_key <- paste(legacy$contrast, legacy$feature, sep="\r")
    expect_true(all(compact_key %in% legacy_key))
    expect_identical(
        result$variability$asDF,
        legacy[c("contrast", "feature", "average", "sd", "ratio")])
    expect_identical(
        result$means$asDF,
        legacy[c("contrast", "feature", "meanFirst", "meanSecond")])
    expect_identical(result$variability$rowKeys, result$table$rowKeys)
    expect_identical(result$means$rowKeys, result$table$rowKeys)
})

simper_test_label <- function(pair) {
    if (any(grepl("\\bvs\\b", pair, ignore.case=TRUE)))
        paste0("\u201c", pair[[1L]], "\u201d vs \u201c", pair[[2L]], "\u201d")
    else
        paste(pair, collapse=" vs ")
}

simper_expected <- function(data, vars, factor, transform="none") {
    transformed <- tofu_transform_community(as.matrix(data[, vars, drop=FALSE]), transform)
    group <- droplevels(as.factor(data[[factor]]))
    pairs <- utils::combn(as.character(unique(group)), 2L, simplify=FALSE)
    fit <- vegan::simper(transformed, group, permutations=0L)
    summaries <- summary(fit)

    rows <- vector("list", length(pairs))
    for (index in seq_along(pairs)) {
        tab <- as.data.frame(summaries[[index]])
        tab$feature <- rownames(tab)
        tab <- tab[is.finite(tab$average) & tab$average >= 0, , drop=FALSE]
        tab <- tab[order(-tab$average, tab$feature), , drop=FALSE]
        tab$contribution <- 100 * tab$average / sum(tab$average)
        tab$cumulative <- cumsum(tab$contribution)
        rows[[index]] <- list(
            pair=pairs[[index]],
            label=simper_test_label(pairs[[index]]),
            overall=as.numeric(fit[[index]]$overall),
            table=tab)
    }
    rows
}

test_that("descriptive SIMPER matches independent vegan values and direction", {
    data <- simper_state_data()
    vars <- c("sp1", "sp2", "sp3")
    result <- suppressWarnings(suppressMessages(do.call(
        simper,
        list(
            data=data,
            vars=vars,
            factor="group",
            simperTop=10,
            simperCum=70))))
    expected <- simper_expected(data, vars, "group")
    contrast_table <- result$contrasts$asDF
    contribution_table <- result$table$asDF

    expect_identical(
        contrast_table$contrast,
        vapply(expected, `[[`, character(1), "label"))
    expect_equal(
        contrast_table$overall,
        vapply(expected, `[[`, numeric(1), "overall"),
        tolerance=1e-12)

    for (index in seq_along(expected)) {
        item <- expected[[index]]
        pair <- item$pair
        expect_identical(
            contrast_table$nFirst[[index]],
            as.integer(sum(as.character(data$group) == pair[[1L]])))
        expect_identical(
            contrast_table$nSecond[[index]],
            as.integer(sum(as.character(data$group) == pair[[2L]])))

        actual <- contribution_table[
            contribution_table$contrast == item$label, , drop=FALSE]
        reference <- item$table[match(actual$feature, item$table$feature), , drop=FALSE]
        expect_equal(actual$average, reference$average, tolerance=1e-12)
        expect_equal(actual$sd, reference$sd, tolerance=1e-12)
        expect_equal(actual$ratio, reference$ratio, tolerance=1e-12)
        expect_equal(actual$meanFirst, reference$ava, tolerance=1e-12)
        expect_equal(actual$meanSecond, reference$avb, tolerance=1e-12)
        expect_equal(actual$contribution, reference$contribution, tolerance=1e-12)
        expect_equal(actual$cumulative, reference$cumulative, tolerance=1e-12)
    }

    numeric <- contribution_table[c(
        "average", "contribution", "cumulative")]
    expect_true(all(is.finite(as.matrix(numeric))))
    expect_true(all(as.matrix(numeric) >= 0))
})

test_that("stopping rules include the crossing feature without renormalising", {
    data <- simper_state_data()
    result <- suppressWarnings(suppressMessages(simper(
        data=data,
        vars=c("sp1", "sp2", "sp3"),
        factor="group",
        simperTop=10,
        simperCum=70
    )))

    for (contrast in unique(result$contributions$asDF$contrast)) {
        rows <- result$contributions$asDF[
            result$contributions$asDF$contrast == contrast, , drop=FALSE]
        expect_gte(tail(rows$cumulative, 1L), 70)
        if (nrow(rows) > 1L)
            expect_lt(rows$cumulative[[nrow(rows) - 1L]], 70)
    }

    capped <- suppressWarnings(suppressMessages(simper(
        data=data,
        vars=c("sp1", "sp2", "sp3"),
        factor="group",
        simperTop=2,
        simperCum=100
    )))
    counts <- table(capped$contributions$asDF$contrast)
    expect_true(all(counts == 2L))
    expect_true(all(tapply(
        capped$contributions$asDF$cumulative,
        capped$contributions$asDF$contrast,
        max) < 100))
})

test_that("contrast labels follow first-observed groups without parsing names", {
    labels <- c("Wet_group", "Dry vs control", "Other")
    group <- rep(labels, each=3L)
    index <- seq_along(group)
    data <- data.frame(
        sp_1=1 + rep(c(0, 4, 8), each=3L) + index %% 2,
        sp2=2 + rep(c(0, 2, 4), each=3L) + index %% 3,
        group=factor(group, levels=rev(labels)))
    result <- suppressWarnings(suppressMessages(simper(
        data=data,
        vars=c("sp_1", "sp2"),
        factor="group"
    )))

    expected <- c(
        "\u201cWet_group\u201d vs \u201cDry vs control\u201d",
        "Wet_group vs Other",
        "\u201cDry vs control\u201d vs \u201cOther\u201d")
    expect_identical(result$contrasts$asDF$contrast, expected)
    expect_identical(unique(result$table$asDF$contrast), expected)
    expect_false(any(result$table$asDF$contrast == "Wet_group_Dry vs control"))
    expect_true(all(nzchar(result$table$asDF$contrast)))
})

test_that("legacy distance and Binary requests are inert and disclosed", {
    data <- simper_state_data()
    args <- list(
        data=data,
        vars=c("sp1", "sp2", "sp3"),
        factor="group",
        simperTop=10,
        simperCum=70)
    baseline <- suppressWarnings(suppressMessages(do.call(simper, args)))
    legacy <- suppressWarnings(suppressMessages(do.call(
        simper,
        c(args, list(distance="mahalanobis", distBinary=TRUE)))))

    expect_equal(legacy$contrasts$asDF, baseline$contrasts$asDF, tolerance=0)
    expect_equal(legacy$table$asDF, baseline$table$asDF, tolerance=0)
    warning <- as.character(legacy$warnings$asString())
    expect_match(warning, "legacy distance request")
    expect_match(warning, "legacy Binary dissimilarity request")
    expect_match(warning, "Presence/absence transformation")

    settings <- setNames(legacy$settings$asDF$value, legacy$settings$asDF$setting)
    expect_identical(settings[["Effective dissimilarity"]], "Bray-Curtis (fixed for SIMPER)")
    expect_identical(settings[["Ignored legacy distance request"]], "Mahalanobis")
})

test_that("incompatible hidden transformations stop actionably", {
    data <- simper_state_data()
    for (transform in c("standardize", "rclr")) {
        result <- simper(
            data=data,
            vars=c("sp1", "sp2", "sp3"),
            factor="group",
            transform=transform)
        expect_true(result$guidance$visible)
        expect_match(as.character(result$guidance$asString()), "cannot produce valid")
        expect_false(result$contributions$visible)
        expect_false(result$table$visible)
        expect_equal(nrow(result$contributions$asDF), 0L)
        expect_equal(nrow(result$table$asDF), 0L)
    }
})

test_that("permutation assessment is separate and adjusts before filtering", {
    data <- simper_state_data()
    vars <- c("sp1", "sp2", "sp3")
    result <- suppressWarnings(suppressMessages(do.call(
        simper,
        list(
            data=data,
            vars=vars,
            factor="group",
            simperAssess=TRUE,
            simperN=19,
            simperAdjust="holm",
            simperTop=1,
            simperCum=100,
            seed=123))))

    set.seed(123)
    fit <- vegan::simper(data[, vars], data$group, permutations=19)
    pairs <- utils::combn(as.character(unique(data$group)), 2L, simplify=FALSE)
    actual <- result$assessment$asDF
    expect_true(result$assessment$visible)
    expect_identical(nrow(actual), length(pairs))

    for (index in seq_along(pairs)) {
        label <- simper_test_label(pairs[[index]])
        row <- actual[actual$contrast == label, , drop=FALSE]
        p <- fit[[index]]$p
        expected_adjusted <- stats::p.adjust(p, method="holm")
        expect_equal(row$p, unname(p[row$feature]), tolerance=0)
        expect_equal(row$padj, unname(expected_adjusted[row$feature]), tolerance=0)
    }

    settings <- setNames(result$settings$asDF$value, result$settings$asDF$setting)
    expect_identical(settings[["Requested permutations"]], "19")
    expect_identical(settings[["Effective permutations"]], "19")
    expect_identical(settings[["P-value adjustment"]], "Holm")
    expect_identical(settings[["Random seed"]], "123")
    expect_match(
        as.character(result$note$asString()),
        "does[[:space:]]+not[[:space:]]+test[[:space:]]+contribution[[:space:]]+percentage")
})

test_that("effective permutations report the evaluated exhaustive count", {
    data <- data.frame(
        sp1=c(1, 2, 8),
        sp2=c(2, 1, 7),
        sp3=c(1, 2, 6),
        group=factor(c("A", "A", "B")))
    result <- suppressWarnings(suppressMessages(simper(
        data=data,
        vars=c("sp1", "sp2", "sp3"),
        factor="group",
        simperAssess=TRUE,
        simperN=99,
        seed=7
    )))
    settings <- setNames(result$settings$asDF$value, result$settings$asDF$setting)

    expect_identical(settings[["Requested permutations"]], "99")
    expect_identical(settings[["Effective permutations"]], "5")
})

test_that("assessment failure preserves valid descriptive output", {
    original_simper <- vegan::simper
    testthat::local_mocked_bindings(
        simper=function(..., permutations=999) {
            if (as.integer(permutations) > 0L)
                stop("simulated assessment failure")
            original_simper(..., permutations=permutations)
        },
        .package="vegan")

    result <- suppressWarnings(suppressMessages(tofu::simper(
        data=simper_state_data(),
        vars=c("sp1", "sp2", "sp3"),
        factor="group",
        simperAssess=TRUE,
        simperN=19,
        seed=123
    )))

    expect_true(result$contributions$visible)
    expect_false(result$table$visible)
    expect_gt(nrow(result$table$asDF), 0L)
    expect_true(result$contributionPlots$visible)
    expect_false(result$assessment$visible)
    expect_equal(length(result$assessment$rowKeys), 0L)
    expect_match(
        tofu_squish_result(result$warnings),
        "simulated assessment failure")
})

test_that("insufficient replication preserves descriptive blanks without NaN", {
    result <- suppressWarnings(suppressMessages(simper(
        data=simper_state_data(counts=c(3L, 1L, 1L)),
        vars=c("sp1", "sp2", "sp3"),
        factor="group"
    )))
    table <- result$table$asDF

    expect_true(result$contributions$visible)
    expect_false(result$table$visible)
    expect_gt(nrow(table), 0L)
    expect_true(any(is.na(table$sd) | is.na(table$ratio)))
    expect_match(as.character(result$warnings$asString()), "Replication is insufficient")
    expect_false(grepl("NaN|Inf", as.character(result$table$asString())))
    numeric <- table[vapply(table, is.numeric, logical(1))]
    expect_true(all(vapply(numeric, function(x) all(is.na(x) | is.finite(x)), logical(1))))
})

test_that("SIMPER valid invalid valid transitions clear stale output", {
    data <- simper_state_data()
    options <- simperOptions$new(
        vars=c("sp1", "sp2", "sp3"),
        factor="group",
        simperDetails=TRUE,
        simperHeatmap=TRUE,
        simperAssess=TRUE,
        simperN=19,
        seed=123)
    analysis <- simperClass$new(options=options, data=data)

    suppressWarnings(suppressMessages(analysis$run()))
    expect_true(analysis$results$contributions$visible)
    expect_true(analysis$results$variability$visible)
    expect_true(analysis$results$means$visible)
    expect_false(analysis$results$table$visible)
    expect_true(analysis$results$assessment$visible)
    expect_true(analysis$results$contributionPlots$visible)
    expect_true(analysis$results$heatmap$visible)
    expect_true(analysis$results$heatmapValues$visible)

    factor_option <- options$option("factor")
    factor_option$.__enclos_env__$private$.value <- NULL
    analysis$run()
    expect_true(analysis$results$guidance$visible)
    for (name in c("contributions", "variability", "means", "table")) {
        expect_false(analysis$results[[name]]$visible)
        expect_equal(length(analysis$results[[name]]$rowKeys), 0L)
    }
    expect_false(analysis$results$assessment$visible)
    expect_equal(length(analysis$results$assessment$rowKeys), 0L)
    expect_false(analysis$results$contributionPlots$visible)
    expect_length(analysis$results$contributionPlots$items, 0L)
    expect_false(analysis$results$heatmap$visible)
    expect_false(analysis$results$heatmapDescription$visible)
    expect_false(analysis$results$heatmapValues$visible)
    expect_equal(length(analysis$results$heatmapValues$rowKeys), 0L)

    factor_option$.__enclos_env__$private$.value <- "group"
    suppressWarnings(suppressMessages(analysis$run()))
    expect_false(analysis$results$guidance$visible)
    expect_true(analysis$results$contributions$visible)
    expect_true(analysis$results$variability$visible)
    expect_true(analysis$results$means$visible)
    expect_false(analysis$results$table$visible)
    expect_true(analysis$results$assessment$visible)
    expect_true(analysis$results$contributionPlots$visible)
    expect_true(analysis$results$heatmap$visible)
    expect_true(analysis$results$heatmapValues$visible)
})

test_that("five groups produce ten independently bounded contrast plots", {
    group <- rep(LETTERS[1:5], each=3L)
    index <- seq_along(group)
    effect <- rep(seq(0, 8, by=2), each=3L)
    data <- data.frame(
        sp1=1 + effect + index %% 2,
        sp2=2 + effect / 2 + index %% 3,
        group=factor(group, levels=rev(LETTERS[1:5])))
    result <- suppressWarnings(suppressMessages(simper(
        data=data,
        vars=c("sp1", "sp2"),
        factor="group"
    )))

    expect_equal(nrow(result$contrasts$asDF), 10L)
    expect_length(result$contributionPlots$items, 10L)
    expect_true(all(vapply(
        result$contributionPlots$items,
        function(item) item$plot$width <= 600 && item$plot$height <= 650,
        logical(1))))
    expect_true(all(vapply(
        result$contributionPlots$items,
        function(item) item$plot$height == 430,
        logical(1))))
    expect_identical(
        vapply(
            result$contributionPlots$items,
            function(item) item$title,
            character(1)),
        result$contrasts$asDF$contrast)
    expect_true(all(result$contrasts$asDF$contrast %in% result$table$asDF$contrast))
    for (item in result$contributionPlots$items) {
        values <- item$values$asDF
        expectedFeatures <- result$contributions$asDF$feature[
            result$contributions$asDF$contrast == item$title]
        expect_identical(values$feature, expectedFeatures)
        expect_true(all(values$firstGroup != ""))
        expect_true(all(values$secondGroup != ""))
        expect_false(grepl("NaN", as.character(item$values$asString())))
    }
})

test_that("SIMPER plot alternatives contain exactly the displayed values", {
    data <- simper_many_feature_data(groups=LETTERS[1:3])
    vars <- names(data)[names(data) != "group"]
    result <- suppressWarnings(suppressMessages(do.call(simper, list(
        data=data, vars=vars, factor="group", simperTop=4,
        simperCum=100, simperHeatmap=TRUE, simperDetails=TRUE))))

    compact <- result$contributions$asDF
    full <- result$table$asDF
    for (item in result$contributionPlots$items) {
        values <- item$values$asDF
        plotted <- compact[compact$contrast == item$title, , drop=FALSE]
        detailed <- full[
            full$contrast == item$title &
                full$feature %in% plotted$feature, , drop=FALSE]
        detailed <- detailed[match(plotted$feature, detailed$feature), ]
        expect_identical(values$feature, plotted$feature)
        expect_equal(values$contribution, plotted$contribution, tolerance=0)
        expect_equal(values$cumulative, plotted$cumulative, tolerance=0)
        expect_equal(values$average, detailed$average, tolerance=0)
        expect_equal(values$meanFirst, detailed$meanFirst, tolerance=0)
        expect_equal(values$meanSecond, detailed$meanSecond, tolerance=0)
        expect_true(all(nzchar(values$direction)))
    }

    heatmap <- result$heatmapValues$asDF
    expectedCells <- length(unique(compact$feature)) *
        length(unique(compact$contrast))
    expect_identical(nrow(heatmap), expectedCells)
    expect_identical(sum(heatmap$selected == "Yes"), nrow(compact))
    expect_true(all(is.na(heatmap$contribution[heatmap$selected == "No"])))
    omittedRows <- which(heatmap$selected == "No")
    expect_true(all(vapply(omittedRows, function(i) {
        identical(result$heatmapValues$getCell(
            rowKey=as.character(i), col="contribution")$value, "")
    }, logical(1))))
    selectedKey <- paste(
        heatmap$contrast[heatmap$selected == "Yes"],
        heatmap$feature[heatmap$selected == "Yes"], sep="\r")
    compactKey <- paste(compact$contrast, compact$feature, sep="\r")
    expect_setequal(selectedKey, compactKey)
    expect_false(grepl("NaN", as.character(result$heatmapValues$asString())))
})

test_that("two-group plots omit features only from images and compact rows", {
    data <- simper_many_feature_data()
    vars <- names(data)[names(data) != "group"]
    result <- suppressWarnings(suppressMessages(do.call(simper, list(
        data=data,
        vars=vars,
        factor="group",
        simperTop=3,
        simperCum=100,
        simperDetails=TRUE))))
    compact_only <- suppressWarnings(suppressMessages(do.call(simper, list(
        data=data,
        vars=vars,
        factor="group",
        simperTop=3,
        simperCum=100,
        simperDetails=FALSE))))

    expect_length(result$contributionPlots$items, 1L)
    expect_equal(nrow(result$contributions$asDF), 3L)
    expect_equal(nrow(result$table$asDF), 12L)
    expect_equal(nrow(result$variability$asDF), 12L)
    expect_equal(nrow(result$means$asDF), 12L)
    long_label <- vars[[12L]]
    expect_true(long_label %in% result$table$asDF$feature)
    expect_true(long_label %in% result$variability$asDF$feature)
    description <- gsub(
        "[[:space:]]+", " ",
        as.character(result$contributionPlots$items[[1L]]$description$asString()))
    expect_match(description, "3 features shown; 9 omitted", fixed=TRUE)
    expect_match(description, "retained in the detailed tables", fixed=TRUE)
    compact_description <- gsub(
        "[[:space:]]+", " ",
        as.character(
            compact_only$contributionPlots$items[[1L]]$description$asString()))
    expect_match(compact_description, "3 features shown; 9 omitted", fixed=TRUE)
    expect_match(
        compact_description,
        "available by enabling Show detailed statistics",
        fixed=TRUE)
    expect_identical(result$table$asDF, compact_only$table$asDF)
})

test_that("ten contrast plots stay filtered while every detailed table is complete", {
    data <- simper_many_feature_data(groups=LETTERS[1:5])
    vars <- names(data)[names(data) != "group"]
    result <- suppressWarnings(suppressMessages(do.call(simper, list(
        data=data,
        vars=vars,
        factor="group",
        simperTop=3,
        simperCum=100,
        simperDetails=TRUE))))

    expect_equal(nrow(result$contrasts$asDF), 10L)
    expect_length(result$contributionPlots$items, 10L)
    expect_true(all(table(result$contributions$asDF$contrast) == 3L))
    expect_true(all(table(result$table$asDF$contrast) == 12L))
    expect_true(all(table(result$variability$asDF$contrast) == 12L))
    expect_true(all(table(result$means$asDF$contrast) == 12L))
    expect_true(vars[[12L]] %in% result$table$asDF$feature)

    descriptions <- vapply(
        result$contributionPlots$items,
        function(item) gsub(
            "[[:space:]]+", " ", as.character(item$description$asString())),
        character(1))
    expect_true(all(grepl("3 features shown; 9 omitted", descriptions, fixed=TRUE)))
    expect_true(all(grepl("retained in the detailed tables", descriptions, fixed=TRUE)))
})

test_that("SIMPER ggplot builder bounds labels and preserves full table names", {
    first_group <- paste0("Group", paste(rep("AlphaUnbroken", 5L), collapse=""))
    second_group <- paste0("Group", paste(rep("BetaUnbroken", 5L), collapse=""))
    first_feature <- paste0("Feature", paste(rep("LongTokenOne", 5L), collapse=""))
    second_feature <- paste0("Feature", paste(rep("LongTokenTwo", 5L), collapse=""))
    data <- data.frame(
        first=c(1, 2, 3, 8, 9, 10),
        second=c(5, 4, 6, 1, 2, 1),
        group=factor(rep(c(first_group, second_group), each=3L)),
        check.names=FALSE)
    names(data)[1:2] <- c(first_feature, second_feature)
    options <- simperOptions$new(
        vars=c(first_feature, second_feature),
        factor="group",
        simperTop=2,
        simperCum=100)
    analysis <- simperClass$new(options=options, data=data)
    suppressWarnings(suppressMessages(analysis$run()))
    private <- analysis$.__enclos_env__$private
    before <- unserialize(serialize(private$.state$plotData, NULL))
    contrast <- unique(private$.state$plotData$contrast)[[1L]]
    rows <- private$.state$plotData[
        private$.state$plotData$contrast == contrast, , drop=FALSE]
    plot <- private$.buildContributionPlot(rows, contrast)

    legacy <- analysis$results$table$asDF
    expect_true(all(c(first_feature, second_feature) %in% legacy$feature))
    expect_match(legacy$contrast[[1L]], first_group, fixed=TRUE)
    expect_match(legacy$contrast[[1L]], second_group, fixed=TRUE)
    expect_identical(private$.state$plotData, before)
    expect_s3_class(plot, "ggplot")
    expect_identical(
        sort(as.character(plot$data$feature)),
        sort(c(first_feature, second_feature)))
    feature_labels <- plot$scales$get_scales("y")$labels
    expect_true(all(nchar(feature_labels) <= 24L))
    expect_true(any(grepl("\u2026", feature_labels, fixed=TRUE)))
    expect_match(plot$labels$x, "Contribution to average dissimilarity")
    expect_false(grepl("driver|responsible|significant", paste(plot$labels, collapse=" "), ignore.case=TRUE))

    description <- as.character(
        analysis$results$contributionPlots$items[[1L]]$description$asString())
    description_text <- gsub("[[:space:]]+", " ", description)
    expect_match(description, first_group, fixed=TRUE)
    expect_match(description, second_group, fixed=TRUE)
    expect_match(description, "Top 2|100%")
    expect_match(description, "features shown")
    expect_match(description, "descriptive")
    expect_match(description_text, "does not establish cause")
    expect_match(description_text, "available by enabling Show detailed statistics")
    expect_lte(nchar(description), 900L)
    expect_false(grepl(first_feature, description, fixed=TRUE))
})

test_that("SIMPER direction uses redundant fill and border patterns", {
    first_group <- "Alpha treatment group with full name"
    second_group <- "Beta reference group with full name"
    data <- data.frame(
        first_higher=c(9, 10, 11, 1, 2, 3),
        second_higher=c(1, 2, 3, 9, 10, 11),
        equal_means=c(1, 3, 5, 2, 3, 4),
        group=factor(rep(c(first_group, second_group), each=3L)))
    options <- simperOptions$new(
        vars=c("first_higher", "second_higher", "equal_means"),
        factor="group", simperTop=3, simperCum=100)
    analysis <- simperClass$new(options=options, data=data)
    suppressWarnings(suppressMessages(analysis$run()))
    private <- analysis$.__enclos_env__$private
    contrast <- unique(private$.state$plotData$contrast)[[1L]]
    rows <- private$.state$plotData[
        private$.state$plotData$contrast == contrast, , drop=FALSE]
    before <- rows[, c("feature", "contribution", "cumulative"), drop=FALSE]
    expected_order <- order(rows$contribution, rows$feature)
    plot <- private$.buildContributionPlot(rows, contrast)
    built <- ggplot2::ggplot_build(plot)
    fill_scale <- plot$scales$get_scales("fill")
    linetype_scale <- plot$scales$get_scales("linetype")
    values <- analysis$results$contributionPlots$items[[1L]]$values$asDF

    expect_s3_class(plot, "ggplot")
    expect_identical(
        as.character(plot$data$feature),
        as.character(before$feature[expected_order]))
    expect_equal(
        plot$data$contribution,
        before$contribution[expected_order], tolerance=0)
    expect_equal(
        plot$data$cumulative,
        before$cumulative[expected_order], tolerance=0)
    expect_identical(fill_scale$name, "Transformed group mean")
    expect_identical(linetype_scale$name, fill_scale$name)
    expect_identical(linetype_scale$breaks, fill_scale$breaks)
    expect_identical(linetype_scale$labels, fill_scale$labels)
    expect_length(fill_scale$breaks, 3L)
    expect_length(unique(built$data[[1L]]$fill), 3L)
    expect_length(unique(built$data[[1L]]$linetype), 3L)
    expect_setequal(
        values$direction,
        c(
            paste0(first_group, " higher"),
            paste0(second_group, " higher"),
            "Equal means"))
    expect_identical(
        analysis$results$contributionPlots$items[[1L]]$plot$width, 580)
    expect_identical(
        analysis$results$contributionPlots$items[[1L]]$plot$height, 430)
    expect_false(grepl(
        "driver|responsible|significant",
        paste(plot$labels, collapse=" "), ignore.case=TRUE))
})

test_that("colliding shortened identities remain distinct plot coordinates", {
    feature_prefix <- paste0("feature_", paste(rep("sharedprefix", 4L), collapse=""))
    features <- paste0(feature_prefix, c("_alpha", "_beta"))
    group_prefix <- paste0("Group", paste(rep("SharedPrefix", 4L), collapse=""))
    groups <- paste0(group_prefix, c("_A", "_B", "_C"))
    group <- rep(groups, each=4L)
    group_index <- match(group, groups) - 1L
    row_index <- seq_along(group)
    data <- data.frame(
        first=1 + 5 * group_index + row_index %% 3,
        second=2 + 3 * group_index + (row_index * 2) %% 5,
        group=factor(group, levels=rev(groups)),
        check.names=FALSE)
    names(data)[1:2] <- features
    analysis <- simperClass$new(
        options=simperOptions$new(
            vars=features, factor="group", simperTop=2,
            simperCum=100, simperHeatmap=TRUE),
        data=data)
    suppressWarnings(suppressMessages(analysis$run()))
    private <- analysis$.__enclos_env__$private
    contrast <- private$.state$contrastLabels[[1L]]
    rows <- private$.state$plotData[
        private$.state$plotData$contrast == contrast, , drop=FALSE]
    contribution_plot <- private$.buildContributionPlot(rows, contrast)
    contribution_build <- ggplot2::ggplot_build(contribution_plot)
    contribution_labels <- contribution_plot$scales$get_scales("y")$labels

    expect_equal(length(unique(contribution_build$data[[1L]]$y)), 2L)
    expect_length(unique(unname(contribution_labels)), 2L)
    expect_true(all(nchar(contribution_labels) <= 24L))
    expect_identical(sort(levels(contribution_plot$data$feature)), sort(features))

    heatmap <- private$.buildHeatmapPlot()
    heatmap_build <- ggplot2::ggplot_build(heatmap)
    feature_labels <- heatmap$scales$get_scales("y")$labels
    contrast_labels <- heatmap$scales$get_scales("x")$labels
    expect_equal(length(unique(heatmap_build$data[[1L]]$y)), 2L)
    expect_equal(length(unique(heatmap_build$data[[1L]]$x)), 3L)
    expect_length(unique(unname(feature_labels)), 2L)
    expect_length(unique(unname(contrast_labels)), 3L)
    expect_true(all(nchar(feature_labels) <= 20L))
    expect_true(all(nchar(contrast_labels) <= 24L))
    expect_identical(sort(levels(heatmap$data$feature)), sort(features))
    expect_identical(
        sort(levels(heatmap$data$contrast)),
        sort(private$.state$contrastLabels))
})

test_that("two groups default to one contrast plot and optional heatmap is bounded", {
    data <- simper_state_data(counts=c(4L, 4L), observed=c("B", "A"))
    base <- suppressWarnings(suppressMessages(simper(
        data=data,
        vars=c("sp1", "sp2", "sp3"),
        factor="group")))
    with_heatmap <- suppressWarnings(suppressMessages(simper(
        data=data,
        vars=c("sp1", "sp2", "sp3"),
        factor="group",
        simperHeatmap=TRUE)))

    expect_length(base$contributionPlots$items, 1L)
    expect_false(base$heatmap$visible)
    expect_false(base$heatmapDescription$visible)
    expect_false(base$heatmapValues$visible)
    expect_true(with_heatmap$heatmap$visible)
    expect_true(with_heatmap$heatmapDescription$visible)
    expect_true(with_heatmap$heatmapValues$visible)
    expect_lte(with_heatmap$heatmap$width, 600L)
    expect_lte(with_heatmap$heatmap$height, 650L)
    expect_equal(base$table$asDF, with_heatmap$table$asDF, tolerance=0)
    expect_equal(base$contrasts$asDF, with_heatmap$contrasts$asDF, tolerance=0)

    analysis <- simperClass$new(
        options=simperOptions$new(
            vars=c("sp1", "sp2", "sp3"), factor="group", simperHeatmap=TRUE),
        data=data)
    suppressWarnings(suppressMessages(analysis$run()))
    heatmap <- analysis$.__enclos_env__$private$.buildHeatmapPlot()
    expect_s3_class(heatmap, "ggplot")
    expect_true(inherits(heatmap$scales$get_scales("fill"), "ScaleContinuous"))
    expect_type(heatmap$data$missing, "logical")
    expect_false("contribution" %in% names(heatmap$data))
    expect_true(all(is.finite(heatmap$layers[[2L]]$data$contribution)))
    expect_identical(heatmap$scales$get_scales("fill")$na.value, "#D9D9D9")
})

test_that("saved small and large datasets independently confirm descriptive output", {
    for (dataset in c("tofu-small.csv", "tofu-large.csv")) {
        data <- read.csv(
            test_path("..", "manual", dataset),
            stringsAsFactors=FALSE,
            check.names=FALSE)
        vars <- grep("^feature_[0-9]+$", names(data), value=TRUE)
        data$group <- factor(data$group)
        result <- suppressWarnings(suppressMessages(do.call(
            simper,
            list(
                data=data,
                vars=vars,
                factor="group",
                simperTop=10,
                simperCum=70))))
        expected <- simper_expected(data, vars, "group")

        expect_identical(
            result$contrasts$asDF$contrast,
            vapply(expected, `[[`, character(1), "label"),
            info=dataset)
        expect_equal(
            result$contrasts$asDF$overall,
            vapply(expected, `[[`, numeric(1), "overall"),
            tolerance=1e-12,
            info=dataset)

        for (item in expected) {
            actual <- result$table$asDF[
                result$table$asDF$contrast == item$label, , drop=FALSE]
            reference <- item$table[
                match(actual$feature, item$table$feature), , drop=FALSE]
            expect_equal(actual$average, reference$average, tolerance=1e-12, info=dataset)
            expect_equal(actual$meanFirst, reference$ava, tolerance=1e-12, info=dataset)
            expect_equal(actual$meanSecond, reference$avb, tolerance=1e-12, info=dataset)
            expect_equal(
                actual$contribution,
                reference$contribution,
                tolerance=1e-12,
                info=dataset)
            expect_equal(actual$cumulative, reference$cumulative, tolerance=1e-12, info=dataset)
        }
    }
})
