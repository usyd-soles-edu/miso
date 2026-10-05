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


test_that("SIMPER UI nests assessment controls under its enabling checkbox", {
    ui <- yaml::read_yaml(miso_fixture_path("jamovi", "simper.u.yaml"))
    assessment <- find_simper_yaml_node(ui, "simperAssess")
    expect_identical(assessment$style, "list")
    children <- setNames(assessment$children,
        vapply(assessment$children, `[[`, character(1), "name"))
    expect_identical(names(children),
        c("simperN", "simperAdjust", "useFixedSeed", "seed"))
    expect_true(all(vapply(children, function(child)
        identical(child$enable, "(simperAssess)"), logical(1))))
})

test_that("SIMPER UI dependencies and progressive disclosure are explicit", {
    source <- paste(
        readLines(miso_fixture_path("jamovi", "js", "simper.js")),
        collapse="\n")

    expect_match(source, "simperN\\.setEnabled\\(ui\\.simperAssess\\.value\\(\\)\\)")
    expect_match(source, "simperAdjust\\.setEnabled\\(ui\\.simperAssess\\.value\\(\\)\\)")
    expect_match(source,
        "seed\\.setEnabled\\(ui\\.simperAssess\\.value\\(\\) && ui\\.useFixedSeed\\.value\\(\\)\\)")
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
    expect_match(scenario$result_slot, "Contribution Variability", fixed=TRUE)
    expect_match(scenario$result_slot, "Group Means", fixed=TRUE)

    generator <- paste(
        readLines(test_path("..", "manual", "generate-reference-results.R")),
        collapse="\n")
    expect_match(
        generator,
        '"simper-small-transform", transform = "sqrt", details = TRUE',
        fixed=TRUE)
})


test_that("new and incomplete SIMPER analyses show one actionable state", {
    data <- simper_state_data()
    options <- simperOptions$new(vars=character(), factor=NULL)
    analysis <- simperClass$new(options=options, data=data)
    analysis$.__enclos_env__$private$.run()
    new <- analysis$results
    expect_simper_visibility(new,
        visible=c("contrasts", "contributions"),
        hidden=c("guidance", "warnings", "detailsByContrast", "contributionPlots", "heatmap", "heatmapDescription", "assessment"))

    features_only <- simper_test_run(
        data=data,
        vars=c("sp1", "sp2", "sp3"),
        factor=NULL)
    expect_match(as.character(features_only$guidance$asString()), "Grouping variable")
    expect_simper_visibility(features_only,
        visible=c("guidance", "contrasts", "contributions"),
        hidden=c("warnings", "detailsByContrast", "contributionPlots", "heatmap", "heatmapDescription", "assessment"))
})


simper_test_label <- function(pair) {
    if (any(grepl("\\bvs\\b", pair, ignore.case=TRUE)))
        paste0("\u201c", pair[[1L]], "\u201d vs \u201c", pair[[2L]], "\u201d")
    else
        paste(pair, collapse=" vs ")
}

simper_expected <- function(data, vars, factor, transform="none") {
    transformed <- miso_transform_community(as.matrix(data[, vars, drop=FALSE]), transform)
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
        simper_test_run,
        list(
            data=data,
            vars=vars,
            factor="group",
            simperTop=10,
            simperCum=70))))
    expected <- simper_expected(data, vars, "group")
    contrast_table <- result$contrasts$asDF
    contribution_table <- simper_test_full(result)

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
    result <- suppressWarnings(suppressMessages(simper_test_run(
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

    capped <- suppressWarnings(suppressMessages(simper_test_run(
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
    result <- suppressWarnings(suppressMessages(simper_test_run(
        data=data,
        vars=c("sp_1", "sp2"),
        factor="group"
    )))

    expected <- c(
        "\u201cWet_group\u201d vs \u201cDry vs control\u201d",
        "Wet_group vs Other",
        "\u201cDry vs control\u201d vs \u201cOther\u201d")
    expect_identical(result$contrasts$asDF$contrast, expected)
    expect_identical(unique(simper_test_full(result)$contrast), expected)
    expect_false(any(simper_test_full(result)$contrast == "Wet_group_Dry vs control"))
    expect_true(all(nzchar(simper_test_full(result)$contrast)))
})


test_that("incompatible hidden transformations stop actionably", {
    data <- simper_state_data()
    for (transform in c("standardize", "rclr")) {
        result <- simper_test_run(
            data=data,
            vars=c("sp1", "sp2", "sp3"),
            factor="group",
            transform=transform)
        expect_true(result$guidance$visible)
        expect_match(as.character(result$guidance$asString()), "cannot produce valid")
        expect_true(result$contributions$visible)
        expect_error(result$table, "does not exist", fixed=TRUE)
        expect_miso_empty_table(result$contributions)
        expect_equal(nrow(simper_test_full(result)), 0L)
    }
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

    result <- suppressWarnings(suppressMessages(miso::simper(
        data=simper_state_data(),
        vars=c("sp1", "sp2", "sp3"),
        factor="group",
        simperAssess=TRUE,
        simperN=19,
        simperPlots=TRUE,
        seed=123
    )))

    expect_true(result$contributions$visible)
    expect_error(result$table, "does not exist", fixed=TRUE)
    expect_gt(nrow(result$contributions$asDF), 0L)
    expect_true(result$contributionPlots$visible)
    expect_false(result$assessment$visible)
    expect_equal(length(result$assessment$rowKeys), 0L)
    expect_match(
        miso_squish_result(result$warnings),
        "simulated assessment failure")
})

test_that("single-pair blanks remain descriptive without hidden detail warnings", {
    result <- suppressWarnings(suppressMessages(simper_test_run(
        data=simper_state_data(counts=c(3L, 1L, 1L)),
        vars=c("sp1", "sp2", "sp3"),
        factor="group"
    )))
    table <- simper_test_full(result)

    expect_true(result$contributions$visible)
    expect_error(result$table, "does not exist", fixed=TRUE)
    expect_gt(nrow(table), 0L)
    expect_true(any(is.na(table$sd) | is.na(table$ratio)))
    expect_false(result$warnings$visible)
    expect_false(grepl("NaN|Inf", as.character(result$contributions$asString())))
    numeric <- table[vapply(table, is.numeric, logical(1))]
    expect_true(all(vapply(numeric, function(x) all(is.na(x) | is.finite(x)), logical(1))))
})


test_that("SIMPER contribution plots are optional display output", {
    options <- simperOptions$new(
        vars=c("sp1", "sp2", "sp3"), factor="group", seed=123)
    analysis <- simperClass$new(options=options, data=simper_state_data())

    suppressWarnings(suppressMessages(analysis$run()))
    contributions <- analysis$results$contributions$asDF
    contributionKeys <- analysis$results$contributions$rowKeys
    expect_false(analysis$results$contributionPlots$visible)
    expect_length(analysis$results$contributionPlots$items, 0L)

    plotOption <- options$option("simperPlots")
    plotOption$.__enclos_env__$private$.value <- TRUE
    suppressWarnings(suppressMessages(analysis$run()))
    expect_true(analysis$results$contributionPlots$visible)
    expect_length(analysis$results$contributionPlots$items, 3L)
    expect_identical(analysis$results$contributions$asDF, contributions)
    expect_identical(analysis$results$contributions$rowKeys, contributionKeys)

    plotOption$.__enclos_env__$private$.value <- FALSE
    suppressWarnings(suppressMessages(analysis$run()))
    expect_false(analysis$results$contributionPlots$visible)
    expect_length(analysis$results$contributionPlots$items, 3L)
    expect_identical(analysis$results$contributions$asDF, contributions)
    expect_identical(analysis$results$contributions$rowKeys, contributionKeys)
})


test_that("successful SIMPER hides duplicate descriptions and retains native plots", {
    result <- suppressWarnings(suppressMessages(simper_test_run(
        data=simper_state_data(), vars=c("sp1", "sp2", "sp3"),
        factor="group", simperPlots=TRUE, simperHeatmap=TRUE)))
    expect_true(result$contributionPlots$visible)
    expect_true(result$heatmap$visible)
    expect_false(result$heatmapDescription$visible)
    for (item in result$contributionPlots$items) {
        expect_false(item$description$visible)
        expect_true(nzchar(item$title))
        expect_false(is.null(item$plot$state))
    }
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
        simperCum=100,
        simperPlots=TRUE)
    analysis <- simperClass$new(options=options, data=data)
    suppressWarnings(suppressMessages(analysis$run()))
    private <- analysis$.__enclos_env__$private
    before <- unserialize(serialize(private$.state$plotData, NULL))
    contrast <- unique(private$.state$plotData$contrast)[[1L]]
    rows <- private$.state$plotData[
        private$.state$plotData$contrast == contrast, , drop=FALSE]
    plot <- private$.buildContributionPlot(rows, contrast)

    legacy <- simper_test_full(analysis)
    expect_true(all(c(first_feature, second_feature) %in% legacy$feature))
    expect_match(legacy$contrast[[1L]], first_group, fixed=TRUE)
    expect_match(legacy$contrast[[1L]], second_group, fixed=TRUE)
    expect_identical(private$.state$plotData, before)
    expect_s3_class(plot, "ggplot")
    expect_identical(plot$theme$plot.background$fill, "transparent")
    expect_identical(plot$theme$panel.background$fill, "transparent")
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
    expect_false(grepl(
        "Shows the leading feature contributions for each contrast",
        description_text, fixed=TRUE))
    expect_lte(nchar(description), 1250L)
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
        factor="group", simperTop=3, simperCum=100, simperPlots=TRUE)
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
    values <- plot$data

    expect_s3_class(plot, "ggplot")
    expect_identical(plot$theme$plot.background$fill, "transparent")
    expect_identical(plot$theme$panel.background$fill, "transparent")
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
            paste0(unname(.misoUniqueShortLabels(c(first_group,second_group),width=18L)[[1L]]), " higher"),
            paste0(unname(.misoUniqueShortLabels(c(first_group,second_group),width=18L)[[2L]]), " higher"),
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
    expect_identical(gsub("\n", "", unname(feature_labels), fixed=TRUE), features)
    expect_true(all(nchar(contrast_labels) <= 24L))
    expect_identical(sort(levels(heatmap$data$feature)), sort(features))
    expect_identical(
        sort(levels(heatmap$data$contrast)),
        sort(private$.state$contrastLabels))
})


test_that("saved small and large datasets independently confirm descriptive output", {
    for (dataset in c("miso-small.csv", "miso-large.csv")) {
        data <- read.csv(
            test_path("..", "manual", dataset),
            stringsAsFactors=FALSE,
            check.names=FALSE)
        vars <- grep("^feature_[0-9]+$", names(data), value=TRUE)
        data$group <- factor(data$group)
        result <- suppressWarnings(suppressMessages(do.call(
            simper_test_run,
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
            actual <- simper_test_full(result)[
                simper_test_full(result)$contrast == item$label, , drop=FALSE]
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

test_that("contribution plots and heatmap render from serialized Image state", {
    data <- simper_many_feature_data(groups=LETTERS[1:3])
    vars <- names(data)[names(data) != "group"]
    options <- simperOptions$new(
        vars=vars, factor="group",
        simperTop=3, simperCum=100, simperPlots=TRUE, simperHeatmap=TRUE)
    analysis <- simperClass$new(options=options, data=data)
    suppressWarnings(suppressMessages(analysis$run()))
    private <- analysis$.__enclos_env__$private
    items <- analysis$results$contributionPlots$items
    expect_length(items, 3L)

    # Change the presentation options after capturing state: restored plots
    # must render from serialized state, not from these option values.
    top_option <- options$option("simperTop")
    top_option$.__enclos_env__$private$.value <- 99
    cum_option <- options$option("simperCum")
    cum_option$.__enclos_env__$private$.value <- 1

    expect_rendered_from_state <- function(image, fun, width, height) {
        state <- image$state
        expect_false(is.null(state), info=image$key)
        restored <- unserialize(serialize(state, NULL))
        private$.state$plotData <- NULL
        private$.state$contrastLabels <- character()
        image$setState(restored)

        file <- tempfile(fileext=".png")
        on.exit({
            if (grDevices::dev.cur() > 1L)
                grDevices::dev.off()
            unlink(file)
        }, add=TRUE)
        grDevices::png(file, width=width, height=height)
        fun(image)
        grDevices::dev.off()
        expect_gt(
            file.info(file)$size,
            1000,
            label=paste("rendered PNG size for", image$key))
    }

    for (item in items) {
        image <- item$plot
        state <- image$state
        expect_false(is.null(state), info=image$key)
        expect_true(all(state$rows$contrastIndex == as.integer(image$key)), info=image$key)
        expect_true(all(state$rows$feature %in% vars), info=image$key)
        expect_equal(state$simperTop, 3, info=image$key)
        expect_equal(state$simperCum, 100, info=image$key)
        expect_rendered_from_state(
            image,
            function(img) private$.plotContribution(img),
            580, 430)

        plotFromState <- private$.buildContributionPlot(
            state$rows,
            image$key,
            top=state$simperTop,
            cumulative=state$simperCum)
        expect_identical(
            plotFromState$labels$subtitle,
            "Top 3 or 100% cumulative; crossing feature included",
            info=image$key)
    }

    heatmapImage <- analysis$results$heatmap
    heatmapState <- heatmapImage$state
    expect_false(is.null(heatmapState))
    expect_setequal(unique(heatmapState$contrast), items |> vapply(function(item) item$title, character(1)))
    expect_rendered_from_state(
        heatmapImage,
        function(img) private$.plotHeatmap(img),
        heatmapImage$size$width, heatmapImage$size$height)
})


test_that("structural display filtering rebuilds contribution rows while array items persist", {
    data <- simper_state_data()
    options <- simperOptions$new(
        vars=c("sp1", "sp2", "sp3"),
        factor="group",
        simperTop=10,
        simperPlots=TRUE,
        seed=123)
    analysis <- simperClass$new(options=options, data=data)
    suppressWarnings(suppressMessages(analysis$run()))
    keys <- analysis$results$contributions$rowKeys
    itemKeys <- analysis$results$contributionPlots$itemKeys
    rowsBefore <- nrow(analysis$results$contributions$asDF)

    topOption <- options$option("simperTop")
    topOption$.__enclos_env__$private$.value <- 1
    suppressWarnings(suppressMessages(analysis$run()))
    expect_false(identical(analysis$results$contributions$rowKeys, keys))
    expect_lt(nrow(analysis$results$contributions$asDF), rowsBefore)
    # The contrast set is unchanged, so the array items are not rebuilt.
    expect_identical(
        analysis$results$contributionPlots$itemKeys, itemKeys)
    expect_length(analysis$results$contributionPlots$items, 3L)
})

test_that("SIMPER omits routine method settings from untransformed table notes", {
    result <- suppressWarnings(suppressMessages(simper_test_run(
        data=simper_state_data(), vars=c("sp1", "sp2", "sp3"),
        factor="group", simperN=19, seed=123)))
    expect_true(result$contributions$visible)
    expect_identical(miso_table_note(result$contrasts, "meaning"), "")
})


test_that("SIMPER caption filtering survives saved state and legacy states", {
    analysis <- simperClass$new(options=simperOptions$new(
        vars=c("sp1", "sp2", "sp3"), factor="group", simperTop=1,
        simperPlots=TRUE, simperDetails=TRUE),
        data=simper_state_data())
    suppressWarnings(suppressMessages(analysis$run()))
    private <- analysis$.__enclos_env__$private
    state <- unserialize(serialize(analysis$results$contributionPlots$items[[1L]]$plot$state, NULL))
    # A zero-contribution feature can be omitted even when cumulative is 100%.
    state$rows$cumulative <- 100
    plot <- private$.buildContributionPlot(state$rows, "A vs B",
        top=state$simperTop, cumulative=state$simperCum, isFiltered=state$isFiltered)
    expect_match(plot$labels$subtitle, "crossing feature included", fixed=TRUE)
    # Legacy images infer omission only when their displayed cumulative total
    # establishes it; absence of the added flag must remain renderable.
    state$rows$cumulative <- 80
    legacy <- private$.buildContributionPlot(state$rows, "A vs B",
        top=state$simperTop, cumulative=state$simperCum)
    expect_identical(legacy$labels$subtitle, plot$labels$subtitle)
    row <- private$.state$descriptive$fullRows[[1L]]
    private$.state$descriptive$fullRows[[1L]]$sd <- NA_real_
    private$.state$descriptive$diagnostics <- c(private$.state$descriptive$diagnostics,
        list(list(contrastIndex=row$contrastIndex,feature=row$feature,
            pairCount=9L,reason="unexpected")))
    private$.setTableNotes()
    expect_match(miso_table_note(simper_test_detail_table(analysis$results), "meaning"),
        "Other blank SD or ratio cells indicate that a finite value could not be calculated.", fixed=TRUE)
})


test_that("SIMPER empty default footers leave no exported Note marker", {
    result <- suppressWarnings(suppressMessages(simper_test_run(
        data=simper_state_data(), vars=c("sp1", "sp2", "sp3"),
        factor="group", simperTop=50, simperCum=100, simperDetails=TRUE)))
    for (name in c("contrasts", "contributions")) {
        expect_null(result[[name]]$.__enclos_env__$private$.notes[["meaning"]])
        expect_false(grepl("Note.", as.character(result[[name]]$asString()), fixed=TRUE))
    }
})
