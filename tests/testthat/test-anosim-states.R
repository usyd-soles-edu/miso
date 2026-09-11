anosim_state_data <- function(two_groups=FALSE, counts=c(3L, 3L, 3L)) {
    groups <- rep(c("A", "B", "C"), counts)
    index <- seq_along(groups)
    data <- data.frame(
        sp1=ifelse(groups == "A", 1, ifelse(groups == "B", 7, 3)) + index %% 2,
        sp2=ifelse(groups == "A", 2, ifelse(groups == "B", 8, 4)) + index %% 3,
        sp3=ifelse(groups == "A", 1, ifelse(groups == "B", 6, 3)) + index %% 2,
        group=factor(groups, levels=c("A", "B", "C")),
        block=factor(rep(seq_len(max(counts)), length.out=length(groups)))
    )
    if (isTRUE(two_groups))
        data <- droplevels(data[data$group != "C", , drop=FALSE])
    data
}

anosim_blocked_data <- function() {
    data <- expand.grid(
        group=factor(c("A", "B", "C"), levels=c("A", "B", "C")),
        block=factor(seq_len(4L)))
    data <- data[order(data$block, data$group), , drop=FALSE]
    group_effect <- c(A=0, B=3, C=6)[as.character(data$group)]
    block_effect <- as.integer(data$block) / 10
    data$sp1 <- 1 + group_effect + block_effect
    data$sp2 <- 2 + group_effect / 2 + block_effect
    data$sp3 <- 1 + rev(group_effect) / 3 + block_effect
    data[c("sp1", "sp2", "sp3", "group", "block")]
}

anosim_negative_data <- function() {
    data.frame(
        sp1=c(1, 2, 1, 2, 1, 2),
        sp2=c(2, 1, 3, 1, 4, 2),
        sp3=c(1, 0, 1, 0, 2, 1),
        group=factor(c("A", "A", "B", "B", "C", "C"))
    )
}

expect_anosim_visibility <- function(result, visible, hidden) {
    for (name in visible)
        expect_true(result[[name]]$visible, info=paste(name, "should be visible"))
    fixed <- c("global")
    for (name in hidden)
        if (name %in% fixed)
            expect_true(result[[name]]$visible, info=paste(name, "fixed shell should remain visible"))
        else
            expect_false(result[[name]]$visible, info=paste(name, "should be hidden"))
}

find_anosim_yaml_node <- function(node, name) {
    if (is.list(node) && identical(node$name, name))
        return(node)
    if (is.list(node)) {
        for (child in node) {
            found <- find_anosim_yaml_node(child, name)
            if (! is.null(found))
                return(found)
        }
    }
    NULL
}

test_that("ANOSIM schema follows the approved required-first hierarchy", {
    options <- yaml::read_yaml(miso_fixture_path("jamovi", "anosim.a.yaml"))$options
    ui <- yaml::read_yaml(miso_fixture_path("jamovi", "anosim.u.yaml"))
    by_name <- setNames(options, vapply(options, `[[`, character(1), "name"))

    expect_identical(by_name$vars$title, "Feature Variables")
    expect_identical(by_name$factor$title, "Grouping Variable")
    expect_identical(by_name$permRestriction$default, "free")
    expect_identical(
        vapply(by_name$permRestriction$options, `[[`, character(1), "name"),
        c("free", "stratified", "series")
    )
    expect_true(by_name$permScheme$hidden)
    expect_identical(by_name$permScheme$default, "free")
    expect_false(by_name$anosimPairwise$default)
    expect_identical(by_name$anosimAdjust$default, "holm")
    expect_true(by_name$showRankPlot$default)

    expect_false(find_anosim_yaml_node(ui, "analysisChoices")$collapsed)
    expect_false(find_anosim_yaml_node(ui, "plots")$collapsed)
    expect_false(is.null(find_anosim_yaml_node(ui, "showRankPlot")))
    expect_true(find_anosim_yaml_node(ui, "studyDesign")$collapsed)
    expect_true(find_anosim_yaml_node(ui, "reproducibility")$collapsed)
    expect_false(is.null(find_anosim_yaml_node(ui, "permRestriction")))
    expect_true(is.null(find_anosim_yaml_node(ui, "permScheme")))
})

test_that("ANOSIM UI nests adjustment under pairwise comparisons", {
    ui <- yaml::read_yaml(miso_fixture_path("jamovi", "anosim.u.yaml"))
    pairwise <- find_anosim_yaml_node(ui, "anosimPairwise")
    expect_identical(pairwise$style, "list")
    expect_identical(pairwise$children[[1L]]$name, "anosimAdjust")
    expect_identical(pairwise$children[[1L]]$enable, "(anosimPairwise)")
})

test_that("ANOSIM UI dependencies and progressive disclosure are explicit", {
    source <- paste(
        readLines(miso_fixture_path("jamovi", "js", "anosim.js")),
        collapse="\n")

    expect_match(
        source,
        "anosimAdjust\\.setEnabled\\(ui\\.anosimPairwise\\.value\\(\\)\\)")
    expect_match(source, "studyDesign\\.expand\\(\\)")
    expect_match(source, "reproducibility\\.expand\\(\\)")
    expect_match(source, "permRestriction\\.value\\(\\) !== 'free'")
})

test_that("ANOSIM result schema hides every empty shell", {
    results <- yaml::read_yaml(
        miso_fixture_path("jamovi", "anosim.r.yaml"))$items
    by_name <- setNames(results, vapply(results, `[[`, character(1), "name"))

    expect_true(all(vapply(results, function(item) identical(item$visible, FALSE), logical(1))))
    expect_identical(by_name$guidance$type, "Html")
    expect_identical(by_name$warnings$type, "Html")
    expect_identical(by_name$global$title, "Global ANOSIM")
    expect_identical(by_name$global$columns[[2L]]$title, "R")
    expect_identical(by_name$global$columns[[3L]]$title, "Permutation p")
    expect_identical(by_name$pairwise$columns[[4L]]$title, "Adjusted p")
    expect_identical(by_name$rankPlot$type, "Image")
    expect_true(by_name$rankPlot$width >= 580L)
    expect_true(by_name$rankPlot$width <= 600L)
    expect_true(by_name$rankPlot$height <= 500L)
    expect_identical(by_name$rankPlotDescription$type, "Html")
    expect_identical(
        vapply(by_name$rankSummary$columns, `[[`, character(1), "name"),
        c("category", "pairs", "median", "q1", "q3"))
})

test_that("new ANOSIM shows only complete getting-started guidance", {
    options <- anosimOptions$new(vars=character(), factor=NULL)
    analysis <- anosimClass$new(options=options, data=anosim_state_data())
    analysis$.__enclos_env__$private$.run()
    result <- analysis$results

    guidance <- as.character(result$guidance$asString())
    expect_match(guidance, "ANOSIM compares ranked")
    expect_match(guidance, "Feature variables")
    expect_match(guidance, "Grouping variable")
    expect_anosim_visibility(
        result,
        visible="guidance",
        hidden=c("warnings", "global", "pairwise")
    )
})

test_that("incomplete and fatal ANOSIM states show one correction", {
    features_only <- anosim(
        data=anosim_state_data(),
        vars=c("sp1", "sp2", "sp3"),
        factor=NULL
    )
    expect_match(as.character(features_only$guidance$asString()), "Grouping variable")
    expect_anosim_visibility(
        features_only,
        visible="guidance",
        hidden=c("warnings", "global", "pairwise")
    )

    invalid <- anosim(
        data=anosim_state_data(),
        vars=c("sp1", "sp2", "sp3"),
        factor="group",
        permRestriction="stratified"
    )
    expect_match(
        as.character(invalid$guidance$asString()),
        "Blocking[[:space:]|]+variable")
    expect_anosim_visibility(
        invalid,
        visible="guidance",
        hidden=c("warnings", "global", "pairwise")
    )
})

test_that("successful ANOSIM hides empty guidance warning and pairwise shells", {
    result <- suppressWarnings(suppressMessages(anosim(
        data=anosim_state_data(),
        vars=c("sp1", "sp2", "sp3"),
        factor="group",
        anosimN=19,
        seed=123
    )))

    expect_anosim_visibility(
        result,
        visible=c(
            "global", "rankPlot",
            "rankSummary"),
        hidden=c("guidance", "warnings", "pairwise", "rankPlotDescription")
    )
    expect_equal(nrow(result$global$asDF), 1L)
})

test_that("ANOSIM rank diagnostic uses exact fitted ranks and classes", {
    data <- anosim_state_data(two_groups=TRUE)
    options <- anosimOptions$new(
        vars=c("sp1", "sp2", "sp3"), factor="group",
        anosimN=19, seed=123)
    analysis <- anosimClass$new(options=options, data=data)
    suppressWarnings(suppressMessages(analysis$run()))
    private <- analysis$.__enclos_env__$private

    distance <- vegan::vegdist(data[c("sp1", "sp2", "sp3")], method="bray")
    set.seed(123)
    fit <- vegan::anosim(distance, data$group, permutations=19)
    plot_data <- private$.state$rankPlotData$all

    expect_identical(plot_data$rank, as.numeric(fit$dis.rank))
    expect_identical(plot_data$sourceClass, as.character(fit$class.vec))
    expect_identical(
        levels(plot_data$category),
        c("Between", "Within A", "Within B"))
    expect_identical(as.character(plot_data$category), ifelse(
        as.character(fit$class.vec) == "Between",
        "Between",
        paste("Within", as.character(fit$class.vec))))
})

test_that("ANOSIM rank summaries use every finite fitted pair exactly", {
    data <- anosim_state_data()
    options <- anosimOptions$new(
        vars=c("sp1", "sp2", "sp3"), factor="group",
        anosimN=19, seed=123)
    analysis <- anosimClass$new(options=options, data=data)
    suppressWarnings(suppressMessages(analysis$run()))
    private <- analysis$.__enclos_env__$private
    plot_data <- private$.state$rankPlotData
    actual <- analysis$results$rankSummary$asDF

    expected <- do.call(rbind, lapply(levels(plot_data$all$category), function(category) {
        values <- plot_data$all$rank[
            plot_data$all$category == category & is.finite(plot_data$all$rank)]
        data.frame(
            category=category,
            pairs=length(values),
            median=unname(stats::median(values)),
            q1=unname(stats::quantile(values, .25, names=FALSE)),
            q3=unname(stats::quantile(values, .75, names=FALSE)))
    }))

    expect_identical(actual$category, expected$category)
    expect_identical(actual$pairs, expected$pairs)
    for (column in c("median", "q1", "q3"))
        expect_equal(actual[[column]], expected[[column]])
    expect_identical(sum(actual$pairs), length(plot_data$all$rank))
})

test_that("ANOSIM rank raw marks are deterministic, bounded, and RNG-safe", {
    groups <- factor(rep(paste0("Group ", LETTERS[1:5]), each=20L))
    index <- seq_along(groups)
    data <- data.frame(
        sp1=as.integer(groups) * 4 + index %% 7,
        sp2=as.integer(groups) * 2 + index %% 5,
        sp3=as.integer(groups) + index %% 3,
        group=groups)
    options <- anosimOptions$new(
        vars=c("sp1", "sp2", "sp3"), factor="group",
        anosimN=19, seed=123)
    analysis <- anosimClass$new(options=options, data=data)

    suppressWarnings(suppressMessages(analysis$run()))
    private <- analysis$.__enclos_env__$private
    first <- private$.state$rankPlotData
    set.seed(8675309)
    before <- .Random.seed
    plot_one <- private$.buildRankPlot()
    plot_two <- private$.buildRankPlot()
    after_plot <- .Random.seed

    expect_identical(first$raw, private$.state$rankPlotData$raw)
    expect_lte(nrow(first$raw), 600L)
    expect_lt(nrow(first$raw), nrow(first$all))
    raw_counts <- table(first$raw$category)
    expect_identical(sum(raw_counts), 600L)
    expect_lte(max(raw_counts) - min(raw_counts), 1L)
    expect_identical(first$raw$x, plot_one$layers[[2L]]$data$x)
    expect_identical(
        ggplot2::ggplot_build(plot_one)$data[[2L]][c("x", "y")],
        ggplot2::ggplot_build(plot_two)$data[[2L]][c("x", "y")])
    expect_identical(plot_one$layers[[2L]]$aes_params$size, 2.1)
    expect_identical(plot_one$layers[[2L]]$aes_params$alpha, .9)
    expect_identical(before, after_plot)
})

test_that("ANOSIM rank raw subset is invariant to fitted-vector storage order", {
    category_levels <- c("Between", paste("Within", LETTERS[1:5]))
    source_levels <- c("Between", LETTERS[1:5])
    source <- rep(source_levels, length.out=1400L)
    fit <- list(
        dis.rank=seq_len(1400L),
        class.vec=factor(source, levels=source_levels))
    permutation <- order((seq_len(1400L) * 37L) %% 1401L)
    reordered <- list(
        dis.rank=fit$dis.rank[permutation],
        class.vec=factor(
            as.character(fit$class.vec)[permutation],
            levels=source_levels))

    analysis <- anosimClass$new(
        options=anosimOptions$new(
            vars=c("sp1", "sp2", "sp3"), factor="group",
            anosimN=19, seed=123),
        data=anosim_state_data())
    private <- analysis$.__enclos_env__$private
    first <- private$.prepareRankPlotData(fit)
    second <- private$.prepareRankPlotData(reordered)

    expect_identical(levels(first$raw$category), category_levels)
    expect_identical(first$raw, second$raw)
})

test_that("ANOSIM rank plot supports exactly 64 within-group styles", {
    group_index <- rep(seq_len(64L), each=2L)
    index <- seq_along(group_index)
    labels <- sprintf("Group %02d", seq_len(64L))
    data <- data.frame(
        sp1=1 + group_index * 3 + index %% 2L,
        sp2=2 + (65L - group_index) * 2 + index %% 3L,
        sp3=1 + group_index %% 7L + index %% 5L,
        group=factor(labels[group_index], levels=labels))
    analysis <- anosimClass$new(
        options=anosimOptions$new(
            vars=c("sp1", "sp2", "sp3"), factor="group",
            anosimN=19, seed=123),
        data=data)

    suppressWarnings(suppressMessages(analysis$run()))
    private <- analysis$.__enclos_env__$private
    plot <- private$.buildRankPlot()
    colours <- plot$scales$get_scales("colour")$palette(65L)
    shapes <- plot$scales$get_scales("shape")$palette(65L)

    expect_s3_class(plot, "ggplot")
    expect_identical(plot$theme$plot.background$fill, "transparent")
    expect_identical(plot$theme$panel.background$fill, "transparent")
    expect_identical(nrow(analysis$results$rankSummary$asDF), 65L)
    expect_identical(unname(colours[[1L]]), "#222222")
    expect_identical(unname(shapes[[1L]]), 1L)
    expect_identical(length(unique(paste(colours[-1L], shapes[-1L]))), 64L)
})

test_that("ANOSIM rank plot degrades gracefully above 64 groups", {
    labels <- sprintf("Group %02d", seq_len(65L))
    source_levels <- c("Between", labels)
    fit <- list(
        dis.rank=seq_along(source_levels),
        class.vec=factor(source_levels, levels=source_levels))
    options <- anosimOptions$new(
        vars=c("sp1", "sp2", "sp3"), factor="group",
        anosimN=19, seed=123)
    analysis <- anosimClass$new(options=options, data=anosim_state_data())
    private <- analysis$.__enclos_env__$private
    private$.state$rankPlotData <- private$.prepareRankPlotData(fit)
    private$.populateRankDiagnostic()

    expect_s3_class(private$.buildRankPlot(), "ggplot")
    expect_identical(nrow(analysis$results$rankSummary$asDF), 66L)
    expect_identical(analysis$results$rankPlotDescription$content, "")
})

test_that("ANOSIM rank plot handles long colliding labels accessibly", {
    labels <- c(
        "A very long treatment label with shared beginning alpha",
        "A very long treatment label with shared beginning beta",
        "A very long treatment label with shared beginning gamma",
        "A very long treatment label with shared beginning delta",
        "A very long treatment label with shared beginning epsilon")
    groups <- factor(rep(labels, each=4L), levels=labels)
    index <- seq_along(groups)
    data <- data.frame(
        sp1=as.integer(groups) * 3 + index %% 2,
        sp2=as.integer(groups) * 2 + index %% 3,
        sp3=as.integer(groups) + index %% 4,
        group=groups)
    options <- anosimOptions$new(
        vars=c("sp1", "sp2", "sp3"), factor="group",
        anosimN=19, seed=123)
    analysis <- anosimClass$new(options=options, data=data)
    suppressWarnings(suppressMessages(analysis$run()))
    private <- analysis$.__enclos_env__$private
    plot <- private$.buildRankPlot()
    built <- ggplot2::ggplot_build(plot)
    axis_labels <- built$layout$panel_params[[1L]]$x$get_labels()

    expect_s3_class(plot, "ggplot")
    expect_identical(plot$theme$plot.background$fill, "transparent")
    expect_identical(plot$theme$panel.background$fill, "transparent")
    expect_identical(length(unique(axis_labels)), length(axis_labels))
    expect_true(all(nchar(axis_labels) <= 28L))
    expect_true(any(grepl("~", axis_labels, fixed=TRUE)))
    expect_true(all(c("colour", "shape") %in% names(built$data[[2L]])))
    expect_identical(
        analysis$results$rankSummary$asDF$category,
        c("Between", paste("Within", labels)))
})


test_that("ANOSIM rank diagnostic clears stale output across option and input changes", {
    data <- anosim_state_data()
    options <- anosimOptions$new(
        vars=c("sp1", "sp2", "sp3"), factor="group",
        showRankPlot=TRUE, anosimN=19, seed=123)
    analysis <- anosimClass$new(options=options, data=data)
    suppressWarnings(suppressMessages(analysis$run()))
    expect_true(analysis$results$rankPlot$visible)
    expect_gt(nrow(analysis$results$rankSummary$asDF), 0L)

    show_option <- options$option("showRankPlot")
    show_option$.__enclos_env__$private$.value <- FALSE
    suppressWarnings(suppressMessages(analysis$run()))
    expect_false(analysis$results$rankPlot$visible)
    expect_false(analysis$results$rankPlotDescription$visible)
    expect_true(analysis$results$rankSummary$visible)
    expect_gt(nrow(analysis$results$rankSummary$asDF), 0L)
    expect_false(grepl(
        "Between ranks",
        as.character(analysis$results$rankPlotDescription$asString()),
        fixed=TRUE))

    show_option$.__enclos_env__$private$.value <- TRUE
    factor_option <- options$option("factor")
    factor_option$.__enclos_env__$private$.value <- NULL
    analysis$run()
    expect_true(analysis$results$guidance$visible)
    expect_false(analysis$results$rankPlot$visible)
    expect_false(analysis$results$rankPlotDescription$visible)
    expect_false(analysis$results$rankSummary$visible)
    expect_equal(nrow(analysis$results$rankSummary$asDF), 0L)
})

test_that("ANOSIM valid invalid valid transitions clear stale output", {
    data <- anosim_state_data()
    options <- anosimOptions$new(
        vars=c("sp1", "sp2", "sp3"),
        factor="group",
        anosimPairwise=TRUE,
        anosimN=19,
        seed=123)
    analysis <- anosimClass$new(options=options, data=data)

    suppressWarnings(suppressMessages(analysis$run()))
    expect_true(analysis$results$global$visible)
    expect_true(analysis$results$pairwise$visible)

    factor_option <- options$option("factor")
    factor_option$.__enclos_env__$private$.value <- NULL
    analysis$run()
    expect_true(analysis$results$guidance$visible)
    expect_true(analysis$results$global$visible)
    expect_false(analysis$results$pairwise$visible)
    expect_equal(length(analysis$results$global$rowKeys), 1L)
    expect_true(all(is.na(analysis$results$global$asDF$value)))
    expect_equal(length(analysis$results$pairwise$rowKeys), 0L)

    factor_option$.__enclos_env__$private$.value <- "group"
    suppressWarnings(suppressMessages(analysis$run()))
    expect_false(analysis$results$guidance$visible)
    expect_true(analysis$results$global$visible)
    expect_true(analysis$results$pairwise$visible)
})


test_that("ineffective ANOSIM blocks stop inference", {
    data <- anosim_state_data()
    data$block <- data$group
    result <- anosim(
        data=data,
        vars=c("sp1", "sp2", "sp3"),
        factor="group",
        strata="block",
        permRestriction="stratified",
        anosimN=19,
        seed=123
    )

    expect_match(miso_squish_result(result$guidance), "does not vary within any block")
    expect_true(result$global$visible)
    expect_equal(nrow(result$global$asDF), 1L)
    expect_true(all(is.na(result$global$asDF$value)))
})



test_that("ANOSIM retains successful pairwise contrasts and names failures", {
    partial <- suppressWarnings(suppressMessages(anosim(
        data=anosim_state_data(counts=c(3L, 3L, 1L)),
        vars=c("sp1", "sp2", "sp3"),
        factor="group",
        anosimPairwise=TRUE,
        anosimN=19,
        seed=123
    )))

    expect_true(partial$pairwise$visible)
    expect_identical(partial$pairwise$asDF$contrast, "A vs B")
    expect_match(as.character(partial$warnings$asString()), "A vs C")
    expect_match(as.character(partial$warnings$asString()), "B vs C")
})

test_that("ANOSIM hides pairwise output when every contrast fails", {
    result <- suppressWarnings(suppressMessages(anosim(
        data=anosim_state_data(counts=c(3L, 1L, 1L)),
        vars=c("sp1", "sp2", "sp3"),
        factor="group",
        anosimPairwise=TRUE,
        anosimN=19,
        seed=123
    )))

    expect_true(result$global$visible)
    expect_false(result$pairwise$visible)
    expect_equal(nrow(result$pairwise$asDF), 0L)
    expect_match(as.character(result$warnings$asString()), "Pairwise ANOSIM was unavailable")
})





test_that("rank plot renders from serialized Image state alone", {
    options <- anosimOptions$new(
        vars=c("sp1", "sp2", "sp3"), factor="group",
        showRankPlot=TRUE, anosimN=19, seed=123)
    analysis <- anosimClass$new(options=options, data=anosim_state_data())
    suppressWarnings(suppressMessages(analysis$run()))
    image <- analysis$results$rankPlot
    state <- image$state
    expect_false(is.null(state))
    expect_s3_class(state$all, "data.frame")
    expect_s3_class(state$raw, "data.frame")

    restored <- unserialize(serialize(state, NULL))
    analysis$.__enclos_env__$private$.state$rankPlotData <- NULL
    image$setState(restored)

    file <- tempfile(fileext=".png")
    on.exit({
        if (grDevices::dev.cur() > 1L)
            grDevices::dev.off()
        unlink(file)
    }, add=TRUE)
    grDevices::png(file, width=600, height=480)
    analysis$.__enclos_env__$private$.plotRank(image)
    grDevices::dev.off()
    expect_gt(file.info(file)$size, 1000)
})

test_that("rank plot renders from Image state even when showRankPlot is false", {
    options <- anosimOptions$new(
        vars=c("sp1", "sp2", "sp3"), factor="group",
        showRankPlot=FALSE, anosimN=19, seed=123)
    analysis <- anosimClass$new(options=options, data=anosim_state_data())
    suppressWarnings(suppressMessages(analysis$run()))
    image <- analysis$results$rankPlot
    state <- image$state
    expect_false(is.null(state))
    expect_s3_class(state$all, "data.frame")
    expect_false(image$visible)

    restored <- unserialize(serialize(state, NULL))
    analysis$.__enclos_env__$private$.state$rankPlotData <- NULL
    image$setState(restored)

    file <- tempfile(fileext=".png")
    on.exit({
        if (grDevices::dev.cur() > 1L)
            grDevices::dev.off()
        unlink(file)
    }, add=TRUE)
    grDevices::png(file, width=600, height=480)
    analysis$.__enclos_env__$private$.plotRank(image)
    grDevices::dev.off()
    expect_gt(file.info(file)$size, 1000)
})


test_that("structural grouping inputs rebuild pairwise rows while display toggles do not", {
    data <- anosim_state_data()
    data$quarter <- factor(rep(c("W", "X", "Y", "Z"), length.out=nrow(data)))
    options <- anosimOptions$new(
        vars=c("sp1", "sp2", "sp3"),
        factor="group",
        strata="quarter",
        anosimPairwise=TRUE,
        anosimN=19,
        seed=123)
    analysis <- anosimClass$new(options=options, data=data)
    suppressWarnings(suppressMessages(analysis$run()))
    keys <- analysis$results$pairwise$rowKeys
    contrastsBefore <- analysis$results$pairwise$asDF$contrast

    factorOption <- options$option("factor")
    factorOption$.__enclos_env__$private$.value <- "quarter"
    suppressWarnings(suppressMessages(analysis$run()))
    expect_false(identical(analysis$results$pairwise$rowKeys, keys))
    expect_gt(length(analysis$results$pairwise$rowKeys), length(keys))
    expect_false(identical(
        analysis$results$pairwise$asDF$contrast,
        contrastsBefore))
    expect_setequal(
        analysis$results$pairwise$asDF$contrast,
        c("W vs X", "W vs Y", "W vs Z", "X vs Y", "X vs Z", "Y vs Z"))
})

test_that("ANOSIM reports interpretation-critical settings in surviving table notes", {
    result <- suppressWarnings(suppressMessages(anosim(
        data=anosim_state_data(), vars=c("sp1", "sp2", "sp3"),
        factor="group", anosimPairwise=TRUE, anosimN=19, seed=123)))
    expect_true(result$global$visible)
    expect_match(miso_table_note(result$global, "meaning"), "Effective restriction")
    expect_match(miso_table_note(result$pairwise, "scope"), "P-value adjustment")
})

miso_anosim_cells <- function(table) {
    table$columns[[1L]]$.__enclos_env__$private$.cells
}

test_that("enabling the rank plot keeps routine prose absent without changing tables", {
    options <- anosimOptions$new(
        vars=c("sp1", "sp2", "sp3"), factor="group",
        showRankPlot=FALSE, anosimN=19, seed=123)
    analysis <- anosimClass$new(options=options, data=anosim_state_data())
    suppressWarnings(suppressMessages(analysis$run()))
    before <- analysis$results$global$asDF
    cells <- miso_anosim_cells(analysis$results$rankSummary)
    option <- options$option("showRankPlot")
    option$.__enclos_env__$private$.value <- TRUE
    suppressWarnings(suppressMessages(analysis$run()))
    fresh <- suppressWarnings(suppressMessages(anosim(
        data=anosim_state_data(), vars=c("sp1", "sp2", "sp3"),
        factor="group", showRankPlot=TRUE, anosimN=19, seed=123)))
    expect_identical(analysis$results$rankPlotDescription$asString(),
        fresh$rankPlotDescription$asString())
    expect_identical(analysis$results$rankPlotDescription$content, "")
    expect_false(analysis$results$rankPlotDescription$visible)
    expect_equal(analysis$results$global$asDF, before, tolerance=0)
    expect_identical(miso_anosim_cells(analysis$results$rankSummary), cells)
})


test_that("value-only data edits refresh ANOSIM tables and keep their cells", {
    data <- anosim_state_data()
    options <- anosimOptions$new(
        vars=c("sp1", "sp2", "sp3"), factor="group",
        anosimPairwise=TRUE, anosimN=19, seed=123)
    analysis <- anosimClass$new(options=options, data=data)
    suppressWarnings(suppressMessages(analysis$run()))
    pairwiseKeys <- analysis$results$pairwise$rowKeys
    rankKeys <- analysis$results$rankSummary$rowKeys
    pairwiseCells <- miso_anosim_cells(analysis$results$pairwise)
    rankCells <- miso_anosim_cells(analysis$results$rankSummary)
    before <- analysis$results$pairwise$asDF

    # Same samples, groups, and contrasts; only feature values change, so the
    # data signature changes but every row key survives the rerun.
    edited <- anosim_state_data()
    edited$sp1 <- edited$sp1 + 0.25
    analysis$.__enclos_env__$private$.data$sp1 <- edited$sp1
    suppressWarnings(suppressMessages(analysis$run()))

    expect_identical(analysis$results$pairwise$rowKeys, pairwiseKeys)
    expect_identical(analysis$results$rankSummary$rowKeys, rankKeys)
    expect_true(identical(
        miso_anosim_cells(analysis$results$pairwise), pairwiseCells))
    expect_true(identical(
        miso_anosim_cells(analysis$results$rankSummary), rankCells))
    expect_false(isTRUE(all.equal(before, analysis$results$pairwise$asDF)))

    fresh <- anosimClass$new(options=anosimOptions$new(
        vars=c("sp1", "sp2", "sp3"), factor="group",
        anosimPairwise=TRUE, anosimN=19, seed=123), data=edited)
    suppressWarnings(suppressMessages(fresh$run()))
    expect_equal(
        analysis$results$pairwise$asDF, fresh$results$pairwise$asDF,
        tolerance=1e-12)
    expect_equal(
        analysis$results$rankSummary$asDF, fresh$results$rankSummary$asDF,
        tolerance=1e-12)
})

test_that("data edits that remove a group rebuild ANOSIM rows and stay fresh", {
    data <- anosim_state_data()
    options <- anosimOptions$new(
        vars=c("sp1", "sp2", "sp3"), factor="group", anosimN=19, seed=123)
    analysis <- anosimClass$new(options=options, data=data)
    suppressWarnings(suppressMessages(analysis$run()))
    rankKeys <- analysis$results$rankSummary$rowKeys

    edited <- anosim_state_data(two_groups=TRUE)
    analysis$.__enclos_env__$private$.data <- edited
    suppressWarnings(suppressMessages(analysis$run()))

    expect_false(identical(analysis$results$rankSummary$rowKeys, rankKeys))
    expect_identical(
        analysis$results$rankSummary$asDF$category,
        c("Between", "Within A", "Within B"))

    fresh <- anosimClass$new(options=anosimOptions$new(
        vars=c("sp1", "sp2", "sp3"), factor="group", anosimN=19, seed=123),
        data=edited)
    suppressWarnings(suppressMessages(fresh$run()))
    expect_equal(
        analysis$results$rankSummary$asDF, fresh$results$rankSummary$asDF,
        tolerance=1e-12)
})


test_that("ANOSIM publication output omits routine prose", {
    result <- suppressWarnings(suppressMessages(anosim(
        data=anosim_state_data(), vars=c("sp1", "sp2", "sp3"),
        factor="group", seed=123, anosimN=19)))
    note <- miso_table_note(result$global, "meaning")
    expect_false(grepl("Random seed|Execution:|Disabled|Requested permutation restriction|R compares|not applicable to the Residual", note))
    expect_false(result$rankPlotDescription$visible)
    expect_identical(result$rankPlotDescription$content, "")
})
