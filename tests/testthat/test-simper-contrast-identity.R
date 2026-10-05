simper_identity_data <- function(labels=c("D", "B", "A", "C")) {
    group <- rep(labels, each=4L)
    index <- seq_along(group)
    effect <- rep(seq_along(labels), each=4L)
    data.frame(
        x=2 + effect + index %% 3,
        y=1 + (5 - effect) * 2 + (index * 2) %% 5,
        z=2 + effect^2 + (index * 3) %% 7,
        group=factor(group, levels=rev(labels)))
}

simper_identity_run <- function(data, ...) {
    settings <- modifyList(list(vars=c("x", "y", "z"), factor="group",
        simperTop=2, simperCum=100, simperAssess=TRUE, simperN=19,
        useFixedSeed=TRUE, seed=123, simperDetails=TRUE, simperPlots=TRUE,
        simperHeatmap=TRUE), list(...))
    analysis <- simperClass$new(options=do.call(simperOptions$new, settings),
        data=data)
    suppressWarnings(suppressMessages(analysis$run()))
    analysis
}

test_that("delimiter-bearing group names retain every SIMPER contrast and output", {
    baseline <- simper_identity_run(simper_identity_data())
    for (labels in list(c("A", "B_C", "A_B", "C"),
            c("A", "B vs C", "A vs B", "C"),
            c("A", "B\u201d vs \u201cC", "A\u201d vs \u201cB", "C"))) {
        analysis <- simper_identity_run(simper_identity_data(labels))
        private <- analysis$.__enclos_env__$private
        expect_false(analysis$results$guidance$visible)
        expect_true(analysis$results$assessment$visible)
        expect_equal(nrow(analysis$results$contrasts$asDF), 6L)
        expect_length(unique(analysis$results$contrasts$asDF$contrast), 6L)
        expect_length(private$.state$descriptive$fullRows, 18L)
        for (name in c("contrasts", "contributions", "assessment")) {
            actual <- analysis$results[[name]]$asDF
            expected <- baseline$results[[name]]$asDF
            columns <- setdiff(names(actual), "contrast")
            expect_equal(actual[columns], expected[columns], tolerance=0)
        }

        pairs <- utils::combn(labels, 2L, simplify=FALSE)
        details <- analysis$results$detailsByContrast$items
        plots <- analysis$results$contributionPlots$items
        expect_length(details, 6L)
        expect_length(plots, 6L)
        expect_identical(vapply(plots, function(item) item$key, character(1)),
            as.character(seq_len(6L)))
        for (index in seq_along(pairs)) {
            rows <- private$.state$plotData
            rows <- rows[rows$contrastIndex == index, , drop=FALSE]
            expect_equal(nrow(rows), 2L)
            expect_identical(unique(rows$firstGroup), pairs[[index]][[1L]])
            expect_identical(unique(rows$secondGroup), pairs[[index]][[2L]])
            expect_identical(details[[index]]$key, as.character(index))
            expect_identical(details[[index]]$title, rows$contrast[[1L]])
            expect_identical(plots[[index]]$title, rows$contrast[[1L]])
            expect_equal(details[[index]]$asDF,
                baseline$results$detailsByContrast$items[[index]]$asDF,
                tolerance=0)
            expect_identical(plots[[index]]$plot$state$rows, rows)
            plot <- private$.buildContributionPlot(rows, rows$contrast[[1L]])
            expect_equal(nrow(ggplot2::ggplot_build(plot)$data[[1L]]), 2L)
        }

        heatmap <- analysis$results$heatmap$state
        expect_identical(unique(heatmap$contrastIndex), seq_len(6L))
        expect_equal(sum(!heatmap$missing), 12L)
        for (i in seq_len(nrow(heatmap))) {
            row <- heatmap[i, , drop=FALSE]
            selected <- private$.state$plotData
            selected <- selected[selected$contrastIndex == row$contrastIndex &
                selected$feature == row$feature, , drop=FALSE]
            expect_identical(row$missing, nrow(selected) == 0L)
            if (!row$missing)
                expect_equal(row$contribution, selected$contribution, tolerance=0)
        }
        built <- ggplot2::ggplot_build(private$.buildHeatmapPlot(heatmap))
        expect_equal(length(unique(built$data[[1L]]$x)), 6L)
        expect_equal(nrow(built$data[[2L]]), 12L)
    }
})

test_that("coded SIMPER groups preserve ordinary-label numbers order and random draws", {
    data <- simper_identity_data()
    analysis <- simper_identity_run(data, simperTop=50)
    private <- analysis$.__enclos_env__$private
    rng <- .Random.seed
    expected <- suppressMessages(vegan::simper(data[c("x", "y", "z")],
        data$group, permutations=0L))
    expect_identical(unname(private$.state$descriptive$fit), unname(expected))
    expect_identical(analysis$results$contrasts$asDF$contrast,
        c("D vs B", "D vs A", "D vs C", "B vs A", "B vs C", "A vs C"))

    set.seed(123)
    assessment <- suppressMessages(vegan::simper(data[c("x", "y", "z")],
        data$group, permutations=19L))
    expect_identical(.Random.seed, rng)
    actual <- analysis$results$assessment$asDF
    for (index in seq_along(assessment)) {
        label <- analysis$results$contrasts$asDF$contrast[[index]]
        rows <- actual[actual$contrast == label, , drop=FALSE]
        p <- assessment[[index]]$p
        adjusted <- stats::p.adjust(p, method="holm")
        expect_equal(rows$p, unname(p[rows$feature]), tolerance=0)
        expect_equal(rows$padj, unname(adjusted[rows$feature]), tolerance=0)
    }
})

test_that("SIMPER heatmaps render new and legacy serialized image states", {
    analysis <- simper_identity_run(simper_identity_data())
    private <- analysis$.__enclos_env__$private
    state <- unserialize(serialize(analysis$results$heatmap$state, NULL))
    # Result rendering must not depend on the live fit or analysis data.
    private$.state <- list()
    current <- ggplot2::ggplot_build(private$.buildHeatmapPlot(state))
    state$contrastIndex <- NULL
    legacy <- ggplot2::ggplot_build(private$.buildHeatmapPlot(state))
    expect_equal(legacy$data, current$data)
    expect_identical(private$.heatmapLayout(state)$width,
        private$.heatmapLayout(analysis$results$heatmap$state)$width)
})
