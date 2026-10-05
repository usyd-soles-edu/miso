permdisp_inference_data <- function() {
    data.frame(
        x=c(1, 2, 4, 6, 9, 12, 15, 20, 23),
        y=c(4, 5, 9, 7, 3, 8, 2, 6, 12),
        group=factor(rep(c("A", "B", "C"), each=3L)))
}

permdisp_inference_run <- function(data=permdisp_inference_data(), ...) {
    suppressMessages(permdisp(data=data, vars=c("x", "y"), factor="group",
        permN=19, seed=123, ...))
}

test_that("PERMDISP rejects designs with no residual degrees of freedom", {
    data <- permdisp_inference_data()[c(1L, 4L, 7L), ]
    result <- permdisp_inference_run(data, dispPairwise=TRUE,
        showOrdinationPlot=TRUE)
    expect_true(result$guidance$visible)
    expect_match(miso_squish_result(result$guidance),
        "Add replicate samples to at least one group", fixed=TRUE)
    for (name in c("anova", "distances", "pairwise", "ordinationScores"))
        expect_miso_empty_table(result[[name]])
    expect_false(result$plot$visible)
    expect_false(result$ordinationPlot$visible)
})

test_that("undefined dispersion inference cannot leave reusable diagnostic state", {
    data <- permdisp_inference_data()
    options <- permdispOptions$new(vars=c("x", "y"), factor="group",
        distance="euclidean", dispPairwise=TRUE, showOrdinationPlot=TRUE,
        permN=19, seed=123)
    analysis <- permdispClass$new(options=options, data=data)
    suppressMessages(analysis$run())
    expected <- lapply(c("anova", "distances", "pairwise"), function(name)
        analysis$results[[name]]$asDF)
    expect_false(analysis$results$guidance$visible)
    expect_true(analysis$results$plot$visible)

    # Zero distances leave residual df but yield an undefined global F/p.
    invalid <- data
    invalid$x <- 1
    invalid$y <- 2
    private <- analysis$.__enclos_env__$private
    private$.data <- invalid
    suppressMessages(analysis$run())
    expect_true(analysis$results$guidance$visible)
    expect_match(miso_squish_result(analysis$results$guidance),
        "finite test statistic and p-value", fixed=TRUE)
    expect_null(private$.state$fit)
    expect_null(private$.state$distanceDiagnostic)
    expect_null(private$.state$ordination)
    expect_true(is.na(private$.state$pValue))
    for (name in c("anova", "distances", "pairwise", "ordinationScores"))
        expect_miso_empty_table(analysis$results[[name]])
    expect_identical(miso_table_note(analysis$results$anova, "structuralCells"), "")

    for (enabled in c(FALSE, TRUE)) {
        for (name in c("showDistancePlot", "showOrdinationPlot")) {
            option <- options$option(name)
            option$value <- enabled
            analysis$optionsChangedHandler(name)
        }
        suppressMessages(analysis$run())
        expect_true(analysis$results$guidance$visible)
        expect_false(analysis$results$plot$visible)
        expect_false(analysis$results$ordinationPlot$visible)
        expect_miso_empty_table(analysis$results$anova)
        expect_miso_empty_table(analysis$results$ordinationScores)
    }

    private$.data <- data
    suppressMessages(analysis$run())
    expect_false(analysis$results$guidance$visible)
    expect_true(analysis$results$plot$visible)
    expect_true(analysis$results$ordinationPlot$visible)
    actual <- lapply(c("anova", "distances", "pairwise"), function(name)
        analysis$results[[name]]$asDF)
    expect_equal(actual, expected, tolerance=0)
})

test_that("finite dispersion inference remains available with a singleton group", {
    data <- permdisp_inference_data()[seq_len(7L), ]
    data$group <- factor(c("A", rep("B", 3L), rep("C", 3L)))
    result <- permdisp_inference_run(data, distance="euclidean")
    set.seed(123)
    fit <- vegan::betadisper(stats::dist(data[c("x", "y")]), data$group)
    expected <- suppressMessages(vegan::permutest(fit,
        permutations=permute::how(nperm=19)))
    expect_false(result$guidance$visible)
    expect_equal(result$anova$asDF$f[[1L]], expected$tab[1L, "F"])
    expect_equal(result$anova$asDF$p[[1L]], expected$tab[1L, "Pr(>F)"])
    expect_equal(result$anova$asDF$df[[2L]], 4L)
})

test_that("distance and permutation warnings accompany usable PERMDISP results", {
    expect_warning(result <- permdisp_inference_run(), NA)
    expect_false(result$guidance$visible)
    expect_true(result$warnings$visible)
    expect_match(miso_squish_result(result$warnings),
        "Distance fit warning: some squared distances are negative and changed to zero",
        fixed=TRUE)
    expect_true(is.finite(result$anova$asDF$f[[1L]]))
    expect_true(is.finite(result$anova$asDF$p[[1L]]))

    # Two samples per group produce finite inference with perfect-fit warnings.
    data <- permdisp_inference_data()[seq_len(6L), ]
    data$group <- factor(rep(c("A", "B", "C"), each=2L))
    expect_warning(perfect <- permdisp_inference_run(data, distance="euclidean",
        dispPairwise=TRUE), NA)
    expect_false(perfect$guidance$visible)
    warnings <- miso_squish_result(perfect$warnings)
    expect_match(warnings, "Permutation test warning:", fixed=TRUE)
    expect_match(warnings, "Pairwise dispersion test warning:", fixed=TRUE)
    expect_match(warnings, "unreliable", fixed=TRUE)
    expect_true(is.finite(perfect$anova$asDF$f[[1L]]))
})

test_that("PERMDISP validates F and p separately and retains warnings on rejection", {
    original <- vegan::permutest
    column <- "F"
    testthat::local_mocked_bindings(permutest=function(...) {
        result <- original(...)
        warning("inference reliability warning", call.=FALSE)
        result$tab[1L, column] <- NA_real_
        result
    }, .package="vegan")
    for (name in c("F", "Pr(>F)")) {
        column <- name
        expect_warning(result <- permdisp_inference_run(distance="euclidean"), NA)
        expect_true(result$guidance$visible)
        expect_miso_empty_table(result$anova)
        expect_false(result$plot$visible)
        expect_true(result$warnings$visible)
        expect_match(miso_squish_result(result$warnings),
            "inference reliability warning", fixed=TRUE)
    }
})
