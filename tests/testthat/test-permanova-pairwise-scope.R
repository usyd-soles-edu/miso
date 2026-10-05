test_that("ordinary pairwise notes describe the selected test without altering inference", {
    i <- seq_len(18L)
    data <- data.frame(
        y=20 + 2 * i + rep(c(.1, -.2, .3, -.1, .2, -.3), 3L),
        group=factor(rep(c("A", "B", "C"), each=6L)),
        depth=as.numeric(i),
        nuisance=factor(c(rep("X", 12L), rep(c("X", "Y"), 3L))))
    results <- list()
    for (mode in c("terms", "margin")) {
        result <- suppressMessages(permanova(data=data, vars="y", factor="group",
            permFactors="nuisance", covariates="depth", distance="euclidean",
            permN=99, seed=123, permPairwise=TRUE, permBy=mode))
        actual <- result$pairwise$asDF
        expect_equal(nrow(actual), 3L)
        pairs <- utils::combn(levels(data$group), 2L, simplify=FALSE)
        expected <- lapply(pairs, function(pair) {
            subset <- droplevels(data[data$group %in% pair, ])
            distance <- stats::dist(subset$y)
            # The additional factor becomes constant in the A/B subset.
            formula <- if (nlevels(subset$nuisance) > 1L)
                distance ~ group + nuisance + depth else
                distance ~ group + depth
            set.seed(123)
            fit <- suppressMessages(vegan::adonis2(formula, data=subset,
                permutations=permute::how(nperm=99), by=mode))
            c(f=fit[1L, "F"], p=fit[1L, "Pr(>F)"])
        })
        expected <- do.call(rbind, expected)
        expect_equal(actual$f, expected[, "f"], tolerance=1e-10)
        expect_equal(actual$p, expected[, "p"], tolerance=0)
        expect_equal(actual$padj, stats::p.adjust(expected[, "p"], "holm"),
            tolerance=0)
        note <- miso_table_note(result$pairwise, "scope")
        expect_match(note, if (mode == "terms")
            "Sequential comparisons of group, tested first in each pairwise model." else
            "Marginal comparisons of group in each pairwise model.", fixed=TRUE)
        expect_false(grepl("adjusted for|nuisance|depth", note))
        expect_match(note, "Holm correction across 3 contrasts.", fixed=TRUE)
        results[[mode]] <- actual
    }
    # The confounded A/B contrast distinguishes the two actual hypotheses.
    expect_gt(results$terms$f[[1L]], 100 * results$margin$f[[1L]])
})

test_that("pairwise scope notes refresh when test type changes", {
    data <- data.frame(y=c(1, 2, 4, 6, 8, 11),
        group=factor(rep(c("A", "B", "C"), each=2L)))
    options <- permanovaOptions$new(vars="y", factor="group", distance="euclidean",
        permN=19, seed=123, permPairwise=TRUE)
    analysis <- permanovaClass$new(options=options, data=data)
    for (mode in c("terms", "margin", "terms")) {
        option <- options$option("permBy")
        option$value <- mode
        analysis$optionsChangedHandler("permBy")
        suppressMessages(analysis$run())
        fresh <- suppressMessages(permanova(data=data, vars="y", factor="group",
            distance="euclidean", permN=19, seed=123, permPairwise=TRUE,
            permBy=mode))
        expect_identical(miso_table_note(analysis$results$pairwise, "scope"),
            miso_table_note(fresh$pairwise, "scope"))
        expect_equal(analysis$results$pairwise$asDF, fresh$pairwise$asDF, tolerance=0)
    }
})
