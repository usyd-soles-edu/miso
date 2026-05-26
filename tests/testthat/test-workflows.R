workflow_data <- function() {
    data.frame(
        sp1 = c(1, 2, 1, 2, 1, 2),
        sp2 = c(2, 1, 3, 1, 4, 2),
        sp3 = c(1, 0, 1, 0, 2, 1),
        group = factor(c("A", "A", "B", "B", "C", "C"))
    )
}

test_that("missing feature variables provide a clear note", {
    res <- permanova(
        data = data.frame(group = c("A", "B")),
        vars = character(0),
        factor = "group"
    )

    expect_match(trimws(as.character(res$warnings$asString())), "Select one or more feature variables.")
})

test_that("missing grouping variable is required for group workflows", {
    res <- permanova(
        data = data.frame(
            sp1 = c(1, 2, 3, 4),
            sp2 = c(2, 1, 3, 4),
            group = c("A", "A", "B", "B")
        ),
        vars = c("sp1", "sp2"),
        factor = NULL
    )

    expect_match(trimws(as.character(res$warnings$asString())), "Select a primary grouping factor.")
})

test_that("missing selected columns are reported by option validation", {
    expect_error(
        permanova(
            data = data.frame(
                sp1 = c(1, 2, 3, 4),
                group = c("A", "A", "B", "B")
            ),
            vars = c("sp1", "missing_feature"),
            factor = "group"
        ),
        "Argument 'vars' contains 'missing_feature' which is not present in the dataset",
        fixed = TRUE
    )
})

test_that("non-numeric feature variables are rejected by option validation", {
    expect_error(
        permanova(
            data = data.frame(
                sp1 = c(1, 2, 3, 4),
                sp2 = c("low", "high", "low", "high"),
                group = c("A", "A", "B", "B")
            ),
            vars = c("sp1", "sp2"),
            factor = "group"
        ),
        "Argument 'vars' requires a numeric variable ('sp2' is not valid)",
        fixed = TRUE
    )
})

test_that("negative feature values are rejected with a clear note", {
    res <- permanova(
        data = data.frame(
            a = c(1, -1, 2),
            b = c(0, 1, 1),
            group = c("A", "A", "B")
        ),
        vars = c("a", "b"),
        factor = "group"
    )

    expect_match(as.character(res$warnings$asString()), "Negative values detected in feature variables: a")
})

test_that("missing rows are excluded and warning text is added", {
    res <- permanova(
        data = data.frame(
            a = c(1, NA, 2, 0),
            b = c(0, 1, 2, 1),
            group = c("A", "A", "B", "B")
        ),
        vars = c("a", "b"),
        factor = "group"
    )

    expect_match(as.character(res$warnings$asString()), "1 rows excluded due to missing values in selected variables\\.")
    expect_equal(res$summary$asDF$value[res$summary$asDF$item == "Samples used"], "3")
})

test_that("all-zero samples and features are filtered with note and retained feature count", {
    res <- permanova(
        data = data.frame(
            `sp 1` = c(0, 0, 1, 1, 2, 2),
            `sp-2` = c(0, 0, 1, 2, 2, 1),
            `sp 3` = c(0, 0, 0, 0, 0, 0),
            group = factor(c("A", "A", "A", "B", "B", "B")),
            check.names = FALSE
        ),
        vars = c("sp 1", "sp-2", "sp 3"),
        factor = "group"
    )

    note <- as.character(res$warnings$asString())
    expect_match(note, "2 all-zero samples excluded\\.")
    expect_match(note, "1 all-zero feature variables excluded\\.")
    expect_equal(as.character(res$summary$asDF$value[res$summary$asDF$item == "Feature variables used"]), "2")
})

test_that("entirely empty datasets return a clear note instead of crashing", {
    res <- permanova(
        data = data.frame(
            sp1 = numeric(),
            sp2 = numeric(),
            group = factor()
        ),
        vars = c("sp1", "sp2"),
        factor = "group"
    )

    expect_match(
        as.character(res$warnings$asString()),
        "No feature variables with non-zero values remain after filtering\\."
    )
})

test_that("fatal post-filter cases return clear notes", {
    all_zero <- permanova(
        data = data.frame(
            sp1 = c(0, 0, 0, 0),
            sp2 = c(0, 0, 0, 0),
            group = factor(c("A", "A", "B", "B"))
        ),
        vars = c("sp1", "sp2"),
        factor = "group"
    )

    expect_match(
        as.character(all_zero$warnings$asString()),
        "No feature variables with non-zero values remain after filtering\\.|Too few samples \\(0\\) for multivariate analysis\\."
    )

    one_group <- permanova(
        data = data.frame(
            sp1 = c(1, 2, 3, 4),
            sp2 = c(2, 3, 4, 5),
            group = factor(c("A", "A", "A", "A"))
        ),
        vars = c("sp1", "sp2"),
        factor = "group"
    )

    expect_match(
        as.character(one_group$warnings$asString()),
        "Primary factor 'group' has fewer than 2 groups after filtering\\."
    )
})

test_that("non-syntactic variable names are handled", {
    res <- suppressMessages(
        permanova(
            data = data.frame(
                `leaf cover` = c(1, 2, 1, 2, 3, 1),
                `sp-1` = c(2, 1, 0, 1, 2, 0),
                `site group` = factor(c("A", "A", "A", "B", "B", "B")),
                check.names = FALSE
            ),
            vars = c("leaf cover", "sp-1"),
            factor = "site group",
            permN = 9,
            seed = 123
        )
    )

    expect_equal(trimws(as.character(res$warnings$asString())), "")
    expect_equal(
        as.character(res$summary$asDF$value[res$summary$asDF$item == "Grouping variable"]),
        "site group"
    )
})

test_that("count-data distance warns when values are non-integer", {
    res <- permanova(
        data = data.frame(
            sp1 = c(1.2, 2.5, 1.1, 2.2, 3.4, 1.8),
            sp2 = c(2.1, 1.4, 3.2, 1.7, 4.1, 2.3),
            group = factor(c("A", "A", "A", "B", "B", "B"))
        ),
        vars = c("sp1", "sp2"),
        factor = "group",
        distance = "morisita"
    )

    expect_match(
        as.character(res$warnings$asString()),
        "'morisita' is designed for count data\\. Non-integer values detected\\."
    )
})

test_that("saturated PERMANOVA model reports a failure note instead of crashing", {
    res <- suppressMessages(
        permanova(
            data = data.frame(
                sp1 = c(1, 2, 3),
                sp2 = c(4, 5, 6),
                group = factor(c("A", "B", "C"))
            ),
            vars = c("sp1", "sp2"),
            factor = "group",
            permN = 9,
            seed = 123
        )
    )

    expect_match(as.character(res$note$asString()), "PERMANOVA failed: PERMANOVA model is saturated \\(no residual degrees of freedom\\)\\.")
    expect_equal(nrow(res$table$asDF), 0L)
})

test_that("PERMANOVA returns stable numeric results", {
    res <- suppressMessages(
        permanova(
            data = workflow_data(),
            vars = c("sp1", "sp2", "sp3"),
            factor = "group",
            permN = 19,
            seed = 123
        )
    )

    tab <- res$table$asDF
    expect_equal(trimws(as.character(res$warnings$asString())), "")
    expect_equal(tab$r2[tab$source == "group"], 0.1788435, tolerance = 1e-6)
    expect_equal(tab$f[tab$source == "group"], 0.3266921, tolerance = 1e-6)
    expect_equal(tab$p[tab$source == "group"], 0.7, tolerance = 1e-6)
})

test_that("PERMDISP returns test table and plot output", {
    res <- suppressWarnings(suppressMessages(
        permdisp(
            data = workflow_data(),
            vars = c("sp1", "sp2", "sp3"),
            factor = "group",
            permN = 19,
            seed = 123
        )
    ))

    expect_match(as.character(res$note$asString()), "PERMDISP tests whether groups differ in multivariate dispersion")
    expect_true(nrow(res$anova$asDF) >= 2L)
    expect_true(nrow(res$distances$asDF) >= 3L)
    expect_false(is.null(res$plot))
})

test_that("ANOSIM returns stable numeric results", {
    res <- suppressMessages(
        anosim(
            data = workflow_data(),
            vars = c("sp1", "sp2", "sp3"),
            factor = "group",
            anosimN = 19,
            seed = 123
        )
    )

    tab <- res$global$asDF
    expect_match(as.character(res$note$asString()), "rank-based group comparison")
    expect_equal(nrow(tab), 1L)
    expect_equal(tab$value[tab$statistic == "Global R"], -0.4444444, tolerance = 1e-6)
    expect_equal(tab$p[tab$statistic == "Global R"], 1, tolerance = 1e-6)
})

test_that("SIMPER returns stable contribution results and plot output", {
    res <- suppressMessages(
        simper(
            data = workflow_data(),
            vars = c("sp1", "sp2", "sp3"),
            factor = "group",
            simperN = 19,
            seed = 123
        )
    )

    tab <- res$table$asDF
    expect_match(as.character(res$note$asString()), "exploratory follow-up")
    expect_true(nrow(tab) >= 1L)
    expect_equal(tab$contribution[1], 48.47328, tolerance = 1e-5)
    expect_equal(names(tab)[names(tab) == "feature"], "feature")
    expect_false(is.null(res$plot))
})

test_that("nMDS returns stress results and plot outputs", {
    res <- suppressWarnings(suppressMessages(
        nmds(
            data = workflow_data(),
            vars = c("sp1", "sp2", "sp3"),
            factor = "group",
            nmdsTrymax = 5,
            seed = 123
        )
    ))

    stress <- res$stress$asDF
    expect_match(as.character(res$note$asString()), "Seed:")
    expect_true(any(stress$item == "Stress"))
    expect_equal(as.numeric(stress$value[stress$item == "Stress"]), 0, tolerance = 1e-6)
    expect_false(is.null(res$ordination))
    expect_false(is.null(res$shepard))
})

test_that("nMDS can run without an overlay grouping variable", {
    res <- suppressWarnings(suppressMessages(
        nmds(
            data = workflow_data(),
            vars = c("sp1", "sp2", "sp3"),
            factor = NULL,
            nmdsOverlay = FALSE,
            nmdsTrymax = 5,
            seed = 123
        )
    ))

    expect_equal(trimws(as.character(res$warnings$asString())), "")
    expect_equal(as.character(res$summary$asDF$value[res$summary$asDF$item == "Grouping variable"]), "not selected")
}
)
