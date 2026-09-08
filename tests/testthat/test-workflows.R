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

    expect_match(trimws(as.character(res$guidance$asString())), "Add one or more numeric Feature variables.")
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

    expect_match(trimws(as.character(res$guidance$asString())), "waiting for a Grouping variable")
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

    expect_match(miso_squish_result(res$guidance), "Negative values detected in feature variables: a")
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
        miso_squish_result(res$guidance),
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
        miso_squish_result(all_zero$guidance),
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
        miso_squish_result(one_group$guidance),
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

    expect_false(res$warnings$visible)
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
        miso_squish_result(res$warnings),
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

    expect_match(miso_squish_result(res$guidance), "PERMANOVA model is saturated \\(no residual degrees of freedom\\)\\.")
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
    expect_false(res$warnings$visible)
    expect_equal(tab$r2[tab$source == "group"], 0.1788435, tolerance = 1e-6)
    expect_equal(tab$f[tab$source == "group"], 0.3266921, tolerance = 1e-6)
    expect_equal(tab$p[tab$source == "group"], 0.7, tolerance = 1e-6)

    for (source in c("Residual", "Total")) {
        rowKey <- res$table$rowKeys[[which(tab$source == source)]]
        expect_true(is.na(tab$f[tab$source == source]))
        expect_true(is.na(tab$p[tab$source == source]))
        expect_identical(res$table$getCell(rowKey=rowKey, col="f")$value, "")
        expect_identical(res$table$getCell(rowKey=rowKey, col="p")$value, "")
        expect_length(
            res$table$getCell(rowKey=rowKey, col="f")$footnotes,
            0L)
        expect_length(
            res$table$getCell(rowKey=rowKey, col="p")$footnotes,
            0L)
    }
    expect_length(res$table$notes, 0L)
    expect_false(grepl("NaN", res$asString(), fixed=TRUE))
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

    expect_match(
        as.character(res$note$asString()),
        "PERMDISP tests multivariate spread")
    expect_true(nrow(res$anova$asDF) >= 2L)
    expect_true(nrow(res$distances$asDF) >= 3L)
    expect_false(is.null(res$plot))

    tab <- res$anova$asDF
    residual <- which(tab$source %in% c("Residual", "Residuals"))
    expect_length(residual, 1L)
    expect_true(is.na(tab$f[residual]))
    expect_true(is.na(tab$p[residual]))

    rowKey <- res$anova$rowKeys[[residual]]
    expect_identical(res$anova$getCell(rowKey=rowKey, col="f")$value, "")
    expect_identical(res$anova$getCell(rowKey=rowKey, col="p")$value, "")
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
    expect_match(
        as.character(res$note$asString()),
        "R measures rank separation")
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
    expect_match(
        as.character(res$note$asString()),
        "SIMPER contributions are descriptive")
    expect_true(nrow(tab) >= 1L)
    expect_equal(tab$contribution[1], 48.47328, tolerance = 1e-5)
    expect_equal(names(tab)[names(tab) == "feature"], "feature")
    expect_false(res$table$visible)
    expect_true(res$contributions$visible)
    compact <- res$contributions$asDF
    complete <- tab[c("contrast", "feature", "contribution", "cumulative")]
    compact_key <- paste(compact$contrast, compact$feature, sep="\r")
    complete_key <- paste(complete$contrast, complete$feature, sep="\r")
    expected_compact <- complete[match(compact_key, complete_key), , drop=FALSE]
    rownames(compact) <- NULL
    rownames(expected_compact) <- NULL
    expect_identical(compact, expected_compact)
    expect_false(res$variability$visible)
    expect_false(res$means$visible)
    expect_equal(nrow(res$contrasts$asDF), 3L)
    expect_false(res$assessment$visible)
    settings <- setNames(res$settings$asDF$value, res$settings$asDF$setting)
    expect_identical(settings[["Permutation assessment"]], "Disabled")
    expect_length(res$contributionPlots$items, 3L)
    expect_true(all(vapply(
        res$contributionPlots$items,
        function(item) ! is.null(item$plot),
        logical(1))))
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
    summary <- setNames(res$summary$asDF$value, res$summary$asDF$item)
    expect_identical(summary[["Seed"]], "Fixed (123)")
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

    expect_false(grepl(
        "grouping variable|grouping layer",
        as.character(res$warnings$asString()),
        ignore.case=TRUE))
    summary <- setNames(res$summary$asDF$value, res$summary$asDF$item)
    expect_identical(summary[["Grouping assignment"]], "None")
}
)

test_that("new distance indices run through PERMANOVA", {
    for (dm in c("clark", "altGower")) {
        res <- suppressMessages(permanova(
            data = workflow_data(), vars = c("sp1", "sp2", "sp3"), factor = "group",
            distance = dm, permN = 19, seed = 123))
        expect_true(nrow(res$table$asDF) >= 1L, info = dm)
    }
})

test_that("new transformations run through PERMANOVA", {
    for (tr in c("rclr", "chi.square", "normalize", "range", "standardize", "max", "frequency")) {
        res <- suppressMessages(permanova(
            data = workflow_data(), vars = c("sp1", "sp2", "sp3"), factor = "group",
            transform = tr, distance = "euclidean", permN = 9, seed = 123))
        expect_true(nrow(res$table$asDF) >= 1L, info = tr)
    }
})

test_that("binary dissimilarity toggles presence/absence and warns", {
    res <- suppressMessages(permanova(
        data = workflow_data(), vars = c("sp1", "sp2", "sp3"), factor = "group",
        distBinary = TRUE, permN = 19, seed = 123))
    expect_match(as.character(res$warnings$asString()), "Binary \\(presence/absence\\) dissimilarity requested")
    expect_true(nrow(res$table$asDF) >= 1L)
})

test_that("mahalanobis is rejected when n <= p", {
    set.seed(1)
    wide <- as.data.frame(matrix(rpois(6 * 8, 3), nrow = 6, dimnames = list(NULL, paste0("g", 1:8))))
    wide$group <- factor(rep(c("A", "B", "C"), each = 2))
    res <- suppressMessages(permanova(
        data = wide, vars = paste0("g", 1:8), factor = "group",
        distance = "mahalanobis", permN = 9, seed = 123))
    expect_match(miso_squish_result(res$guidance), "mahalanobis requires more samples than features")
})

test_that("sqrt.dist and additive constant run in PERMANOVA", {
    res <- suppressMessages(permanova(
        data = workflow_data(), vars = c("sp1", "sp2", "sp3"), factor = "group",
        distSqrt = TRUE, distAdd = "cailliez", permN = 19, seed = 123))
    expect_true(nrow(res$table$asDF) >= 1L)

    res2 <- suppressMessages(permanova(
        data = workflow_data(), vars = c("sp1", "sp2", "sp3"), factor = "group",
        distAdd = "lingoes", permN = 19, seed = 123))
    expect_true(nrow(res2$table$asDF) >= 1L)
})

test_that("sqrt.dist and additive constant run in PERMDISP", {
    res <- suppressWarnings(suppressMessages(permdisp(
        data = workflow_data(), vars = c("sp1", "sp2", "sp3"), factor = "group",
        distSqrt = TRUE, distAdd = "cailliez", permN = 19, seed = 123)))
    expect_true(nrow(res$anova$asDF) >= 2L)
})

test_that("ANOSIM pairwise comparisons are p-adjusted", {
    res <- suppressMessages(anosim(
        data = workflow_data(), vars = c("sp1", "sp2", "sp3"), factor = "group",
        anosimPairwise = TRUE, anosimN = 19, seed = 123))
    pw <- res$pairwise$asDF
    expect_true(nrow(pw) >= 1L)
    expect_true("padj" %in% names(pw))
    expect_equal(pw$padj, stats::p.adjust(pw$p, method = "holm"))
})

test_that("PERMDISP pairwise comparisons report permutation p and t-statistic", {
    res <- suppressWarnings(suppressMessages(permdisp(
        data = workflow_data(), vars = c("sp1", "sp2", "sp3"), factor = "group",
        dispPairwise = TRUE, permN = 19, seed = 123)))
    pw <- res$pairwise$asDF
    expect_true(nrow(pw) >= 1L)
    expect_true(all(c("contrast", "statistic", "p", "padj") %in% names(pw)))
    expect_true(all(is.finite(pw$statistic)))
})

test_that("permutation schemes run in PERMANOVA", {
    for (sc in c("free", "series")) {
        res <- suppressMessages(permanova(
            data = workflow_data(), vars = c("sp1", "sp2", "sp3"), factor = "group",
            permScheme = sc, permN = 19, seed = 123))
        expect_true(nrow(res$table$asDF) >= 1L, info = sc)
    }
})

test_that("permutation schemes run in ANOSIM and PERMDISP", {
    for (sc in c("free", "stratified", "series")) {
        ra <- suppressMessages(anosim(
            data = workflow_data(), vars = c("sp1", "sp2", "sp3"), factor = "group",
            permScheme = sc, anosimN = 19, seed = 123))
        expect_true(nrow(ra$global$asDF) >= 1L, info = paste0("anosim-", sc))

        rp <- suppressWarnings(suppressMessages(permdisp(
            data = workflow_data(), vars = c("sp1", "sp2", "sp3"), factor = "group",
            permScheme = sc, permN = 19, seed = 123)))
        expect_true(nrow(rp$anova$asDF) >= 2L, info = paste0("permdisp-", sc))
    }
})

test_that("stratified scheme with a strata factor runs in PERMANOVA", {
    d <- workflow_data()
    d$block <- factor(c("x", "x", "x", "y", "y", "y"))
    res <- suppressMessages(permanova(
        data = d, vars = c("sp1", "sp2", "sp3"), factor = "group",
        strata = "block", permScheme = "stratified", permN = 19, seed = 123))
    expect_true(nrow(res$table$asDF) >= 1L)
})

test_that("parallel PERMANOVA reproduces serial results under the same seed", {
    serial <- suppressMessages(permanova(
        data = workflow_data(), vars = c("sp1", "sp2", "sp3"), factor = "group",
        useParallel = FALSE, permN = 19, seed = 123))
    par <- suppressMessages(permanova(
        data = workflow_data(), vars = c("sp1", "sp2", "sp3"), factor = "group",
        useParallel = TRUE, permN = 19, seed = 123))
    expect_equal(par$table$asDF$p, serial$table$asDF$p, tolerance = 1e-6)
})

test_that("continuous covariates are included as model terms", {
    d <- workflow_data()
    d$depth <- c(1.1, 2.2, 3.3, 4.4, 5.5, 6.6)
    res <- suppressMessages(permanova(
        data = d, vars = c("sp1", "sp2", "sp3"), factor = "group",
        covariates = "depth", permN = 19, seed = 123))
    tab <- res$table$asDF
    expect_true("depth" %in% tab$source)
})

test_that("covariates that saturate the model return a note", {
    d <- workflow_data()
    d$depth <- c(0, 1, 0, 1, 0, 1)
    d$temp <- c(0, 1, 0, 2, 0, 3)
    d$sal <- c(0, 1, 0, 3, 0, 2)
    res <- suppressMessages(permanova(
        data = d, vars = c("sp1", "sp2", "sp3"), factor = "group",
        covariates = c("depth", "temp", "sal"), permN = 9, seed = 123))
    expect_match(as.character(res$guidance$asString()), "PERMANOVA model is saturated")
})

test_that("envfit populates the environmental fit table", {
    d <- workflow_data()
    d$temp <- c(10, 12, 14, 16, 18, 20)
    d$depth <- c(1, 2, 3, 4, 5, 6)
    res <- suppressWarnings(suppressMessages(nmds(
        data = d, vars = c("sp1", "sp2", "sp3"), factor = "group",
        nmdsEnv = c("temp", "depth"), nmdsTrymax = 5, seed = 123)))
    ef <- res$envfit$asDF
    expect_true(nrow(ef) >= 1L)
    expect_true(all(c("temp", "depth") %in% ef$variable))
    expect_true(all(is.finite(ef$r2)))
    expect_true(all(is.finite(ef$p)))
    expect_identical(ef$samples, rep(6L, 2L))
    expect_identical(ef$permutations, rep(99L, 2L))
    expect_match(
        res$envfit$notes$interpretation$note,
        "association|causation|unadjusted",
        ignore.case=TRUE)
})

test_that("nMDS ordination ornaments run without error", {
    res <- suppressWarnings(suppressMessages(nmds(
        data = workflow_data(), vars = c("sp1", "sp2", "sp3"), factor = "group",
        nmdsSpecies = TRUE, nmdsHull = TRUE, nmdsEllipse = TRUE, nmdsSpider = TRUE,
        nmdsTrymax = 5, seed = 123)))
    expect_false(is.null(res$ordination))
    expect_false(is.null(res$envfit))
})

test_that("narrative HTML is readable, wrapping, and escaped", {
    html <- miso_html_block(c(
        "A deliberately long guidance sentence.",
        "<script>alert('x')</script> & more"))

    expect_match(html, "max-width: 44em", fixed=TRUE)
    expect_match(html, "line-height: 1.45", fixed=TRUE)
    expect_match(html, "overflow-wrap: anywhere", fixed=TRUE)
    expect_match(html, "word-break: normal", fixed=TRUE)
    expect_match(html, "font-family: inherit", fixed=TRUE)
    expect_match(html, "font-size: inherit", fixed=TRUE)
    expect_false(grepl("<script>", html, fixed=TRUE))
    expect_match(html, "&lt;script&gt;", fixed=TRUE)
    expect_match(html, "&amp; more", fixed=TRUE)

    purpose <- miso_html_block(
        "Tests whether multivariate composition is associated with each model term.",
        ariaLabel="About PERMANOVA table",
        title="PERMANOVA table")
    expect_match(purpose, 'role="note"', fixed=TRUE)
    expect_match(
        purpose,
        'aria-label="About PERMANOVA table"',
        fixed=TRUE)
    expect_match(purpose, 'role="heading"', fixed=TRUE)
    expect_match(purpose, 'aria-level="3"', fixed=TRUE)
    expect_match(purpose, "PERMANOVA table", fixed=TRUE)

    warning <- miso_warning_block(
        "Two rows with missing values were excluded.",
        title="Data handling warning")
    expect_match(warning, 'role="note"', fixed=TRUE)
    expect_match(warning, "Data handling warning", fixed=TRUE)
    expect_match(warning, "box-sizing: border-box", fixed=TRUE)
    expect_match(warning, "width: 100%", fixed=TRUE)
    expect_match(warning, "max-width: 100%", fixed=TRUE)
    expect_match(warning, "border-left:", fixed=TRUE)
    expect_match(warning, "overflow-wrap: anywhere", fixed=TRUE)
})
