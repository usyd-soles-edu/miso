conditional_effects_data <- function() {
    y <- c(1, 2, 3, 5, 6, 8, 9, 12, 20, 21, 22,
           1, 2, 3, 5, 6, 8, 9, 12, 1.5, 2.5, 3.5, 5.5)
    data.frame(
        y=y,
        A=factor(c(rep("A1", 4), rep("A2", 4), rep("A3", 3),
                   rep("A1", 3), rep("A2", 5), rep("A3", 4))),
        B=factor(c(rep("B1", 11), rep("B2", 12))))
}

conditional_effects_options <- function(...) {
    options <- list(
        vars="y",
        factor="A",
        permFactors="B",
        permInteractions=TRUE,
        permPairwise=TRUE,
        permBy="margin",
        transform="none",
        distance="euclidean",
        permN=99,
        seed=123)
    overrides <- list(...)
    for (name in names(overrides))
        options[[name]] <- overrides[[name]]
    options
}

run_conditional_effects <- function(data=conditional_effects_data(), ...) {
    suppressWarnings(suppressMessages(do.call(
        permanova,
        c(list(data=data), conditional_effects_options(...)))))
}

conditional_warning_text <- function(result) {
    miso_squish_result(result$warnings)
}

# Independent pooled-variance two-sample F; identical to one-way ANOVA F.
pooled_f <- function(x, z) {
    y <- c(x, z)
    sp2 <- (sum((x - mean(x))^2) + sum((z - mean(z))^2)) / (length(y) - 2L)
    (mean(x) - mean(z))^2 / (sp2 * (1 / length(x) + 1 / length(z)))
}

# Independent squared-distance sums-of-squares pseudo-F (Anderson 2001).
distance_ss_f <- function(x, z) {
    y <- c(x, z)
    group <- rep(c(1, 2), c(length(x), length(z)))
    d <- as.matrix(stats::dist(y))
    n <- nrow(d)
    total <- sum(d[upper.tri(d)]^2) / n
    within <- sum(vapply(split(seq_len(n), group), function(ii)
        sum(d[ii, ii][upper.tri(d[ii, ii])]^2) / length(ii),
        numeric(1)))
    (total - within) / (within / (n - 2L))
}

manual_holm <- function(p, m) {
    known <- !is.na(p)
    z <- ifelse(known, p, 1)
    ord <- order(z)
    adjusted <- numeric(length(z))
    adjusted[ord] <- pmin(1, cummax((m - seq_along(ord) + 1L) * z[ord]))
    adjusted[!known] <- NA_real_
    adjusted
}

# Reproduce the fixed seeded shuffleSet schedule that adonis2 consumes for
# a subset, then evaluate the permutation p by counting manual F statistics
# over that exact schedule.
scheduled_p <- function(x, z, seed=123, nperm=99) {
    y <- c(x, z)
    labels <- rep(c(1, 2), c(length(x), length(z)))
    observed <- pooled_f(x, z)
    set.seed(seed)
    schedule <- suppressMessages(permute::shuffleSet(
        length(y), control=permute::how(nperm=nperm)))
    count <- sum(vapply(seq_len(nrow(schedule)), function(i) {
        permuted <- y[schedule[i, ]]
        pooled_f(permuted[labels == 1],
            permuted[labels == 2]) >=
            observed - sqrt(.Machine$double.eps)
    }, logical(1)))
    (1 + count) / (nrow(schedule) + 1)
}

conditional_contrasts <- c(
    "A1 vs A2 (B: B1)", "A1 vs A3 (B: B1)", "A2 vs A3 (B: B1)",
    "A1 vs A2 (B: B2)", "A1 vs A3 (B: B2)", "A2 vs A3 (B: B2)")

test_that("conditional simple effects match independent manual oracles", {
    expect_equal(pooled_f(c(1, 2, 3, 5), c(6, 8, 9, 12)),
        15.7090909090909, tolerance=1e-9)
    expect_equal(pooled_f(c(1, 2, 3), c(5, 6, 8, 9, 12)),
        12.65625, tolerance=1e-9)
    expect_equal(distance_ss_f(c(1, 2, 3, 5), c(6, 8, 9, 12)),
        15.7090909090909, tolerance=1e-9)
    expect_equal(distance_ss_f(c(1, 2, 3), c(5, 6, 8, 9, 12)),
        12.65625, tolerance=1e-9)

    data <- conditional_effects_data()
    result <- run_conditional_effects()
    pairwise <- result$pairwise$asDF

    expect_true(result$table$visible)
    expect_false(result$guidance$visible)
    expect_true(result$pairwise$visible)
    expect_equal(nrow(pairwise), 6L)
    expect_identical(pairwise$contrast, conditional_contrasts)
    expect_equal(pairwise$f[[1L]], 15.7090909090909, tolerance=1e-9)
    expect_equal(pairwise$f[[4L]], 12.65625, tolerance=1e-9)

    pairsA <- utils::combn(c("A1", "A2", "A3"), 2, simplify=FALSE)
    subset_oracle <- function(oracle) {
        unlist(lapply(c("B1", "B2"), function(b)
            vapply(pairsA, function(pair)
                oracle(
                    data$y[data$B == b & data$A == pair[[1L]]],
                    data$y[data$B == b & data$A == pair[[2L]]]),
                numeric(1))))
    }
    expect_equal(pairwise$f, subset_oracle(pooled_f), tolerance=1e-9)
    expect_equal(pairwise$f, subset_oracle(distance_ss_f), tolerance=1e-9)
    expect_equal(pairwise$padj, manual_holm(pairwise$p, 6L), tolerance=1e-12)

    note <- miso_table_note(result$pairwise, "scope")
    expect_match(note,
        "Conditional simple-effect comparisons of A within each level of B.",
        fixed=TRUE)
    expect_match(note, "Holm-adjusted across all 6 planned comparisons",
        fixed=TRUE)
    expect_match(note, "subset-specific", fixed=TRUE)
    expect_match(note, "Free permutations", fixed=TRUE)
    expect_match(note, "not Type III", fixed=TRUE)
    expect_match(note, "not PRIMER pooled pairwise comparisons", fixed=TRUE)
    expect_match(note, "omnibus interaction test is separate", fixed=TRUE)
})

test_that("conditional p-values reproduce the fixed seeded schedule", {
    result <- run_conditional_effects()
    pairwise <- result$pairwise$asDF
    data <- conditional_effects_data()

    # Random-sampling regime (8-row subset): exact schedule reproduction
    # over the 99 rows shuffleSet returns for the fixed seed.
    expect_equal(scheduled_p(c(1, 2, 3, 5), c(6, 8, 9, 12)),
        pairwise$p[[1L]], tolerance=0)
    # The 7-row subset is also scored over its returned 99-row schedule,
    # not over all unique label allocations.
    expect_equal(scheduled_p(
            data$y[data$B == "B1" & data$A == "A1"],
            data$y[data$B == "B1" & data$A == "A3"]),
        pairwise$p[[2L]], tolerance=0)

    # Direct vegan subset parity with the reproduced schedule.
    set.seed(123)
    schedule <- suppressMessages(permute::shuffleSet(
        8, control=permute::how(nperm=99)))
    reference <- suppressWarnings(vegan::adonis2(
        stats::dist(c(1, 2, 3, 5, 6, 8, 9, 12)) ~ group,
        data=data.frame(group=factor(rep(c("a", "b"), c(4, 4)))),
        permutations=schedule,
        by="terms"))
    expect_equal(as.data.frame(reference)$`Pr(>F)`[[1L]],
        pairwise$p[[1L]], tolerance=0)
})

test_that("scheduled permutation oracle permutes responses not labels", {
    # vegan permutes responses and holds the group labels fixed, so the
    # manual oracle must apply the schedule to y under the original labels.
    data <- conditional_effects_data()
    data$y[1:8] <- c(1, 8, 3, 5, 6, 2, 9, 12)
    result <- run_conditional_effects(data=data)
    pairwise <- result$pairwise$asDF

    expect_identical(pairwise$contrast[[1L]], "A1 vs A2 (B: B1)")
    expect_equal(pairwise$p[[1L]], 0.38, tolerance=0)
    expect_equal(scheduled_p(data$y[1:4], data$y[5:8]), 0.38, tolerance=0)
})

test_that("missing subsets keep planned rows with family-wide Holm", {
    data <- conditional_effects_data()
    data$y[data$B == "B2" & data$A == "A3"] <- NA_real_
    result <- run_conditional_effects(data=data)
    pairwise <- result$pairwise$asDF

    expect_true(result$pairwise$visible)
    expect_equal(nrow(pairwise), 6L)
    expect_match(pairwise$contrast[[5L]],
        "A1 vs A3 (B: B2) (not estimated)", fixed=TRUE)
    expect_match(pairwise$contrast[[6L]],
        "A2 vs A3 (B: B2) (not estimated)", fixed=TRUE)
    expect_true(all(is.na(pairwise$f[c(5L, 6L)])))
    expect_true(all(is.na(pairwise$p[c(5L, 6L)])))
    expect_true(all(is.na(pairwise$padj[c(5L, 6L)])))
    expect_equal(sum(is.finite(pairwise$p)), 4L)
    expect_equal(
        pairwise$padj[is.finite(pairwise$padj)],
        manual_holm(pairwise$p, 6L)[is.finite(pairwise$padj)],
        tolerance=1e-12)

    warning <- conditional_warning_text(result)
    expect_match(warning, "4 rows excluded due to missing values", fixed=TRUE)
    expect_match(warning,
        "Conditional comparison A1 vs A3 (B: B2) was not estimated",
        fixed=TRUE)
    expect_match(warning,
        "each Grouping level needs at least two samples", fixed=TRUE)
})

test_that("degenerate subsets report reasons instead of being dropped", {
    zeroDistance <- data.frame(
        y=c(2, 2, 2, 2, 1, 2, 3, 4, 5, 6),
        A=factor(c("A1", "A1", "A2", "A2",
                   "A1", "A1", "A1", "A2", "A2", "A2")),
        B=factor(c("B1", "B1", "B1", "B1", rep("B2", 6))))
    result <- run_conditional_effects(data=zeroDistance)
    pairwise <- result$pairwise$asDF
    expect_equal(nrow(pairwise), 2L)
    expect_match(pairwise$contrast[[1L]], "not estimated", fixed=TRUE)
    expect_true(is.na(pairwise$f[[1L]]))
    expect_false(grepl("not estimated", pairwise$contrast[[2L]]))
    expect_true(is.finite(pairwise$f[[2L]]))
    expect_match(conditional_warning_text(result),
        "subset distances are all zero", fixed=TRUE)

    singletonGroup <- data.frame(
        y=c(1, 5, 6, 1, 2, 3, 6, 7, 8),
        A=factor(c("A1", "A2", "A2",
                   "A1", "A1", "A1", "A2", "A2", "A2")),
        B=factor(c("B1", "B1", "B1", rep("B2", 6))))
    tiny <- run_conditional_effects(data=singletonGroup)
    tinyPairwise <- tiny$pairwise$asDF
    expect_equal(nrow(tinyPairwise), 2L)
    expect_match(tinyPairwise$contrast[[1L]], "not estimated", fixed=TRUE)
    expect_match(conditional_warning_text(tiny),
        "each Grouping level needs at least two samples", fixed=TRUE)
})

test_that("unsupported conditional settings preserve the main result", {
    data <- conditional_effects_data()
    unsupported <- list(
        sequentialTerms=list(permBy="terms"),
        omnibusTestType=list(permBy="omnibus"),
        bonferroniAdjustment=list(permAdjust="bonferroni"),
        seriesPermutations=list(permScheme="series"),
        assignedBlocking=list(strata="B"),
        requestedCovariates=list(covariates="y"),
        twoAdditionalFactors=list(permFactors=c("B", "A")),
        binaryDistances=list(distBinary=TRUE),
        sqrtDistances=list(distSqrt=TRUE),
        additiveConstant=list(distAdd="cailliez"),
        squareRootTransform=list(transform="sqrt"),
        jaccardDistance=list(distance="jaccard"),
        brayWithoutFourthRoot=list(distance="bray", transform="none"),
        euclideanWithFourthRoot=list(distance="euclidean", transform="fourthroot"))

    for (name in names(unsupported)) {
        result <- do.call(run_conditional_effects,
            c(list(data=data), unsupported[[name]]))
        warning <- conditional_warning_text(result)
        expect_true(result$table$visible, info=name)
        expect_true(nrow(result$table$asDF) > 0L, info=name)
        expect_false(result$pairwise$visible, info=name)
        expect_equal(nrow(result$pairwise$asDF), 0L, info=name)
        expect_match(warning, "conditional simple-effect tests",
            fixed=TRUE, info=name)
        expect_match(warning, "require Test type Marginal terms",
            fixed=TRUE, info=name)
        expect_match(warning, "The main PERMANOVA results are unchanged.",
            fixed=TRUE, info=name)
    }

    # Fourth-root Bray-Curtis stays a supported conditional distance mode.
    supported <- run_conditional_effects(
        distance="bray", transform="fourthroot")
    expect_true(supported$pairwise$visible)
    expect_equal(nrow(supported$pairwise$asDF), 6L)
})

test_that("retained-factor guard rejects dropped and duplicated factors", {
    data <- conditional_effects_data()
    data$constantB <- factor(rep("only", nrow(data)))

    dropped <- run_conditional_effects(data=data, permFactors="constantB")
    expect_match(conditional_warning_text(dropped),
        "'constantB' was not retained in the fitted model", fixed=TRUE)
    expect_false(dropped$pairwise$visible)

    duplicatedFactor <- run_conditional_effects(
        data=data, permFactors=c("B", "constantB"))
    expect_match(conditional_warning_text(duplicatedFactor),
        "2 Additional factors are selected", fixed=TRUE)
    expect_false(duplicatedFactor$pairwise$visible)

    # A requested covariate is rejected even when cleaning removed it from
    # the fitted model (y is a Feature variable, so it is dropped as a covariate).
    cleanedAway <- run_conditional_effects(data=data, covariates="y")
    expect_match(conditional_warning_text(cleanedAway),
        "Continuous covariates are assigned", fixed=TRUE)
    expect_false(cleanedAway$pairwise$visible)
})

test_that("conditional state clears across valid, unsupported, and off runs", {
    options <- do.call(
        permanovaOptions$new, conditional_effects_options())
    analysis <- permanovaClass$new(
        options=options, data=conditional_effects_data())
    suppressWarnings(suppressMessages(analysis$run()))
    expect_true(analysis$results$pairwise$visible)
    expect_equal(
        unlist(analysis$results$pairwise$rowKeys), as.character(1:6))

    adjust <- options$option("permAdjust")
    adjust$.__enclos_env__$private$.value <- "bonferroni"
    suppressWarnings(suppressMessages(analysis$run()))
    expect_false(analysis$results$pairwise$visible)
    expect_equal(length(analysis$results$pairwise$rowKeys), 0L)
    expect_match(miso_squish_result(analysis$results$warnings),
        "P-value adjustment is Bonferroni", fixed=TRUE)

    adjust$.__enclos_env__$private$.value <- "holm"
    pairwiseOption <- options$option("permPairwise")
    pairwiseOption$.__enclos_env__$private$.value <- FALSE
    suppressWarnings(suppressMessages(analysis$run()))
    expect_false(analysis$results$pairwise$visible)
    expect_equal(length(analysis$results$pairwise$rowKeys), 0L)
    scopeNote <- miso_table_note(analysis$results$pairwise, "scope")
    expect_true(is.null(scopeNote) || !nzchar(scopeNote))
    expect_false(grepl("not estimated",
        miso_squish_result(analysis$results$warnings)))
})

test_that("conditional comparisons do not depend on interaction significance", {
    # Pure main-effect structure: the data contain no A:B interaction.
    data <- data.frame(
        y=c(1.0, 1.1, 0.9, 1.2, 1.0, 1.1, 1.2,
            2.0, 2.1, 1.9, 2.2, 2.0, 2.1, 1.9, 2.2, 2.1,
            3.0, 3.1, 2.9, 3.0, 3.1, 2.9, 3.2),
        A=factor(c(rep("A1", 7), rep("A2", 9), rep("A3", 7))),
        B=factor(c(rep("B1", 4), rep("B2", 3),
                   rep("B1", 4), rep("B2", 5),
                   rep("B1", 3), rep("B2", 4))))
    result <- run_conditional_effects(data=data)
    pairwise <- result$pairwise$asDF

    expect_equal(nrow(pairwise), 6L)
    expect_false(any(grepl("not estimated", pairwise$contrast)))
    interactionRow <- match("A:B", result$table$asDF$source)
    expect_false(is.na(interactionRow))
    expect_gt(result$table$asDF$p[[interactionRow]], 0.05)
})

test_that("noninteraction pairwise workflows and seed notes are unchanged", {
    legacy <- suppressWarnings(suppressMessages(permanova(
        data=conditional_effects_data(),
        vars="y",
        factor="A",
        permPairwise=TRUE,
        permBy="margin",
        distance="euclidean",
        transform="none",
        permN=99,
        seed=123)))
    pairwise <- legacy$pairwise$asDF

    expect_true(legacy$pairwise$visible)
    expect_equal(nrow(pairwise), 3L)
    expect_identical(pairwise$contrast, c("A1 vs A2", "A1 vs A3", "A2 vs A3"))
    expect_equal(pairwise$padj,
        stats::p.adjust(pairwise$p, method="holm"), tolerance=1e-12)
    expect_match(miso_table_note(legacy$pairwise, "scope"),
        "P-value adjustment: Holm across 3 available contrasts", fixed=TRUE)
    expect_match(miso_table_note(legacy$table, "seed"),
        "Random seed: 123 (fixed)", fixed=TRUE)
})

test_that("conditional results are deterministic and row-order invariant in F", {
    first <- run_conditional_effects()
    second <- run_conditional_effects()
    expect_identical(first$pairwise$asDF, second$pairwise$asDF)
    expect_identical(first$table$asDF, second$table$asDF)

    data <- conditional_effects_data()
    set.seed(7)
    reordered <- data[sample(nrow(data)), ]
    result <- run_conditional_effects(data=reordered)
    expect_identical(result$pairwise$asDF$contrast, conditional_contrasts)
    expect_equal(result$pairwise$asDF$f, first$pairwise$asDF$f, tolerance=1e-9)
    expect_equal(nrow(result$pairwise$asDF), 6L)
})
