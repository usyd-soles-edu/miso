test_that("ANOSIM retains all contrasts when group names contain the separator", {
    labels <- c("A", "B vs C", "A vs B", "C")
    data <- data.frame(
        x=c(1, 2, 4, 6, 9, 12, 15, 20, 23, 27, 30, 32),
        y=c(4, 5, 9, 7, 3, 8, 2, 6, 12, 11, 20, 21),
        group=factor(rep(labels, each=3), levels=labels))
    result <- suppressWarnings(suppressMessages(anosim(
        data=data, vars=c("x", "y"), factor="group",
        anosimPairwise=TRUE, anosimN=19, seed=123)))
    actual <- result$pairwise$asDF
    pairs <- utils::combn(labels, 2L, simplify=FALSE)
    distance <- as.matrix(vegan::vegdist(data[c("x", "y")]))
    expected <- lapply(pairs, function(pair) {
        keep <- data$group %in% pair
        set.seed(123)
        fit <- suppressMessages(vegan::anosim(
            stats::as.dist(distance[keep, keep]), droplevels(data$group[keep]),
            permutations=permute::how(nperm=19)))
        c(r=unname(fit$statistic), p=fit$signif)
    })
    expected <- do.call(rbind, expected)

    expect_equal(nrow(actual), 6L)
    expect_identical(actual$contrast, c(
        "“A” vs “B vs C”", "“A” vs “A vs B”", "A vs C",
        "“B vs C” vs “A vs B”", "“B vs C” vs “C”", "“A vs B” vs “C”"))
    expect_equal(actual$r, expected[, "r"], tolerance=0)
    expect_equal(actual$p, expected[, "p"], tolerance=0)
    expect_equal(actual$padj, stats::p.adjust(expected[, "p"], "holm"), tolerance=0)
    expect_match(miso_table_note(result$pairwise, "scope"),
        "across 6 available contrasts", fixed=TRUE)
})

test_that("contrast labels distinguish literal quotes escapes and separators", {
    labels <- c("A", "B vs C", "A vs B", "C", "B” vs “C", "A” vs “B",
        'A"', "A\\", "A\n", "A\\n", "A“", "A\\u201c")
    pairs <- utils::combn(labels, 2L, simplify=FALSE)
    displayed <- vapply(pairs, miso_contrast_label, character(1))
    expect_equal(anyDuplicated(displayed), 0L)
    expect_identical(miso_contrast_label(c("A", "B")), "A vs B")
    expect_identical(miso_contrast_label(c("North-East", "South-West")),
        "North-East vs South-West")
})
