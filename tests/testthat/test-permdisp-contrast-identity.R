permdisp_contrast_data <- function(labels) {
    data.frame(
        sp1=c(1, 2, 4, 5, 3, 5, 9, 10, 2, 7, 8, 11, 4, 6, 13, 18),
        sp2=c(9, 7, 8, 12, 2, 3, 6, 4, 4, 8, 3, 7, 6, 9, 10, 15),
        sp3=c(3, 5, 2, 8, 6, 7, 3, 1, 9, 2, 7, 5, 8, 4, 6, 12),
        group=factor(rep(labels[c(3, 1, 4, 2)], each=4),
            levels=c(labels, "Unused")))
}

test_that("PERMDISP preserves pair identities and numerical order for arbitrary group labels", {
    cases <- list(
        ordinary=list(
            groups=c("D", "B", "A", "C"),
            contrasts=c("D vs B", "D vs A", "D vs C", "B vs A",
                "B vs C", "A vs C")),
        hyphens=list(
            groups=c("A", "B-C", "A-B", "C"),
            contrasts=c("A vs B-C", "A vs A-B", "A vs C", "B-C vs A-B",
                "B-C vs C", "A-B vs C")),
        ambiguous=list(
            groups=c("A", "B vs C", "A vs B", "C"),
            contrasts=c("“A” vs “B vs C”", "“A” vs “A vs B”", "A vs C",
                "“B vs C” vs “A vs B”", "“B vs C” vs “C”",
                "“A vs B” vs “C”")))

    for (name in names(cases)) {
        case <- cases[[name]]
        data <- permdisp_contrast_data(case$groups)
        result <- suppressWarnings(suppressMessages(permdisp(
            data=data, vars=c("sp1", "sp2", "sp3"), factor="group",
            distance="euclidean", transform="none", seed=123, permN=99,
            dispPairwise=TRUE, dispAdjust="holm")))
        actual_rng <- .Random.seed

        set.seed(123)
        fit <- vegan::betadisper(stats::dist(data[c("sp1", "sp2", "sp3")]),
            droplevels(data$group), type="median")
        pairwise <- vegan::permutest(fit, permutations=permute::how(nperm=99),
            pairwise=TRUE)
        expected_rng <- .Random.seed
        actual <- result$pairwise$asDF

        expect_true(result$pairwise$visible, info=name)
        expect_identical(actual$contrast, case$contrasts, info=name)
        expect_identical(length(unique(actual$contrast)), 6L, info=name)
        expect_equal(actual$statistic, unname(pairwise$statistic[-1L]),
            tolerance=1e-10, info=name)
        expect_equal(actual$p, unname(pairwise$pairwise$permuted), info=name)
        expect_equal(actual$padj,
            unname(stats::p.adjust(pairwise$pairwise$permuted, method="holm")),
            info=name)
        expect_equal(result$anova$asDF$p[[1L]], pairwise$tab[1L, "Pr(>F)"],
            info=name)
        expect_identical(actual_rng, expected_rng, info=name)
    }
})
