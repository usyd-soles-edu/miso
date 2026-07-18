test_that("tofu uses one flat task-labelled analysis menu", {
    module <- yaml::read_yaml(test_path("..", "..", "jamovi", "0000.yaml"))
    analyses <- module$analyses

    expect_identical(
        vapply(analyses, `[[`, character(1), "name"),
        c("permanova", "anosim", "permdisp", "nmds", "simper")
    )
    expect_identical(
        vapply(analyses, `[[`, character(1), "menuTitle"),
        c("PERMANOVA", "ANOSIM", "PERMDISP", "nMDS", "SIMPER")
    )
    expect_identical(
        vapply(analyses, `[[`, character(1), "menuSubtitle"),
        c(
            "Test group differences",
            "Rank-based alternative",
            "Check group dispersion",
            "Visualise sample patterns",
            "Feature contributions"
        )
    )
    expect_true(all(vapply(analyses, function(analysis) is.null(analysis$menuSubgroup), logical(1))))
})
