test_that("tofu uses one flat task-labelled analysis menu", {
    module <- yaml::read_yaml(tofu_fixture_path("jamovi", "0000.yaml"))
    analyses <- module$analyses

    expect_identical(
        vapply(analyses, `[[`, character(1), "name"),
        c("permanova", "anosim", "permdisp", "nmds", "pcoa", "cluster", "simper")
    )
    expect_identical(
        vapply(analyses, `[[`, character(1), "menuTitle"),
        c("PERMANOVA", "ANOSIM", "PERMDISP", "nMDS", "PCoA", "Cluster analysis", "SIMPER")
    )
    expect_identical(
        vapply(analyses, `[[`, character(1), "menuSubtitle"),
        c(
            "Test group differences",
            "Rank-based alternative",
            "Check group dispersion",
            "Visualise sample patterns",
            "Visualise distance structure",
            "Visualise sample similarity",
            "Feature contributions"
        )
    )
    expect_true(all(vapply(analyses, function(analysis) is.null(analysis$menuSubgroup), logical(1))))
})

test_that("every library analysis has a public description", {
    module <- yaml::read_yaml(tofu_fixture_path("jamovi", "0000.yaml"))

    descriptions <- vapply(
        module$analyses,
        function(analysis) analysis$description,
        character(1))

    expect_true(all(nzchar(descriptions)))
    expect_true(all(nchar(descriptions) <= 300L))
})
