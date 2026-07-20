test_that("source contract fixtures match the development checkout", {
    sourceRoot <- test_path("..", "..")
    fixtureRoot <- test_path("fixtures")
    sourceFiles <- list.files(
        file.path(fixtureRoot, "jamovi"),
        recursive=TRUE,
        full.names=FALSE)

    if (! file.exists(file.path(sourceRoot, "jamovi", "0000.yaml")))
        skip("development source files are not present in the package-check tree")

    for (sourceFile in sourceFiles) {
        expect_identical(
            readBin(file.path(fixtureRoot, "jamovi", sourceFile), "raw", n=1e7),
            readBin(file.path(sourceRoot, "jamovi", sourceFile), "raw", n=1e7),
            info=sourceFile)
    }
})
