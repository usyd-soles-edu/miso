# Run from the repository root. CI must exercise saved results and controllers,
# rather than report success after skipping unavailable dependencies.
required <- c("pkgload", "testthat", "RProtoBuf")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly=TRUE)]
if (length(missing) > 0L)
    stop("Missing CI dependencies: ", paste(missing, collapse=", "), call.=FALSE)
if (!nzchar(Sys.which("node")))
    stop("Node.js is required for the controller tests.", call.=FALSE)

results <- testthat::test_local(".", reporter="summary", stop_on_failure=TRUE)
report <- as.data.frame(results)
if (any(report$skipped)) {
    writeLines(paste(report$file[report$skipped], report$test[report$skipped], sep=": "))
    stop("CI requires every test to run; skipped tests are failures.", call.=FALSE)
}
cat(sprintf("\nPassed %d assertions across %d tests with no skips.\n",
    sum(report$passed), nrow(report)))
