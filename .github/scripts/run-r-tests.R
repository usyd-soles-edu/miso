# Run from the repository root. CI must exercise saved results and controllers,
# rather than report success after skipping unavailable dependencies.
required <- c("pkgload", "testthat", "RProtoBuf", "jmvcore")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly=TRUE)]
if (length(missing) > 0L)
    stop("Missing CI dependencies: ", paste(missing, collapse=", "), call.=FALSE)
if (!nzchar(Sys.which("node")))
    stop("Node.js is required for the controller tests.", call.=FALSE)

helpers <- c("RProtoBuf_new", "RProtoBuf_read", "RProtoBuf_serialize")
runtime <- asNamespace("jmvcore")
available <- vapply(helpers, function(name)
    is.function(get0(name, envir=runtime, inherits=FALSE)), logical(1))
if (!all(available))
    stop(paste("jmvcore was built without saved-result support.",
        "Reinstall jmvcore from source with RProtoBuf already installed."), call.=FALSE)

results <- testthat::test_local(".", reporter="summary", stop_on_failure=TRUE)
report <- as.data.frame(results)
if (any(report$skipped)) {
    writeLines(paste(report$file[report$skipped], report$test[report$skipped], sep=": "))
    stop("CI requires every test to run; skipped tests are failures.", call.=FALSE)
}
cat(sprintf("\nPassed %d assertions across %d tests with no skips.\n",
    sum(report$passed), nrow(report)))
