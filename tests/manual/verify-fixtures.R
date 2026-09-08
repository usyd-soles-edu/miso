#!/usr/bin/env Rscript

script_path <- function() {
    arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
    if (length(arg) != 1L)
        stop("Cannot determine verify-fixtures.R location.")
    normalizePath(sub("^--file=", "", arg[[1L]]), mustWork = TRUE)
}

root <- normalizePath(file.path(dirname(script_path()), "..", ".."), mustWork = TRUE)
manual_dir <- file.path(root, "tests", "manual")
expected_arg <- grep("^--expected-dir=", commandArgs(trailingOnly = TRUE), value = TRUE)
if (length(expected_arg) > 1L)
    stop("Use only one --expected-dir argument.")
expected_dir <- if (length(expected_arg) == 1L) {
    normalizePath(sub("^--expected-dir=", "", expected_arg[[1L]]), mustWork = TRUE)
} else {
    manual_dir
}
tmp <- tempfile("miso-manual-")
dir.create(tmp, recursive = TRUE)
on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

old_wd <- setwd(root)
on.exit(setwd(old_wd), add = TRUE)

run_script <- function(script, output_dir) {
    status <- system2(
        file.path(R.home("bin"), "Rscript"),
        c(
            shQuote(file.path(manual_dir, script)),
            shQuote(paste0("--output-dir=", output_dir))
        )
    )
    if (!identical(status, 0L))
        stop(script, " failed with exit status ", status, ".")
}

file_bytes <- function(path) {
    size <- file.info(path)$size
    readBin(path, what = "raw", n = size)
}

same_file <- function(expected, actual) {
    isTRUE(file.exists(expected)) &&
        isTRUE(file.exists(actual)) &&
        identical(file_bytes(expected), file_bytes(actual))
}

same_session_info <- function(expected, actual) {
    if (!isTRUE(file.exists(expected)) || !isTRUE(file.exists(actual)))
        return(FALSE)

    expected_lines <- readLines(expected, warn = FALSE)
    actual_lines <- readLines(actual, warn = FALSE)
    commit_pattern <- "^Source-commit: [0-9a-f]{40}$"

    if (sum(grepl(commit_pattern, expected_lines)) != 1L ||
            sum(grepl(commit_pattern, actual_lines)) != 1L)
        return(FALSE)

    normalize_commit <- function(lines) {
        sub(commit_pattern, "Source-commit: <verified separately>", lines)
    }

    identical(
        normalize_commit(expected_lines),
        normalize_commit(actual_lines))
}

run_script("generate-datasets.R", tmp)
run_script("generate-reference-results.R", tmp)

files <- c(
    "miso-small.csv",
    "miso-large.csv",
    "miso-invalid.csv",
    "reference-results.csv",
    "reference-session-info.txt"
)

passed <- vapply(files, function(name) {
    expected <- file.path(expected_dir, name)
    actual <- file.path(tmp, name)
    ok <- if (identical(name, "reference-session-info.txt")) {
        same_session_info(expected, actual)
    } else {
        same_file(expected, actual)
    }
    cat(sprintf("%-34s %s\n", name, if (ok) "PASS" else "FAIL"))
    ok
}, logical(1))

if (!all(passed)) {
    failed <- paste(names(passed)[!passed], collapse = ", ")
    stop("Manual-test fixture drift detected: ", failed, call. = FALSE)
}

cat("fixture drift verification: PASS\n")
