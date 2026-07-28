plotHelperSource <- testthat::test_path("..", "..", "R", "plot-helpers.R")
if (file.exists(plotHelperSource)) {
    source(plotHelperSource, local=TRUE)
} else {
    for (name in c(
            ".tofuGroupAesthetics", ".tofuJitter", ".tofuShortLabel",
            ".tofuUniqueShortLabels", ".tofuPlotTheme",
            ".tofuPlotDisclosure"))
        assign(name, getFromNamespace(name, "tofu"))
}

test_that("ggplot2 is an imported package dependency", {
    descriptionPath <- testthat::test_path("..", "..", "DESCRIPTION")
    imports <- if (file.exists(descriptionPath)) {
        read.dcf(descriptionPath, fields="Imports")[[1L]]
    } else {
        packageDescription("tofu", fields="Imports")
    }

    expect_match(imports, "(^|[,[:space:]])ggplot2([,[:space:]]|$)")
})

test_that("shared group aesthetics are stable and redundant", {
    first <- .tofuGroupAesthetics(c("B", "A", "C", "A"))
    second <- .tofuGroupAesthetics(c("C", "B", "A"))

    expect_identical(first, second)
    expect_identical(names(first$colour), c("A", "B", "C"))
    expect_identical(names(first$shape), c("A", "B", "C"))
    expect_identical(names(first$linetype), c("A", "B", "C"))
    expect_length(first$colour, 3L)
    expect_length(first$shape, 3L)
    expect_length(first$linetype, 3L)
    expect_false(any(first$colour %in% c("#F0E442", "yellow")))
})

test_that("shared group aesthetics handle empty and extended group sets", {
    empty <- .tofuGroupAesthetics(character())
    expect_identical(empty$colour, setNames(character(), character()))
    expect_identical(empty$shape, setNames(integer(), character()))
    expect_identical(empty$linetype, setNames(character(), character()))

    groups <- sprintf("group-%02d", 12:1)
    aesthetics <- .tofuGroupAesthetics(groups)
    expect_identical(names(aesthetics$colour), sort(groups))
    expect_identical(names(aesthetics$shape), sort(groups))
    expect_length(aesthetics$colour, 12L)
    expect_length(aesthetics$shape, 12L)
    expect_identical(unname(aesthetics$colour[9L]), unname(aesthetics$colour[1L]))
    expect_false(identical(unname(aesthetics$shape[9L]), unname(aesthetics$shape[1L])))

    groups64 <- sprintf("group-%02d", 64:1)
    aesthetics64 <- .tofuGroupAesthetics(groups64)
    pairs <- paste(aesthetics64$colour, aesthetics64$shape)
    expect_length(unique(pairs), 64L)
    linePairs <- paste(aesthetics64$colour, aesthetics64$linetype)
    expect_length(unique(linePairs), 64L)
    expect_length(unique(aesthetics64$linetype), 8L)

    expect_error(
        .tofuGroupAesthetics(sprintf("group-%02d", 65:1)),
        "at most 64 unique groups"
    )
})

test_that("jitter is deterministic and preserves an existing RNG state", {
    hadSeed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    if (hadSeed)
        oldSeed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    on.exit({
        if (hadSeed)
            assign(".Random.seed", oldSeed, envir = .GlobalEnv)
        else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
            rm(".Random.seed", envir = .GlobalEnv)
    }, add = TRUE)

    set.seed(42L)
    before <- .Random.seed
    first <- .tofuJitter(12L)

    expect_identical(.Random.seed, before)
    expect_identical(first, .tofuJitter(12L))
    expect_true(all(abs(first) <= 0.16))
    expect_identical(.Random.seed, before)
})

test_that("jitter does not leave an RNG state when none existed", {
    hadSeed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    if (hadSeed)
        oldSeed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    on.exit({
        if (hadSeed)
            assign(".Random.seed", oldSeed, envir = .GlobalEnv)
        else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
            rm(".Random.seed", envir = .GlobalEnv)
    }, add = TRUE)

    if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
        rm(".Random.seed", envir = .GlobalEnv)

    expect_length(.tofuJitter(0L), 0L)
    expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
    expect_length(.tofuJitter(4L, width = 0.25, seed = 7L), 4L)
    expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
})

test_that("short labels are vectorised and respect the requested width", {
    values <- c("short", "a very long feature label", NA_character_)
    shortened <- .tofuShortLabel(values, width = 12L)

    expect_identical(shortened[1L], values[1L])
    expect_identical(shortened[3L], NA_character_)
    expect_true(endsWith(shortened[2L], "…"))
    expect_true(all(nchar(shortened[!is.na(shortened)]) <= 12L))
    expect_identical(.tofuShortLabel(c("a", "long"), width = 1L), c("a", "…"))
    expect_error(
        .tofuShortLabel("label", width = Inf),
        "'width' must be one positive whole number",
        fixed = TRUE
    )
    expect_error(
        .tofuShortLabel("label", width = -Inf),
        "'width' must be one positive whole number",
        fixed = TRUE
    )
})

test_that("unique short labels preserve distinct identities after truncation", {
    shared <- paste(rep("same-prefix", 4L), collapse="")
    values <- c(
        paste0(shared, "-alpha"),
        paste0(shared, "-beta"),
        "ordinary",
        paste0(shared, "-alpha"))
    labels <- .tofuUniqueShortLabels(values, width=18L)

    expect_identical(names(labels), unique(values))
    expect_length(unique(unname(labels)), length(unique(values)))
    expect_true(all(nchar(labels) <= 18L))
    expect_identical(.tofuUniqueShortLabels("ordinary", 18L),
                     stats::setNames("ordinary", "ordinary"))
    expect_error(.tofuUniqueShortLabels(values, width=3L), "at least 4")
})

test_that("shared plot theme uses restrained publication settings", {
    theme <- .tofuPlotTheme()

    expect_s3_class(theme, "theme")
    expect_identical(theme$text$size, 12)
    expect_identical(theme$legend.position, "bottom")
    expect_identical(theme$legend.box, "vertical")
    expect_s3_class(theme$panel.grid.minor, "element_blank")
    expect_s3_class(theme$plot.title, "element_blank")
})

test_that("plot disclosure reports shown and omitted observations exactly", {
    expect_identical(
        .tofuPlotDisclosure(8L, 8L),
        "All 8 observations are shown."
    )
    expect_identical(
        .tofuPlotDisclosure(5L, 8L),
        paste0(
            "5 of 8 observations are shown; 3 are omitted from the image ",
            "but retained in summaries."
        )
    )
    expect_identical(
        .tofuPlotDisclosure(2L, 3L, noun = "labels"),
        paste0(
            "2 of 3 labels are shown; 1 is omitted from the image ",
            "but retained in summaries."
        )
    )
    expect_identical(
        .tofuPlotDisclosure(2L, 4L, noun = "labels"),
        paste0(
            "2 of 4 labels are shown; 2 are omitted from the image ",
            "but retained in summaries."
        )
    )
})

test_that("plot disclosure rejects invalid counts", {
    invalidCounts <- list(-1, NA_real_, 1.5, c(1, 2), Inf)

    for (value in invalidCounts) {
        expect_error(
            .tofuPlotDisclosure(value, 8L),
            "'shown' must be one finite non-negative whole number",
            fixed = TRUE
        )
        expect_error(
            .tofuPlotDisclosure(5L, value),
            "'total' must be one finite non-negative whole number",
            fixed = TRUE
        )
    }

    expect_error(
        .tofuPlotDisclosure(9L, 8L),
        "'shown' must not exceed 'total'",
        fixed = TRUE
    )
})
