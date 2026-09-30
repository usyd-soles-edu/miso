seed_contract_data <- function() {
    group <- factor(rep(c("A", "B", "C"), each=4L))
    index <- seq_along(group)
    group_index <- as.integer(group)
    data.frame(
        feature_a=2 + index %% 4L + group_index * 1.7,
        feature_b=3 + rev(index %% 5L) + group_index * 0.8,
        feature_c=1 + (index * 3L) %% 7L + group_index * 1.2,
        group=group)
}

seed_analysis <- function(name, useFixedSeed=TRUE, seed=0) {
    data <- seed_contract_data()
    vars <- c("feature_a", "feature_b", "feature_c")
    if (identical(name, "permanova")) {
        options <- permanovaOptions$new(vars=vars, factor="group",
            permN=11, seed=seed, useFixedSeed=useFixedSeed)
        analysis <- permanovaClass$new(options=options, data=data)
        table <- "table"
    }
    else if (identical(name, "anosim")) {
        options <- anosimOptions$new(vars=vars, factor="group",
            anosimN=11, seed=seed, useFixedSeed=useFixedSeed)
        analysis <- anosimClass$new(options=options, data=data)
        table <- "global"
    }
    else if (identical(name, "permdisp")) {
        options <- permdispOptions$new(vars=vars, factor="group",
            permN=11, seed=seed, useFixedSeed=useFixedSeed)
        analysis <- permdispClass$new(options=options, data=data)
        table <- "anova"
    }
    else if (identical(name, "simper")) {
        options <- simperOptions$new(vars=vars, factor="group",
            simperAssess=TRUE, simperN=11, seed=seed,
            useFixedSeed=useFixedSeed)
        analysis <- simperClass$new(options=options, data=data)
        table <- "contrasts"
    }
    else if (identical(name, "nmds")) {
        options <- nmdsOptions$new(vars=vars, seed=seed,
            useFixedSeed=useFixedSeed, nmdsTrymax=2)
        analysis <- nmdsClass$new(options=options, data=data)
        table <- "sites"
    }
    else {
        stop("Unknown seed analysis: ", name)
    }

    list(
        analysis=analysis,
        options=options,
        table=table,
        private=analysis$.__enclos_env__$private)
}

run_seed_analysis <- function(name, useFixedSeed=TRUE, seed=0) {
    result <- seed_analysis(name, useFixedSeed=useFixedSeed, seed=seed)
    suppressWarnings(suppressMessages(result$analysis$run()))
    result
}

seed_table_cells <- function(table) {
    lapply(table$columns, function(column)
        column$.__enclos_env__$private$.cells)
}

seed_set_option <- function(state, name, value) {
    option <- state$options$option(name)
    option$value <- value
    state$analysis$optionsChangedHandler(name)
}

seed_rerun <- function(state) {
    suppressWarnings(suppressMessages(state$analysis$run()))
}

test_that("fixed-seed options preserve the R API and legacy positive values", {
    optionClasses <- list(
        permanova=permanovaOptions,
        anosim=anosimOptions,
        permdisp=permdispOptions,
        simper=simperOptions,
        nmds=nmdsOptions)

    for (name in names(optionClasses)) {
        optionClass <- optionClasses[[name]]
        fresh <- optionClass$new()
        expect_true(fresh$useFixedSeed, info=name)
        expect_identical(fresh$seed, 0, info=name)
        expect_identical(
            miso_effective_seed(fresh$useFixedSeed, fresh$seed),
            0L, info=name)

        oldPositive <- optionClass$new(seed=123)
        expect_true(oldPositive$useFixedSeed, info=name)
        expect_identical(
            miso_effective_seed(oldPositive$useFixedSeed, oldPositive$seed),
            123L, info=name)

        explicitRandom <- optionClass$new(seed=123, useFixedSeed=FALSE)
        expect_identical(
            miso_effective_seed(
                explicitRandom$useFixedSeed, explicitRandom$seed),
            0L, info=name)

        savedValues <- as.list(oldPositive$values())
        expect_true("useFixedSeed" %in% names(savedValues), info=name)
        expect_identical(savedValues$useFixedSeed, TRUE, info=name)
        expect_identical(savedValues$seed, 123, info=name)
    }

    # Old saved option payloads do not contain the new marker. Its TRUE
    # constructor default preserves their stored positive seed.
    oldPayload <- jsonlite::fromJSON(
        '{"vars":["feature_a","feature_b"],"seed":321}',
        simplifyVector=FALSE)
    restored <- do.call(nmdsOptions$new, oldPayload)
    expect_true(restored$useFixedSeed)
    expect_identical(
        miso_effective_seed(restored$useFixedSeed, restored$seed), 321L)
})

test_that("effective fixed-seed validation rejects invalid active values only", {
    expect_identical(miso_effective_seed(TRUE, 1), 1L)
    expect_identical(miso_effective_seed(TRUE, .Machine$integer.max),
        .Machine$integer.max)
    for (value in c(-1, 1.5, Inf, .Machine$integer.max + 1))
        expect_error(miso_effective_seed(TRUE, value), "fixed random seed")
    expect_error(miso_effective_seed(TRUE, NA_real_), "fixed random seed")

    expect_identical(miso_effective_seed(FALSE, 1.5), 0L)
    expect_identical(miso_effective_seed(FALSE, Inf), 0L)
    expect_identical(miso_effective_seed(TRUE, 1.5, enabled=FALSE), 0L)
})

test_that("seed option changes use the effective seed cache path", {
    for (name in c("permanova", "anosim", "permdisp", "simper", "nmds")) {
        set.seed(11903)
        state <- run_seed_analysis(name)
        key <- state$private$.lastStructuralKey
        table <- state$analysis$results[[state$table]]
        cells <- seed_table_cells(table)
        values <- table$asDF
        rngAfterFit <- .Random.seed
        plotNames <- switch(name,
            permanova="companionPcoa", anosim="rankPlot",
            permdisp=c("plot", "ordinationPlot"), simper="heatmap",
            nmds=c("ordination", "shepard"))
        for (plotName in plotNames)
            state$analysis$results[[plotName]]$.setPath("retained-image.svg")

        # The load-time checked+0 compatibility default is random. Normalizing
        # it to unchecked, then editing a retained inactive value, is a cache
        # no-op and must not clear/rebuild the table or consume RNG.
        seed_set_option(state, "useFixedSeed", FALSE)
        seed_rerun(state)
        expect_identical(state$private$.lastStructuralKey, key, info=name)
        expect_identical(.Random.seed, rngAfterFit, info=name)

        seed_set_option(state, "seed", 407)
        seed_rerun(state)
        expect_identical(state$private$.lastStructuralKey, key, info=name)
        expect_identical(.Random.seed, rngAfterFit, info=name)
        expect_identical(table$asDF, values, info=name)
        expect_identical(seed_table_cells(table), cells, info=name)

        for (plotName in plotNames)
            expect_true(state$analysis$results[[plotName]]$isFilled(),
                info=paste(name, plotName, "inactive seed keeps image"))

        # Activating the retained positive value changes the effective seed,
        # takes the structural fit path, and matches a fresh legacy-style
        # R call that supplies only seed=123.
        seed_set_option(state, "seed", 123)
        seed_set_option(state, "useFixedSeed", TRUE)
        seed_rerun(state)
        fixedKey <- state$private$.lastStructuralKey
        for (plotName in plotNames)
            expect_false(state$analysis$results[[plotName]]$isFilled(),
                info=paste(name, plotName, "true seed refit invalidates image"))
        expect_false(identical(fixedKey, key), info=name)
        freshFixed <- run_seed_analysis(name, seed=123)
        expect_equal(
            state$analysis$results[[state$table]]$asDF,
            freshFixed$analysis$results[[freshFixed$table]]$asDF,
            tolerance=0,
            info=name)

        # Changing back to random mode changes the key despite retaining 123.
        seed_set_option(state, "useFixedSeed", FALSE)
        seed_rerun(state)
        expect_false(identical(state$private$.lastStructuralKey, fixedKey),
            info=name)
    }
})

test_that("seed result invalidation is managed by effective backend keys", {
    for (name in c("permanova", "anosim", "permdisp", "simper", "nmds")) {
        resultFile <- file.path(
            "jamovi", paste0(name, ".r.yaml"))
        results <- yaml::read_yaml(miso_fixture_path(resultFile))$items
        walk <- function(node) {
            if (is.list(node)) {
                if (!is.null(node$clearWith))
                    expect_false(any(c("seed", "useFixedSeed") %in% node$clearWith),
                        info=paste(name,
                            if (is.null(node$name)) "result" else node$name))
                lapply(node, walk)
            }
        }
        walk(results)
    }
})

test_that("seed UI controllers distinguish load defaults from user changes", {
    node <- Sys.which("node")
    skip_if(!nzchar(node), "Node.js is unavailable for the UI controller test")
    script <- test_path("fixtures", "seed-controller.test.js")
    output <- system2(node, shQuote(script), stdout=TRUE, stderr=TRUE)
    status <- attr(output, "status")
    if (is.null(status))
        status <- 0L
    expect_identical(status, 0L, info=paste(output, collapse="\n"))
    expect_match(paste(output, collapse="\n"), "seed UI controller contracts pass")
})
