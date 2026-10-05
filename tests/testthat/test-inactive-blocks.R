inactive_block_data <- function() {
    i <- seq_len(18L)
    group <- factor(rep(c("A", "B", "C"), each=6L))
    data.frame(
        x=2 + (i * 3) %% 7 + as.integer(group),
        y=1 + (i * 5) %% 11 + 2 * as.integer(group),
        z=3 + (i * 7) %% 13,
        group=group,
        block=factor(rep(seq_len(6L), 3L)))
}

inactive_block_run <- function(name, data, ...) {
    args <- c(list(vars=c("x", "y", "z"), factor="group", seed=123), list(...))
    if (identical(name, "permanova")) {
        options <- do.call(permanovaOptions$new,
            c(args, list(permN=19, permPairwise=TRUE)))
        analysis <- permanovaClass$new(options=options, data=data)
    } else {
        options <- do.call(anosimOptions$new,
            c(args, list(anosimN=19, anosimPairwise=TRUE)))
        analysis <- anosimClass$new(options=options, data=data)
    }
    suppressWarnings(suppressMessages(analysis$run()))
    analysis
}

expect_inactive_block_same_results <- function(actual, expected, name) {
    main <- if (identical(name, "permanova")) "table" else "global"
    expect_false(actual$results$guidance$visible, info=name)
    expect_equal(actual$results[[main]]$asDF, expected$results[[main]]$asDF,
        tolerance=0, info=name)
    expect_equal(actual$results$pairwise$asDF, expected$results$pairwise$asDF,
        tolerance=0, info=name)
}

test_that("Free permutations ignore missing inactive blocks in both analyses", {
    data <- inactive_block_data()
    for (missing in list(c(2L, 11L), seq_len(nrow(data)))) {
        data$block[missing] <- NA
        for (name in c("permanova", "anosim")) {
            expected <- inactive_block_run(name, data)
            actual <- inactive_block_run(name, data, strata="block")
            expect_inactive_block_same_results(actual, expected, name)
            warning <- miso_squish_result(actual$results$warnings)
            expect_match(warning,
                "Blocking variable 'block' is assigned but not used with Free permutations.",
                fixed=TRUE)
            expect_false(grepl("excluded due to missing", warning, fixed=TRUE))
        }
    }
})

test_that("active blocks still filter samples under Within blocks and Series", {
    data <- inactive_block_data()
    data$block[c(2L, 11L)] <- NA
    complete <- data[!is.na(data$block), , drop=FALSE]
    for (name in c("permanova", "anosim")) {
        for (scheme in c("stratified", "series")) {
            setting <- setNames(list(scheme),
                if (identical(name, "permanova")) "permScheme" else "permRestriction")
            actual <- do.call(inactive_block_run,
                c(list(name=name, data=data, strata="block"), setting))
            expected <- do.call(inactive_block_run,
                c(list(name=name, data=complete, strata="block"), setting))
            expect_inactive_block_same_results(actual, expected, name)
            expect_match(miso_squish_result(actual$results$warnings),
                "2 rows excluded due to missing values", fixed=TRUE)
        }
    }
})

test_that("ANOSIM legacy restrictions and current-setting precedence are retained", {
    data <- inactive_block_data()
    data$block[c(2L, 11L)] <- NA
    for (scheme in c("stratified", "series")) {
        modern <- inactive_block_run("anosim", data, strata="block",
            permRestriction=scheme)
        legacy <- inactive_block_run("anosim", data, strata="block",
            permScheme=scheme)
        expect_inactive_block_same_results(legacy, modern, "anosim")
        current <- inactive_block_run("anosim", data, strata="block",
            permRestriction=scheme,
            permScheme=if (scheme == "stratified") "series" else "stratified")
        expect_inactive_block_same_results(current, modern, "anosim")
    }
    fallback <- inactive_block_run("anosim", data, permScheme="stratified")
    free <- inactive_block_run("anosim", data)
    expect_inactive_block_same_results(fallback, free, "anosim")
    expect_match(miso_squish_result(fallback$results$warnings),
        "Saved Stratified permutations without a blocking variable", fixed=TRUE)
})

test_that("an inactive PERMANOVA block does not suppress an active model role", {
    data <- inactive_block_data()
    data$depth <- seq_len(nrow(data)) / 2
    data$depth[c(2L, 11L)] <- NA
    expected <- inactive_block_run("permanova", data, covariates="depth")
    actual <- inactive_block_run("permanova", data,
        covariates="depth", strata="depth")
    expect_inactive_block_same_results(actual, expected, "permanova")
    expect_true("depth" %in% actual$results$table$asDF$source)
    expect_match(miso_squish_result(actual$results$warnings),
        "2 rows excluded due to missing values", fixed=TRUE)

    data$block[c(2L, 11L)] <- NA
    expected <- inactive_block_run("permanova", data, permFactors="block")
    actual <- inactive_block_run("permanova", data,
        permFactors="block", strata="block")
    expect_inactive_block_same_results(actual, expected, "permanova")
})

test_that("the PERMANOVA R API preserves numeric covariates assigned as inactive blocks", {
    data <- inactive_block_data()
    data$depth <- seq_len(nrow(data)) / 2
    data$depth[c(2L, 11L)] <- NA
    run <- function(...) suppressWarnings(suppressMessages(permanova(
        data=data, vars=c("x", "y", "z"), factor="group",
        covariates="depth", seed=123, permN=19, permPairwise=TRUE, ...)))
    expected <- run()
    actual <- run(strata="depth")
    expect_false(actual$guidance$visible)
    expect_equal(actual$table$asDF, expected$table$asDF, tolerance=0)
    expect_equal(actual$pairwise$asDF, expected$pairwise$asDF, tolerance=0)
    expect_true("depth" %in% actual$table$asDF$source)
    expect_match(miso_squish_result(actual$warnings),
        "2 rows excluded due to missing values", fixed=TRUE)
})

test_that("numeric and factor block codes retain the same active restrictions", {
    numericData <- inactive_block_data()
    numericData$block <- as.integer(numericData$block) / 2
    numericData$block[c(2L, 11L)] <- NA
    factorData <- numericData
    factorData$block <- factor(factorData$block)
    for (name in c("permanova", "anosim")) {
        for (scheme in c("stratified", "series")) {
            args <- if (name == "permanova")
                list(permN=19, permPairwise=TRUE, permScheme=scheme) else
                list(anosimN=19, anosimPairwise=TRUE, permRestriction=scheme)
            run <- function(data) suppressWarnings(suppressMessages(do.call(
                get(name), c(list(data=data, vars=c("x", "y", "z"),
                    factor="group", strata="block", seed=123), args))))
            actual <- run(numericData)
            expected <- run(factorData)
            main <- if (name == "permanova") "table" else "global"
            expect_false(actual$guidance$visible)
            expect_equal(actual[[main]]$asDF, expected[[main]]$asDF, tolerance=0)
            expect_equal(actual$pairwise$asDF, expected$pairwise$asDF, tolerance=0)
        }
    }
})

test_that("the R APIs preserve decimal features also assigned as inactive blocks", {
    data <- inactive_block_data()
    data$x <- data$x / 3
    for (name in c("permanova", "anosim")) {
        args <- if (name == "permanova")
            list(permN=19, permPairwise=TRUE) else
            list(anosimN=19, anosimPairwise=TRUE)
        run <- function(...) suppressWarnings(suppressMessages(do.call(
            get(name), c(list(data=data, vars=c("x", "y", "z"),
                factor="group", seed=123), args, list(...)))))
        expected <- run()
        actual <- run(strata="x")
        main <- if (name == "permanova") "table" else "global"
        expect_false(actual$guidance$visible)
        expect_equal(actual[[main]]$asDF, expected[[main]]$asDF, tolerance=0)
        expect_equal(actual$pairwise$asDF, expected$pairwise$asDF, tolerance=0)
    }
})

test_that("switching block restrictions refreshes the analysed sample", {
    data <- inactive_block_data()
    data$block[c(2L, 11L)] <- NA
    for (name in c("permanova", "anosim")) {
        analysis <- inactive_block_run(name, data, strata="block")
        optionName <- if (name == "permanova") "permScheme" else "permRestriction"
        for (scheme in c("stratified", "free", "series", "free")) {
            option <- analysis$options$option(optionName)
            option$value <- scheme
            analysis$optionsChangedHandler(optionName)
            suppressWarnings(suppressMessages(analysis$run()))
            expected <- do.call(inactive_block_run,
                c(list(name=name, data=data, strata="block"),
                    setNames(list(scheme), optionName)))
            expect_inactive_block_same_results(analysis, expected, name)
        }
    }
})
