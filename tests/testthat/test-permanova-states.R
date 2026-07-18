permanova_state_data <- function() {
    data.frame(
        sp1 = c(1, 2, 1, 7, 8, 7, 3, 4, 3),
        sp2 = c(2, 1, 2, 8, 7, 8, 4, 3, 4),
        sp3 = c(1, 1, 2, 6, 7, 6, 3, 3, 2),
        group = factor(rep(c("A", "B", "C"), each=3)),
        site = factor(rep(c("north", "south", "north"), times=3)),
        block = factor(rep(1:3, times=3)),
        depth = seq(1, 9)
    )
}

expect_result_visibility <- function(result, visible, hidden) {
    for (name in visible)
        expect_true(result[[name]]$visible, info=paste(name, "should be visible"))
    for (name in hidden)
        expect_false(result[[name]]$visible, info=paste(name, "should be hidden"))
}

test_that("new PERMANOVA shows only complete getting-started guidance", {
    result <- permanova(
        data=permanova_state_data(),
        vars=character(),
        factor="group"
    )

    expect_identical(
        trimws(as.character(result$guidance$asString())),
        paste(
            "To run PERMANOVA:",
            "1. Add one or more numeric Feature variables.",
            "2. Add one categorical Grouping variable with at least two groups.",
            "Results update automatically.",
            sep="\n"
        )
    )
    expect_result_visibility(
        result,
        visible="guidance",
        hidden=c("summary", "warnings", "table", "pairwise", "note", "settings")
    )
})

test_that("features without a group show one actionable correction", {
    result <- permanova(
        data=permanova_state_data(),
        vars=c("sp1", "sp2", "sp3"),
        factor=NULL
    )

    expect_identical(
        trimws(as.character(result$guidance$asString())),
        paste(
            "PERMANOVA is waiting for a Grouping variable.",
            "Add one categorical variable containing at least two groups."
        )
    )
    expect_result_visibility(
        result,
        visible="guidance",
        hidden=c("summary", "warnings", "table", "pairwise", "note", "settings")
    )
})

test_that("preparation failure hides all result shells", {
    data <- permanova_state_data()
    data$sp1[[1L]] <- -1
    result <- permanova(
        data=data,
        vars=c("sp1", "sp2", "sp3"),
        factor="group"
    )

    expect_match(as.character(result$guidance$asString()), "Negative values")
    expect_result_visibility(
        result,
        visible="guidance",
        hidden=c("summary", "warnings", "table", "pairwise", "note", "settings")
    )
})

test_that("model failure hides prepared but invalid output", {
    result <- permanova(
        data=data.frame(
            sp1=c(1, 2, 3),
            sp2=c(4, 5, 6),
            group=factor(c("A", "B", "C"))
        ),
        vars=c("sp1", "sp2"),
        factor="group",
        permN=9,
        seed=123
    )

    expect_match(as.character(result$guidance$asString()), "PERMANOVA could not run")
    expect_result_visibility(
        result,
        visible="guidance",
        hidden=c("summary", "warnings", "table", "pairwise", "note", "settings")
    )
})

test_that("standard success hides empty guidance warnings and pairwise output", {
    result <- suppressMessages(permanova(
        data=permanova_state_data(),
        vars=c("sp1", "sp2", "sp3"),
        factor="group",
        permN=19,
        seed=123
    ))

    expect_result_visibility(
        result,
        visible=c("summary", "table", "note", "settings"),
        hidden=c("guidance", "warnings", "pairwise")
    )
})

test_that("valid invalid valid transitions clear stale result state", {
    data <- permanova_state_data()
    options <- permanovaOptions$new(
        vars=c("sp1", "sp2", "sp3"),
        factor="group",
        permN=19,
        seed=123)
    analysis <- permanovaClass$new(options=options, data=data)

    analysis$run()
    expect_true(analysis$results$table$visible)
    expect_gt(length(analysis$results$table$rowKeys), 0L)

    factorOption <- options$option("factor")
    factorOption$.__enclos_env__$private$.value <- NULL
    analysis$run()
    expect_true(analysis$results$guidance$visible)
    expect_false(analysis$results$table$visible)
    expect_false(analysis$results$summary$visible)
    expect_equal(length(analysis$results$table$rowKeys), 0L)
    expect_equal(length(analysis$results$summary$rowKeys), 0L)

    factorOption$.__enclos_env__$private$.value <- "group"
    analysis$run()
    expect_false(analysis$results$guidance$visible)
    expect_true(analysis$results$table$visible)
    expect_gt(length(analysis$results$table$rowKeys), 0L)
})

test_that("pairwise on off transitions remove stale pairwise rows", {
    data <- permanova_state_data()
    options <- permanovaOptions$new(
        vars=c("sp1", "sp2", "sp3"),
        factor="group",
        permPairwise=TRUE,
        permN=19,
        seed=123)
    analysis <- permanovaClass$new(options=options, data=data)

    analysis$run()
    expect_true(analysis$results$pairwise$visible)
    expect_gt(length(analysis$results$pairwise$rowKeys), 0L)

    pairwiseOption <- options$option("permPairwise")
    pairwiseOption$.__enclos_env__$private$.value <- FALSE
    analysis$run()
    expect_false(analysis$results$pairwise$visible)
    expect_equal(length(analysis$results$pairwise$rowKeys), 0L)
})

test_that("successful preprocessing warnings appear with valid inference", {
    data <- permanova_state_data()
    data$sp1[[1L]] <- NA_real_
    result <- suppressMessages(permanova(
        data=data,
        vars=c("sp1", "sp2", "sp3"),
        factor="group",
        permN=19,
        seed=123
    ))

    expect_match(as.character(result$warnings$asString()), "rows excluded")
    expect_result_visibility(
        result,
        visible=c("summary", "warnings", "table", "note", "settings"),
        hidden=c("guidance", "pairwise")
    )
})

test_that("within-block permutations require a block", {
    result <- permanova(
        data=permanova_state_data(),
        vars=c("sp1", "sp2", "sp3"),
        factor="group",
        permScheme="stratified",
        permN=19,
        seed=123
    )

    expect_match(
        as.character(result$guidance$asString()),
        "Within-block permutations require a Blocking variable"
    )
    expect_result_visibility(
        result,
        visible="guidance",
        hidden=c("summary", "warnings", "table", "pairwise", "note", "settings")
    )
})

test_that("free permutations report an assigned block as unused", {
    result <- suppressMessages(permanova(
        data=permanova_state_data(),
        vars=c("sp1", "sp2", "sp3"),
        factor="group",
        strata="block",
        permScheme="free",
        permN=19,
        seed=123
    ))

    expect_match(as.character(result$warnings$asString()), "Blocking variable.*not used")
    settings <- setNames(result$settings$asDF$value, result$settings$asDF$setting)
    expect_identical(settings[["Requested permutation restrictions"]], "Free")
    expect_identical(settings[["Effective permutation restrictions"]], "Free")
    expect_identical(settings[["Block used"]], "No")
})

test_that("pairwise output is visible only when requested and populated", {
    standard <- suppressMessages(permanova(
        data=permanova_state_data(),
        vars=c("sp1", "sp2", "sp3"),
        factor="group",
        permN=19,
        seed=123
    ))
    pairwise <- suppressMessages(permanova(
        data=permanova_state_data(),
        vars=c("sp1", "sp2", "sp3"),
        factor="group",
        permPairwise=TRUE,
        permN=19,
        seed=123
    ))

    expect_false(standard$pairwise$visible)
    expect_true(pairwise$pairwise$visible)
    expect_gt(nrow(pairwise$pairwise$asDF), 0L)
})

test_that("pairwise conflicts preserve the main result and hide pairwise output", {
    interactions <- suppressMessages(permanova(
        data=permanova_state_data(),
        vars=c("sp1", "sp2", "sp3"),
        factor="group",
        permFactors="site",
        permInteractions=TRUE,
        permPairwise=TRUE,
        permN=19,
        seed=123
    ))
    omnibus <- suppressMessages(permanova(
        data=permanova_state_data(),
        vars=c("sp1", "sp2", "sp3"),
        factor="group",
        permBy="omnibus",
        permPairwise=TRUE,
        permN=19,
        seed=123
    ))

    expect_true(interactions$table$visible)
    expect_false(interactions$pairwise$visible)
    expect_match(as.character(interactions$warnings$asString()), "unavailable while interactions")
    expect_true(omnibus$table$visible)
    expect_false(omnibus$pairwise$visible)
    expect_match(as.character(omnibus$warnings$asString()), "Sequential terms or Marginal terms")
})

test_that("new analyses use Free permutations and report effective settings", {
    result <- suppressMessages(permanova(
        data=permanova_state_data(),
        vars=c("sp1", "sp2", "sp3"),
        factor="group",
        permN=19,
        seed=123
    ))
    settings <- setNames(result$settings$asDF$value, result$settings$asDF$setting)

    expect_identical(settings[["Requested permutation restrictions"]], "Free")
    expect_identical(settings[["Effective permutation restrictions"]], "Free")
    expect_identical(settings[["Blocking variable"]], "None")
    expect_identical(settings[["Block used"]], "No")
    expect_identical(settings[["Test type"]], "Sequential terms")
    expect_identical(settings[["Random seed"]], "123")
})

test_that("within-block validation identifies ineffective and singleton blocks", {
    ineffective <- permanova_state_data()
    ineffective$block <- ineffective$group
    failed <- permanova(
        data=ineffective,
        vars=c("sp1", "sp2", "sp3"),
        factor="group",
        strata="block",
        permScheme="stratified",
        permN=19,
        seed=123
    )

    expect_match(
        as.character(failed$guidance$asString()),
        "Grouping variable does not vary within any block")
    expect_false(failed$table$visible)

    singleton <- permanova_state_data()
    singleton$block <- factor(c("solo", "x", "y", "x", "y", "x", "y", "x", "y"))
    succeeded <- suppressMessages(permanova(
        data=singleton,
        vars=c("sp1", "sp2", "sp3"),
        factor="group",
        strata="block",
        permScheme="stratified",
        permN=19,
        seed=123
    ))

    expect_true(succeeded$table$visible)
    expect_match(as.character(succeeded$warnings$asString()), "single sample")
})

test_that("restricted analyses report too few unique permutations", {
    result <- suppressMessages(permanova(
        data=permanova_state_data(),
        vars=c("sp1", "sp2", "sp3"),
        factor="group",
        strata="block",
        permScheme="stratified",
        permN=999,
        seed=123
    ))

    expect_true(result$table$visible)
    expect_match(as.character(result$warnings$asString()), "unique permutations are available")
})

test_that("Series reports row-order restrictions with and without a block", {
    withoutBlock <- suppressMessages(permanova(
        data=permanova_state_data(),
        vars=c("sp1", "sp2", "sp3"),
        factor="group",
        permScheme="series",
        permN=19,
        seed=123
    ))
    withBlock <- suppressMessages(permanova(
        data=permanova_state_data(),
        vars=c("sp1", "sp2", "sp3"),
        factor="group",
        strata="block",
        permScheme="series",
        permN=7,
        seed=123
    ))
    withoutSettings <- setNames(
        withoutBlock$settings$asDF$value,
        withoutBlock$settings$asDF$setting)
    withSettings <- setNames(
        withBlock$settings$asDF$value,
        withBlock$settings$asDF$setting)

    expect_identical(withoutSettings[["Effective permutation restrictions"]], "Series")
    expect_identical(withoutSettings[["Sequence order"]], "Current data-row order")
    expect_identical(withoutSettings[["Block used"]], "No")
    expect_identical(withSettings[["Effective permutation restrictions"]], "Series")
    expect_identical(withSettings[["Sequence order"]], "Current data-row order")
    expect_identical(withSettings[["Block used"]], "Yes")
})

test_that("pairwise retains adjustment terms for Sequential and Marginal tests", {
    for (testType in c("terms", "margin")) {
        result <- suppressMessages(permanova(
            data=permanova_state_data(),
            vars=c("sp1", "sp2", "sp3"),
            factor="group",
            permFactors="site",
            covariates="depth",
            strata="block",
            permScheme="stratified",
            permBy=testType,
            permPairwise=TRUE,
            permN=7,
            seed=123
        ))

        expect_true(result$table$visible, info=testType)
        expect_true(result$pairwise$visible, info=testType)
        expect_equal(nrow(result$pairwise$asDF), 3L, info=testType)
        expect_true(all(is.finite(result$pairwise$asDF$f)), info=testType)
        expect_true(all(is.finite(result$pairwise$asDF$p)), info=testType)
        expect_match(
            result$pairwise$notes$scope$note,
            "Retained adjustment terms: site, depth",
            info=testType)
    }
})

test_that("partial pairwise failures keep successful comparisons", {
    data <- data.frame(
        sp1=c(1, 2, 7, 8, 7, 8),
        sp2=c(2, 1, 8, 7, 9, 8),
        sp3=c(1, 2, 6, 7, 8, 7),
        group=factor(c("A", "B", "C", "C", "C", "C"))
    )
    result <- suppressMessages(permanova(
        data=data,
        vars=c("sp1", "sp2", "sp3"),
        factor="group",
        permPairwise=TRUE,
        permN=19,
        seed=123
    ))

    expect_true(result$table$visible)
    expect_true(result$pairwise$visible)
    expect_equal(nrow(result$pairwise$asDF), 2L)
    expect_match(as.character(result$warnings$asString()), "A vs B could not be calculated")
})

test_that("all pairwise failures hide the pairwise table", {
    data <- data.frame(
        sp1=c(1, 2, 7, 8, 3, 4),
        sp2=c(2, 1, 8, 7, 4, 3),
        sp3=c(1, 2, 6, 7, 3, 2),
        group=factor(rep(c("A", "B", "C"), each=2)),
        cov1=rep(c(0, 1), 3),
        cov2=c(0, 1, 1, 0, 0, 2)
    )
    result <- suppressMessages(permanova(
        data=data,
        vars=c("sp1", "sp2", "sp3"),
        factor="group",
        covariates=c("cov1", "cov2"),
        permPairwise=TRUE,
        permN=19,
        seed=123
    ))

    expect_true(result$table$visible)
    expect_false(result$pairwise$visible)
    expect_equal(nrow(result$pairwise$asDF), 0L)
    expect_match(as.character(result$warnings$asString()), "No pairwise comparison could be calculated")
})

test_that("interpretation distinguishes simple sequential and marginal models", {
    simple <- suppressMessages(permanova(
        data=permanova_state_data(),
        vars=c("sp1", "sp2", "sp3"),
        factor="group",
        permN=19,
        seed=123
    ))
    sequential <- suppressMessages(permanova(
        data=permanova_state_data(),
        vars=c("sp1", "sp2", "sp3"),
        factor="group",
        permFactors="site",
        permBy="terms",
        permN=19,
        seed=123
    ))
    marginal <- suppressMessages(permanova(
        data=permanova_state_data(),
        vars=c("sp1", "sp2", "sp3"),
        factor="group",
        permFactors="site",
        permBy="margin",
        permN=19,
        seed=123
    ))

    expect_match(as.character(simple$note$asString()), "R² is the proportion")
    expect_match(as.character(simple$note$asString()), "Examine PERMDISP")
    expect_false(grepl("Sequential tests", as.character(simple$note$asString())))
    expect_match(as.character(sequential$note$asString()), "Sequential tests depend on model-term order")
    expect_match(as.character(marginal$note$asString()), "Marginal tests assess each term")
})
