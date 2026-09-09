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

test_that("PERMANOVA lifecycle seams retain the fitted output contract", {
    data <- permanova_state_data()
    options <- permanovaOptions$new(
        vars=c("sp1", "sp2", "sp3"), factor="group",
        permN=19, seed=123)
    analysis <- permanovaClass$new(options=options, data=data)
    suppressWarnings(suppressMessages(analysis$run()))
    private <- analysis$.__enclos_env__$private
    expect_true(all(c(".preparePermanova", ".fitPermanova",
        ".assemblePermanovaResults", ".runCompanionPcoa",
        ".runPairwisePermanova") %in% names(private)))
    expect_true(analysis$results$table$visible)
    expect_true(nrow(analysis$results$table$asDF) > 0L)
    expect_identical(private$.state$permutation$effective, "Free")
})

expect_result_visibility <- function(result, visible, hidden) {
    for (name in visible)
        expect_true(result[[name]]$visible, info=paste(name, "should be visible"))
    fixed <- c("table")
    for (name in hidden)
        if (name %in% fixed)
            expect_true(result[[name]]$visible, info=paste(name, "fixed shell should remain visible"))
        else
            expect_false(result[[name]]$visible, info=paste(name, "should be hidden"))
}

test_that("PERMANOVA study design controls use the full option width", {
    ui <- yaml::read_yaml(
        miso_fixture_path("jamovi", "permanova.u.yaml"))
    rootNames <- vapply(
        ui$children,
        function(node) if (is.null(node$name)) "" else node$name,
        character(1))
    studyDesign <- ui$children[[match("studyDesign", rootNames)]]

    expect_false(is.null(studyDesign))
    expect_identical(studyDesign$label, "Study Design and Model")
    expect_true(studyDesign$collapsed)
    expect_identical(studyDesign$children[[1L]]$name, "studyVariables")
    expect_identical(
        studyDesign$children[[1L]]$label,
        "Optional Model Variables")
})

test_that("PERMANOVA narrative results use bounded HTML", {
    results <- yaml::read_yaml(
        miso_fixture_path("jamovi", "permanova.r.yaml"))$items
    by_name <- setNames(results, vapply(results, `[[`, character(1), "name"))

    expect_identical(by_name$guidance$type, "Html")
    expect_identical(by_name$warnings$type, "Html")
})

test_that("PERMANOVA outputs use native titles and notes", {
    results <- yaml::read_yaml(miso_fixture_path("jamovi", "permanova.r.yaml"))$items
    by_name <- setNames(results, vapply(results, `[[`, character(1), "name"))
    expect_identical(by_name$table$title, "PERMANOVA Table")
    expect_identical(by_name$companionPcoa$title, "Companion PCoA")
    expect_identical(by_name$pairwise$title, "Pairwise PERMANOVA")
    expect_false(any(grepl("Purpose$|^(summary|settings|note|interpretation)$",
        names(by_name))))
})

test_that("new PERMANOVA shows only complete getting-started guidance", {
    result <- permanova(
        data=permanova_state_data(),
        vars=character(),
        factor="group"
    )

    guidance <- miso_squish_result(result$guidance)
    expect_match(guidance, "max-width: 44em", fixed=TRUE)
    expect_match(guidance, "To run PERMANOVA:", fixed=TRUE)
    expect_match(guidance, "1. Add one or more numeric Feature variables.", fixed=TRUE)
    expect_match(guidance, "2. Add one categorical Grouping variable with at least two groups.", fixed=TRUE)
    expect_match(guidance, "Results update automatically.", fixed=TRUE)
    expect_result_visibility(
        result,
        visible="guidance",
        hidden=c("warnings", "table", "pairwise")
    )
})

test_that("features without a group show one actionable correction", {
    result <- permanova(
        data=permanova_state_data(),
        vars=c("sp1", "sp2", "sp3"),
        factor=NULL
    )

    guidance <- miso_squish_result(result$guidance)
    expect_match(guidance, "max-width: 44em", fixed=TRUE)
    expect_match(guidance, "PERMANOVA is waiting for a Grouping variable.", fixed=TRUE)
    expect_match(guidance, "Add one categorical variable containing at least two groups.", fixed=TRUE)
    expect_result_visibility(
        result,
        visible="guidance",
        hidden=c("warnings", "table", "pairwise")
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
        hidden=c("warnings", "table", "pairwise")
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
        hidden=c("warnings", "table", "pairwise")
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
        visible=c("table"),
        hidden=c("guidance", "warnings", "pairwise")
    )
})

test_that("PERMANOVA table fields match adonis2 columns by name", {
    data <- permanova_state_data()
    result <- suppressWarnings(suppressMessages(permanova(
        data=data,
        vars=c("sp1", "sp2", "sp3"),
        factor="group",
        permN=19,
        seed=123)))
    prep <- miso_prepare_resemblance(
        data=data,
        vars=c("sp1", "sp2", "sp3"),
        factor="group",
        transform="none",
        distance="bray",
        seed=123)
    set.seed(123)
    reference <- as.data.frame(vegan::adonis2(
        prep$dist ~ group,
        data=data.frame(group=prep$group),
        permutations=miso_permutation(19, "free"),
        by="terms"))
    table <- result$table$asDF
    referenceSource <- rownames(reference)
    for (column in c("Df", "SumOfSqs", "R2")) {
        outputColumn <- c(Df="df", SumOfSqs="sumsqs", R2="r2")[[column]]
        expect_equal(
            table[[outputColumn]][match(referenceSource, table$source)],
            reference[[column]],
            tolerance=0,
            info=column)
    }
    expect_equal(table$f[match("group", table$source)], reference$F[[1L]],
        tolerance=0)
    expect_equal(table$p[match("group", table$source)], reference$`Pr(>F)`[[1L]],
        tolerance=0)
    for (source in c("Residual", "Total")) {
        row <- match(source, table$source)
        rowKey <- result$table$rowKeys[[row]]
        expect_identical(result$table$getCell(rowKey=rowKey, col="f")$value, "")
        expect_identical(result$table$getCell(rowKey=rowKey, col="p")$value, "")
        expect_length(result$table$getCell(rowKey=rowKey, col="f")$footnotes, 0L)
        expect_length(result$table$getCell(rowKey=rowKey, col="p")$footnotes, 0L)
    }
})

test_that("PERMANOVA permutation notices follow the restriction truth table", {
    data <- permanova_state_data()
    cases <- list(
        freeWithoutBlock=list(permScheme="free"),
        freeWithBlock=list(permScheme="free", strata="block"),
        withinWithoutBlock=list(permScheme="stratified"),
        withinWithBlock=list(permScheme="stratified", strata="block"))
    results <- lapply(cases, function(options) {
        suppressWarnings(suppressMessages(do.call(
            permanova,
            c(list(data=data, vars=c("sp1", "sp2", "sp3"),
                factor="group", permN=19, seed=123), options))))
    })

    expect_false(results$freeWithoutBlock$warnings$visible)
    expect_false(grepl("Blocking variable", miso_squish_result(
        results$freeWithoutBlock$warnings), fixed=TRUE))

    expect_true(results$freeWithBlock$warnings$visible)
    expect_match(miso_squish_result(results$freeWithBlock$warnings),
        "Blocking variable 'block' is assigned but not used with Free permutations.",
        fixed=TRUE)
    expect_match(miso_table_note(results$freeWithBlock$table, "method"),
        "Block used: No", fixed=TRUE)

    expect_true(results$withinWithoutBlock$guidance$visible)
    expect_match(miso_squish_result(results$withinWithoutBlock$guidance),
        "Within-block permutations require a Blocking variable.", fixed=TRUE)
    expect_match(miso_squish_result(results$withinWithoutBlock$guidance),
        "Add one, or select Free permutations.", fixed=TRUE)
    expect_false(results$withinWithoutBlock$warnings$visible)
    expect_equal(nrow(results$withinWithoutBlock$table$asDF), 0L)

    expect_false(results$withinWithBlock$guidance$visible)
    expect_false(results$withinWithBlock$warnings$visible)
    expect_match(miso_table_note(results$withinWithBlock$table, "method"),
        "Block used: Yes", fixed=TRUE)
    expect_match(miso_table_note(results$withinWithBlock$table, "method"),
        "Blocking variable 'block'", fixed=TRUE)
})


test_that("PERMANOVA table notes disclose non-default distance and series settings", {
    result <- suppressWarnings(suppressMessages(permanova(
        data=permanova_state_data(),
        vars=c("sp1", "sp2", "sp3"),
        factor="group",
        distBinary=TRUE,
        distSqrt=TRUE,
        distAdd="cailliez",
        permScheme="series",
        permN=19,
        seed=123)))
    note <- miso_table_note(result$table, "method")
    expect_match(note, "Binary dissimilarity: Enabled", fixed=TRUE)
    expect_match(note, "Square-root distances: Enabled", fixed=TRUE)
    expect_match(note, "Additive correction: Cailliez", fixed=TRUE)
    expect_match(note, "Permutation restrictions: Series", fixed=TRUE)
    expect_match(note, "Sequence order: Current data-row order", fixed=TRUE)
})

test_that("PERMANOVA output names and suffixes remain stable", {
    schema <- yaml::read_yaml(miso_fixture_path("jamovi", "permanova.r.yaml"))
    names <- vapply(schema$items, `[[`, character(1), "name")
    expect_identical(names, c(
        "guidance", "warnings", "table", "companionPcoaDescription",
        "companionPcoa", "companionPcoaSites", "companionPcoaCentroids",
        "pairwise"))
    table <- schema$items[[match("table", names)]]
    expect_identical(vapply(table$columns, `[[`, character(1), "name"),
        c("source", "df", "sumsqs", "r2", "f", "p"))
    expect_identical(vapply(table$columns, `[[`, character(1), "title"),
        c("Source", "df", "Sum Sq", "R²", "Pseudo-F", "Permutation p"))
    expect_false(any(grepl("\\((required|optional)\\)$",
        vapply(yaml::read_yaml(miso_fixture_path(
            "jamovi", "permanova.a.yaml"))$options,
            function(option) if (is.null(option$title)) "" else option$title,
            character(1)), ignore.case=TRUE)))
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
        visible=c("warnings", "table"),
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
        hidden=c("warnings", "table", "pairwise")
    )
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
        miso_squish_result(failed$guidance),
        "Grouping variable does not vary within any block")
    expect_true(failed$table$visible)
    expect_equal(length(failed$table$rowKeys), 0L)

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


permanova_companion_args <- function(...) {
    c(list(
        data=permanova_state_data(),
        vars=c("sp1", "sp2", "sp3"),
        factor="group",
        permN=19,
        seed=123),
    list(...))
}

run_permanova_companion <- function(...) {
    suppressWarnings(suppressMessages(do.call(
        permanova, permanova_companion_args(...))))
}

test_that("PERMANOVA companion contracts are optional, bounded, and ordered", {
    analysis <- yaml::read_yaml(
        miso_fixture_path("jamovi", "permanova.a.yaml"))
    options <- setNames(
        analysis$options,
        vapply(analysis$options, `[[`, character(1), "name"))
    expect_false(options$showCompanionPcoa$default)
    expect_null(options$pcoaDisplayFactor$default)
    expect_false(options$pcoaCentroids$default)
    expect_false(options$pcoaSpiders$default)
    expect_true(all(c("factor", "numeric") %in%
        options$pcoaDisplayFactor$permitted))
    expect_identical(options$pcoaDisplayFactor$title,
        "Model factor")
    expect_match(options$pcoaDisplayFactor$description,
        "already included and retained in the fitted PERMANOVA model",
        fixed=TRUE)

    ui <- yaml::read_yaml(
        miso_fixture_path("jamovi", "permanova.u.yaml"))
    rootNames <- vapply(ui$children, function(item)
        if (is.null(item$name)) "" else item$name, character(1))
    plots <- ui$children[[match("plots", rootNames)]]
    expect_false(plots$collapsed)
    expect_identical(plots$label, "Plots")
    plotChildNames <- vapply(plots$children, function(item)
        if (is.null(item$name)) "" else item$name, character(1))
    expect_true(all(c(
        "showCompanionPcoa", "pcoaDisplayVariables",
        "pcoaCentroids", "pcoaSpiders") %in%
        plotChildNames))
    displaySupplier <- plots$children[[match(
        "pcoaDisplayVariables",
        plotChildNames)]]
    expect_identical(displaySupplier$label,
        "Display Groups By")
    expect_identical(displaySupplier$children[[1L]]$label,
        "Model Factor")

    results <- yaml::read_yaml(
        miso_fixture_path("jamovi", "permanova.r.yaml"))$items
    names <- vapply(results, `[[`, character(1), "name")
    expect_identical(names[match("table", names) + seq_len(5L)], c(
        "companionPcoaDescription", "companionPcoa",
        "companionPcoaSites",
        "companionPcoaCentroids",
        "pairwise"))
    byName <- setNames(results, names)
    expect_identical(byName$companionPcoa$width, 600L)
    expect_identical(byName$companionPcoa$height, 500L)
    expect_false(byName$companionPcoa$visible)
    expect_identical(byName$companionPcoa$renderFun,
        ".plotCompanionPcoa")
    expect_identical(
        vapply(byName$companionPcoaSites$columns, `[[`, character(1), "name"),
        c("site", "sourceRow", "group", "PCoA1", "PCoA2"))
})

test_that("simple companion PCoA automatically displays the primary factor", {
    result <- run_permanova_companion(
        showCompanionPcoa=TRUE,
        pcoaCentroids=TRUE,
        pcoaSpiders=TRUE)

    expect_true(result$table$visible)
    expect_true(result$companionPcoa$visible)
    expect_true(result$companionPcoaDescription$visible)
    expect_true(result$companionPcoaSites$visible)
    expect_true(result$companionPcoaCentroids$visible)
    expect_identical(unique(result$companionPcoaSites$asDF$group),
        levels(permanova_state_data()$group))
    expect_equal(nrow(result$companionPcoaSites$asDF), 9L)
    expect_equal(nrow(result$companionPcoaCentroids$asDF), 3L)
    expect_identical(result$companionPcoaCentroids$asDF$n,
        rep(3L, 3L))
    description <- miso_squish_result(result$companionPcoaDescription)
    expect_match(description,
        "Visualises sample resemblance and group positions alongside the test",
        fixed=TRUE)
    expect_match(description, 'aria-label="About PERMANOVA companion PCoA"',
        fixed=TRUE)
})


test_that("companion coordinates preserve filtered source rows and factor alignment", {
    data <- permanova_state_data()
    data$sp1[[2L]] <- NA_real_
    data[c("sp1", "sp2", "sp3")][5L, ] <- 0
    result <- suppressWarnings(suppressMessages(permanova(
        data=data,
        vars=c("sp1", "sp2", "sp3"),
        factor="group", permFactors="site",
        showCompanionPcoa=TRUE, pcoaDisplayFactor="site",
        permN=19, seed=123)))
    sites <- result$companionPcoaSites$asDF

    expect_identical(sites$sourceRow, setdiff(seq_len(nrow(data)), c(2L, 5L)))
    expect_identical(sites$group,
        as.character(data$site[sites$sourceRow]))
    expect_identical(sites$site, rownames(data)[sites$sourceRow])
    expect_false(any(vapply(sites, function(column)
        any(is.nan(suppressWarnings(as.numeric(column)))), logical(1))))
})

test_that("companion eligibility follows factors retained by the fitted model", {
    data <- permanova_state_data()
    data$collapsing <- factor(c(
        "retained", "filtered", "retained",
        "retained", "filtered", "retained",
        "retained", "retained", "retained"))
    data$sp1[[2L]] <- NA_real_
    data[5L, c("sp1", "sp2", "sp3")] <- 0

    common <- list(
        data=data, vars=c("sp1", "sp2", "sp3"), factor="group",
        permN=19, seed=123)
    baseline <- suppressWarnings(suppressMessages(do.call(
        permanova, common)))
    companion <- suppressWarnings(suppressMessages(do.call(
        permanova, c(common, list(
            permFactors="collapsing", showCompanionPcoa=TRUE,
            pcoaDisplayFactor="collapsing", pcoaCentroids=TRUE)))))

    expect_identical(companion$table$asDF, baseline$table$asDF)
    expect_true(companion$companionPcoa$visible)
    expect_true(companion$companionPcoaSites$visible)
    expect_identical(
        companion$companionPcoaSites$asDF$sourceRow,
        setdiff(seq_len(nrow(data)), c(2L, 5L)))
    expect_identical(
        companion$companionPcoaSites$asDF$group,
        as.character(data$group[companion$companionPcoaSites$asDF$sourceRow]))
    expect_false(any(grepl(
        "collapsing", companion$table$asDF$source, fixed=TRUE)))
    warning <- miso_squish_result(companion$warnings)
    expect_match(warning,
        "selected display factor 'collapsing' was not retained", fixed=TRUE)
    expect_match(warning,
        "automatically displays 'group'", fixed=TRUE)

    retained <- run_permanova_companion(
        permFactors="site", showCompanionPcoa=TRUE,
        pcoaDisplayFactor="site")
    expect_true(retained$companionPcoa$visible)
    expect_identical(
        retained$companionPcoaSites$asDF$group,
        as.character(permanova_state_data()$site))
})

test_that("PERMANOVA companion notices retain invalid display assignments", {
    data <- permanova_state_data()
    data$covariate <- seq_len(nrow(data))
    common <- list(
        data=data, vars=c("sp1", "sp2", "sp3"), factor="group",
        permFactors="site", showCompanionPcoa=TRUE, permN=19, seed=123)

    ineligible <- suppressWarnings(suppressMessages(do.call(
        permanova, c(common, list(pcoaDisplayFactor="covariate")))))
    expect_true(ineligible$table$visible)
    expect_false(ineligible$companionPcoa$visible)
    ineligible_notice <- miso_squish_result(ineligible$companionPcoaDescription)
    expect_match(ineligible_notice, "covariate", fixed=TRUE)
    expect_match(ineligible_notice, "not a categorical factor retained", fixed=TRUE)
    expect_match(ineligible_notice, "group, site", fixed=TRUE)
    expect_match(ineligible_notice, "PERMANOVA and pairwise results remain available", fixed=TRUE)

    unavailable <- permanovaClass$new(
        options=permanovaOptions$new(
            vars=c("sp1", "sp2", "sp3"), factor="group",
            permFactors="site", showCompanionPcoa=TRUE,
            permN=19, seed=123), data=data)
    suppressWarnings(suppressMessages(unavailable$run()))
    unavailable_option <- unavailable$options$option("pcoaDisplayFactor")
    unavailable_option$.__enclos_env__$private$.value <- "removed_factor"
    private <- unavailable$.__enclos_env__$private
    private$.runCompanionPcoa(
        private$.state$companion$prep,
        private$.state$companion$model)
    expect_true(unavailable$results$table$visible)
    unavailable_notice <- miso_squish_result(
        unavailable$results$companionPcoaDescription)
    expect_match(unavailable_notice, "removed_factor", fixed=TRUE)
    expect_match(unavailable_notice, "unavailable in the data", fixed=TRUE)
    expect_match(unavailable_notice, "group, site", fixed=TRUE)
})

test_that("companion coordinates exactly reuse every PERMANOVA distance correction", {
    cases <- list(
        list(sqrt=FALSE, add="none"),
        list(sqrt=TRUE, add="none"),
        list(sqrt=FALSE, add="lingoes"),
        list(sqrt=FALSE, add="cailliez"))
    data <- permanova_state_data()
    prep <- miso_prepare_resemblance(
        data=data, vars=c("sp1", "sp2", "sp3"), factor="group",
        transform="none", distance="bray", seed=123,
        distBinary=FALSE)

    for (case in cases) {
        result <- run_permanova_companion(
            showCompanionPcoa=TRUE,
            distSqrt=case$sqrt,
            distAdd=case$add)
        expected <- .misoPcoa(
            prep$dist,
            sqrtDist=case$sqrt,
            correction=case$add,
            groups=prep$group)
        sites <- result$companionPcoaSites$asDF
        expect_equal(sites$PCoA1, unname(expected$points[, 1L]),
            tolerance=0, info=paste(case$sqrt, case$add))
        expect_equal(sites$PCoA2, unname(expected$points[, 2L]),
            tolerance=0, info=paste(case$sqrt, case$add))

        working <- prep$dist
        if (case$sqrt)
            working[] <- sqrt(working[])
        reference <- vegan::wcmdscale(
            working, k=attr(working, "Size") - 1L, eig=TRUE,
            add=if (identical(case$add, "none")) FALSE else case$add,
            x.ret=TRUE)
        expect_equal(expected$fit$eig, reference$eig, tolerance=0)
        expect_equal(expected$fit$points, reference$points, tolerance=0)
    }
})


test_that("companion failure and one-axis geometry preserve inferential output", {
    originalPcoa <- .misoPcoa
    pcoaCalls <- 0L
    testthat::local_mocked_bindings(
        .misoPcoa=function(...) {
            pcoaCalls <<- pcoaCalls + 1L
            if (pcoaCalls == 1L)
                return(list(
                    error=TRUE, message="simulated companion failure"))
            originalPcoa(...)
        },
        .package="miso")
    failed <- run_permanova_companion(
        showCompanionPcoa=TRUE, permPairwise=TRUE)
    expect_true(failed$table$visible)
    expect_true(failed$pairwise$visible)
    expect_false(failed$companionPcoa$visible)
    expect_false(failed$companionPcoaSites$visible)
    expect_match(miso_squish_result(failed$companionPcoaDescription),
        "simulated companion failure", fixed=TRUE)

    oneAxis <- suppressWarnings(suppressMessages(permanova(
        data=data.frame(
            abundance=c(1, 2, 3, 4, 5, 6),
            group=factor(rep(c("A", "B"), each=3))),
        vars="abundance", factor="group", distance="euclidean",
        showCompanionPcoa=TRUE, pcoaCentroids=TRUE,
        permN=19, seed=123)))
    expect_true(oneAxis$table$visible)
    expect_false(oneAxis$companionPcoa$visible)
    expect_true(oneAxis$companionPcoaSites$visible)
    expect_true(oneAxis$companionPcoaCentroids$visible)
    expect_true(all(is.na(oneAxis$companionPcoaSites$asDF$PCoA2)))
    expect_match(miso_squish_result(oneAxis$companionPcoaDescription),
        "two-dimensional companion plot is unavailable", fixed=TRUE)
    oneAxisDescription <- miso_squish_result(
        oneAxis$companionPcoaDescription)
    expect_match(oneAxisDescription,
        "Coordinate tables retain the fitted result", fixed=TRUE)
    expect_false(grepl("Sites are shown", oneAxisDescription, fixed=TRUE))
    expect_false(grepl("centroids are shown", oneAxisDescription, fixed=TRUE))
})

test_that("companion state clears when toggled off or its model factor is removed", {
    data <- permanova_state_data()
    options <- permanovaOptions$new(
        vars=c("sp1", "sp2", "sp3"), factor="group",
        permFactors=c("site", "block"),
        showCompanionPcoa=TRUE, pcoaDisplayFactor="site",
        pcoaCentroids=TRUE, permN=19, seed=123)
    analysis <- permanovaClass$new(options=options, data=data)
    suppressWarnings(suppressMessages(analysis$run()))
    expect_true(analysis$results$companionPcoa$visible)
    expect_gt(length(analysis$results$companionPcoaSites$rowKeys), 0L)

    factorOption <- options$option("permFactors")
    factorOption$.__enclos_env__$private$.value <- "block"
    suppressWarnings(suppressMessages(analysis$run()))
    expect_true(analysis$results$table$visible)
    expect_false(analysis$results$companionPcoa$visible)
    expect_false(analysis$results$companionPcoaSites$visible)
    expect_equal(length(analysis$results$companionPcoaSites$rowKeys), 0L)
    expect_match(
        miso_squish_result(analysis$results$companionPcoaDescription),
        "Choose one eligible retained model factor", fixed=TRUE)

    showOption <- options$option("showCompanionPcoa")
    showOption$.__enclos_env__$private$.value <- FALSE
    suppressWarnings(suppressMessages(analysis$run()))
    expect_true(analysis$results$table$visible)
    expect_false(analysis$results$companionPcoaDescription$visible)
    expect_false(analysis$results$companionPcoa$visible)
    expect_equal(length(analysis$results$companionPcoaSites$rowKeys), 0L)
    expect_equal(length(analysis$results$companionPcoaCentroids$rowKeys), 0L)
})

test_that("PERMANOVA prepares once and passes one identical distance object", {
    originalPrepare <- miso_prepare_resemblance
    originalPcoa <- .misoPcoa
    originalAdonis <- vegan::adonis2
    prepareCount <- 0L
    pcoaCount <- 0L
    preparedDistance <- NULL
    pcoaDistance <- NULL
    adonisDistance <- NULL
    testthat::local_mocked_bindings(
        miso_prepare_resemblance=function(...) {
            prepareCount <<- prepareCount + 1L
            value <- originalPrepare(...)
            preparedDistance <<- value$dist
            value
        },
        .misoPcoa=function(distance, ...) {
            pcoaCount <<- pcoaCount + 1L
            pcoaDistance <<- distance
            originalPcoa(distance, ...)
        },
        .package="miso")
    testthat::local_mocked_bindings(
        adonis2=function(formula, ...) {
            adonisDistance <<- get(".dist", envir=parent.frame())
            .dist <- adonisDistance
            environment(formula) <- environment()
            do.call(originalAdonis,
                c(list(formula=formula), list(...)))
        },
        .package="vegan")

    result <- run_permanova_companion(showCompanionPcoa=TRUE)
    expect_true(result$table$visible)
    expect_identical(prepareCount, 1L)
    expect_identical(pcoaCount, 1L)
    expect_identical(pcoaDistance, preparedDistance)
    expect_identical(adonisDistance, preparedDistance)
})

test_that("companion plot callback uses cached reusable plot data only", {
    options <- permanovaOptions$new(
        vars=c("sp1", "sp2", "sp3"), factor="group",
        showCompanionPcoa=TRUE, pcoaSpiders=TRUE,
        permN=19, seed=123)
    analysis <- permanovaClass$new(
        options=options, data=permanova_state_data())
    suppressWarnings(suppressMessages(analysis$run()))
    state <- analysis$.__enclos_env__$private$.state$companion
    plot <- .buildPcoaPlot(state$plotData)
    expect_s3_class(plot, "ggplot")
    expect_identical(plot$theme$plot.background$fill, "transparent")
    expect_identical(plot$theme$panel.background$fill, "transparent")
    expect_identical(plot$coordinates$ratio, 1)
    expect_false(any(grepl("ellipse", vapply(
        plot$layers,
        function(layer) paste(class(layer$geom), collapse=" "),
        character(1)), ignore.case=TRUE)))

    originalPlotData <- state$plotData
    testthat::local_mocked_bindings(
        miso_prepare_resemblance=function(...) stop("unexpected preparation"),
        .misoPcoa=function(...) stop("unexpected PCoA refit"),
        .package="miso")
    file <- tempfile(fileext=".png")
    grDevices::png(file, width=600, height=500)
    on.exit({
        if (grDevices::dev.cur() > 1L)
            grDevices::dev.off()
        unlink(file)
    }, add=TRUE)
    expect_silent(
        analysis$.__enclos_env__$private$.plotCompanionPcoa(
            analysis$results$companionPcoa))
    grDevices::dev.off()
    expect_gt(file.info(file)$size, 1000)
    expect_identical(
        analysis$.__enclos_env__$private$.state$companion$plotData,
        originalPlotData)
})

test_that("more than 64 groups use neutral companion styling without data loss", {
    groups <- factor(rep(sprintf("group_%02d", seq_len(65L)), each=2L))
    index <- seq_along(groups)
    data <- data.frame(
        sp1=1 + index,
        sp2=1 + (index %% 17L),
        sp3=1 + (index %% 11L),
        group=groups)
    result <- suppressWarnings(suppressMessages(permanova(
        data=data, vars=c("sp1", "sp2", "sp3"), factor="group",
        distance="euclidean", showCompanionPcoa=TRUE,
        pcoaCentroids=TRUE, pcoaSpiders=TRUE,
        permN=1, seed=123)))

    expect_true(result$table$visible)
    expect_true(result$companionPcoa$visible)
    expect_equal(nrow(result$companionPcoaSites$asDF), 130L)
    expect_equal(nrow(result$companionPcoaCentroids$asDF), 65L)
    expect_identical(sort(unique(result$companionPcoaSites$asDF$group)),
        sort(levels(groups)))
    expect_match(miso_squish_result(result$warnings),
        "Neutral site styling is used because more than 64 groups", fixed=TRUE)
    expect_match(miso_squish_result(result$warnings),
        "centroids and spiders are omitted from the image", fixed=TRUE)
})

test_that("PERMANOVA companion JavaScript manages eligibility and focus", {
    js <- paste(readLines(
        miso_fixture_path("jamovi", "js", "permanova.js"),
        warn=FALSE), collapse="\n")
    expect_match(js,
        "pcoaDisplayFactor.setEnabled(displayEnabled)",
        fixed=TRUE)
    expect_match(js,
        "const displayEnabled = primary.length > 0 || multifactor;",
        fixed=TRUE)
    expect_match(js,
        "const displayed = selections(ui.pcoaDisplayFactor.value())",
        fixed=TRUE)
    expect_false(grepl("pcoaDisplayFactor.setValue(null)", js, fixed=TRUE))
    expect_match(js,
        "pcoaCentroids.setEnabled(centroidsEnabled)",
        fixed=TRUE)
    expect_match(js,
        "pcoaSpiders.setEnabled(spidersEnabled)",
        fixed=TRUE)
    expect_match(js, "The R-side guard", fixed=TRUE)
    expect_match(js, "dependentHadFocus", fixed=TRUE)
    expect_match(js, "focusedDependentDisabled", fixed=TRUE)
    expect_match(js, "focusControl(destination)", fixed=TRUE)
    expect_match(js, "setTimeout(() => refreshView(ui), 100)", fixed=TRUE)
})

test_that("PERMANOVA companion display assignment survives plot and model changes", {
    ui <- yaml::read_yaml(
        miso_fixture_path("jamovi", "permanova.u.yaml"))
    plots <- ui$children[[match(
        "plots", vapply(ui$children, function(item)
            if (is.null(item$name)) "" else item$name, character(1)))]]
    pairwise <- yaml::read_yaml(
        miso_fixture_path("jamovi", "permanova.u.yaml"))$children[[3L]]$children[[6L]]
    expect_identical(pairwise$name, "permPairwise")
    expect_identical(pairwise$children[[1L]]$name, "permAdjust")
    expect_identical(pairwise$children[[1L]]$enable, "(permPairwise)")
    display <- plots$children[[match(
        "pcoaDisplayVariables",
        vapply(plots$children, function(item)
            if (is.null(item$name)) "" else item$name, character(1)))]]
    expect_identical(display$children[[1L]]$children[[1L]]$name,
        "pcoaDisplayFactor")

    data <- permanova_state_data()
    options <- permanovaOptions$new(
        vars=c("sp1", "sp2", "sp3"), factor="group",
        permFactors="site", pcoaDisplayFactor="site",
        showCompanionPcoa=FALSE, permN=19, seed=123)
    analysis <- permanovaClass$new(options=options, data=data)
    suppressWarnings(suppressMessages(analysis$run()))
    show <- options$option("showCompanionPcoa")
    show$.__enclos_env__$private$.value <- TRUE
    suppressWarnings(suppressMessages(analysis$run()))
    expect_true(analysis$results$companionPcoa$visible)
    expect_identical(
        analysis$results$companionPcoaSites$asDF$group,
        as.character(data$site))
})

test_that("PERMANOVA publishes retained display notices after off-to-on toggle", {
    data <- permanova_state_data()
    data$collapsing <- factor(rep(c("retained", "filtered"), length.out=nrow(data)))
    options <- permanovaOptions$new(
        vars=c("sp1", "sp2", "sp3"), factor="group",
        permFactors="collapsing", pcoaDisplayFactor="collapsing",
        showCompanionPcoa=FALSE, permN=19, seed=123)
    analysis <- permanovaClass$new(options=options, data=data)
    suppressWarnings(suppressMessages(analysis$run()))
    expect_true(analysis$results$table$visible)
    expect_false(analysis$results$companionPcoaDescription$visible)

    # Filter/drop the second level while the companion plot remains off.
    analysis$.__enclos_env__$private$.data$collapsing <-
        factor(rep("retained", nrow(data)))
    # A structural option change represents the recalculation triggered by
    # the filtered data in jamovi while the plot remains off.
    seed <- options$option("seed")
    seed$.__enclos_env__$private$.value <- 124
    suppressWarnings(suppressMessages(analysis$run()))
    expect_true(analysis$results$table$visible)
    expect_false(analysis$results$companionPcoaDescription$visible)

    show <- options$option("showCompanionPcoa")
    show$.__enclos_env__$private$.value <- TRUE
    suppressWarnings(suppressMessages(analysis$run()))
    expect_true(analysis$results$table$visible)
    expect_true(analysis$results$warnings$visible)
    expect_true(analysis$results$companionPcoaDescription$visible)
    notice <- miso_squish_result(analysis$results$warnings)
    expect_match(notice, "collapsing", fixed=TRUE)
    expect_match(notice, "not retained", fixed=TRUE)
    expect_match(notice, "automatically displays 'group'", fixed=TRUE)
})

test_that("primary-only display assignments are retained by the R-side guard", {
    data <- permanova_state_data()
    data$covariate <- seq_len(nrow(data))
    options <- permanovaOptions$new(
        vars=c("sp1", "sp2", "sp3"), factor="group",
        pcoaDisplayFactor="covariate",
        showCompanionPcoa=FALSE, permN=19, seed=123)
    analysis <- permanovaClass$new(options=options, data=data)
    suppressWarnings(suppressMessages(analysis$run()))
    expect_true(analysis$results$table$visible)
    expect_false(analysis$results$companionPcoaDescription$visible)

    show <- options$option("showCompanionPcoa")
    show$.__enclos_env__$private$.value <- TRUE
    suppressWarnings(suppressMessages(analysis$run()))
    expect_true(analysis$results$table$visible)
    # The primary factor is the automatic display group; the retained
    # ineligible assignment is explained rather than silently applied.
    expect_true(analysis$results$companionPcoa$visible)
    expect_true(analysis$results$companionPcoaSites$visible)
    expect_identical(
        analysis$.__enclos_env__$private$.state$companion$displayFactor,
        "group")
    expect_true(analysis$results$warnings$visible)
    warning <- miso_squish_result(analysis$results$warnings)
    expect_match(warning, "covariate", fixed=TRUE)
    expect_match(warning, "not a categorical factor retained", fixed=TRUE)
    expect_match(warning, "automatically displays 'group'", fixed=TRUE)
})

test_that("PERMANOVA companion JavaScript executes control-state behavior", {
    node <- Sys.which("node")
    skip_if(!nzchar(node), "Node.js is unavailable")

    harness <- normalizePath(
        miso_fixture_path("js", "permanova-ui-harness.js"),
        mustWork=TRUE)
    module <- normalizePath(
        miso_fixture_path("jamovi", "js", "permanova.js"),
        mustWork=TRUE)
    output <- system2(
        node,
        c(shQuote(harness), shQuote(module)),
        stdout=TRUE, stderr=TRUE)
    status <- attr(output, "status", exact=TRUE)

    expect_true(is.null(status) || identical(status, 0L),
        info=paste(output, collapse="\n"))
    expect_identical(output, "ok")
})

test_that("companion PCoA renders from serialized Image state alone", {
    options <- permanovaOptions$new(
        vars=c("sp1", "sp2", "sp3"), factor="group",
        showCompanionPcoa=TRUE, pcoaCentroids=TRUE, pcoaSpiders=TRUE,
        permN=19, seed=123)
    analysis <- permanovaClass$new(
        options=options, data=permanova_state_data())
    suppressWarnings(suppressMessages(analysis$run()))
    image <- analysis$results$companionPcoa
    state <- image$state
    expect_false(is.null(state))
    expect_true(state$available)

    restored <- unserialize(serialize(state, NULL))
    analysis$.__enclos_env__$private$.state$companion$plotData <- NULL
    image$setState(restored)

    file <- tempfile(fileext=".png")
    on.exit({
        if (grDevices::dev.cur() > 1L)
            grDevices::dev.off()
        unlink(file)
    }, add=TRUE)
    grDevices::png(file, width=600, height=500)
    analysis$.__enclos_env__$private$.plotCompanionPcoa(image)
    grDevices::dev.off()
    expect_gt(file.info(file)$size, 1000)
})


test_that("structural model inputs rebuild term rows while value-only changes update in place", {
    data <- permanova_state_data()
    options <- permanovaOptions$new(
        vars=c("sp1", "sp2", "sp3"),
        factor="group",
        permFactors="site",
        permN=19,
        seed=123)
    analysis <- permanovaClass$new(options=options, data=data)
    suppressMessages(suppressWarnings(analysis$run()))
    keys <- analysis$results$table$rowKeys
    firstCell <- miso_table_first_cell(analysis$results$table)
    rowsBefore <- length(keys)

    # Value-only option change: same model terms, so no structural rebuild.
    permNOption <- options$option("permN")
    permNOption$.__enclos_env__$private$.value <- 29
    suppressMessages(suppressWarnings(analysis$run()))
    expect_identical(analysis$results$table$rowKeys, keys)
    expect_identical(firstCell, miso_table_first_cell(analysis$results$table))
    expect_gt(length(analysis$results$table$rowKeys), 0L)

    # Structural input: adding model interactions rebuilds the term rows.
    interactionsOption <- options$option("permInteractions")
    interactionsOption$.__enclos_env__$private$.value <- TRUE
    suppressMessages(suppressWarnings(analysis$run()))
    expect_false(identical(analysis$results$table$rowKeys, keys))
    expect_false(identical(
        firstCell, miso_table_first_cell(analysis$results$table)))
    expect_gt(length(analysis$results$table$rowKeys), rowsBefore)
    expect_match(
        as.character(analysis$results$table$asString()),
        "group",
        fixed=TRUE)
})

test_that("PERMANOVA keeps interpretation settings in surviving table notes", {
    result <- suppressWarnings(suppressMessages(permanova(
        data=permanova_state_data(), vars=c("sp1", "sp2", "sp3"),
        factor="group", permN=19, seed=123)))
    expect_true(result$table$visible)
    expect_match(miso_table_note(result$table, "method"), "Transformation|restriction|Permutation")
})
