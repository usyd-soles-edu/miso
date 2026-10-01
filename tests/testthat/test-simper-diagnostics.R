simper_diagnostic_data <- function(single_pair=FALSE) {
    if (single_pair)
        return(data.frame(x=c(1,3,4,6), y=c(2,1,2,3), z=c(0,0,2,3),
            group=factor(c("A","B","C","C"))))
    data.frame(x=c(1,1,3,3,5,6), y=c(1,1,1,1,2,3), z=c(0,0,0,0,4,5),
        group=factor(rep(c("A","B","C"), each=2L)))
}

simper_diagnostic_run <- function(data=simper_diagnostic_data(), ...) {
    settings <- modifyList(list(vars=setdiff(names(data), "group"), factor="group",
        simperTop=50, simperCum=100), list(...))
    analysis <- simperClass$new(options=do.call(simperOptions$new, settings), data=data)
    suppressWarnings(suppressMessages(analysis$run()))
    analysis
}

simper_diagnostic_private <- function(analysis) analysis$.__enclos_env__$private

simper_diagnostic_set <- function(analysis, name, value) {
    option <- analysis$options$option(name)
    option$.__enclos_env__$private$.value <- value
    suppressWarnings(suppressMessages(analysis$run()))
}

simper_diagnostic_reasons <- function(analysis, contrast=1L) {
    issues <- Filter(function(issue) issue$contrastIndex == contrast,
        simper_diagnostic_private(analysis)$.state$descriptive$diagnostics)
    setNames(vapply(issues, function(issue) issue$reason, character(1)),
        vapply(issues, function(issue) issue$feature, character(1)))
}

test_that("zero SD explanations distinguish absence and positive contributions", {
    analysis <- simper_diagnostic_run(simperDetails=TRUE, simperTop=1)
    reasons <- simper_diagnostic_reasons(analysis)
    expect_identical(unname(reasons[c("x","y","z")]),
        c("constant_positive", "zero_present", "absent"))
    note <- simper_diagnostic_private(analysis)$.detailNote(simper_diagnostic_private(analysis)$.state$descriptive$diagnostics)
    expect_match(note, "absent in the retained samples", fixed=TRUE)
    expect_match(note, "transformed feature values are identical", fixed=TRUE)
    expect_match(note, "positive contribution with SD zero", fixed=TRUE)
    expect_match(note, "not a significance test", fixed=TRUE)
    expect_false(analysis$results$warnings$visible)
    expect_identical(simper_test_detail_table(analysis$results)$asDF$feature,
        analysis$results$contributions$asDF$feature[analysis$results$contributions$asDF$contrast == simper_test_detail_table(analysis$results)$title])
    compact <- subset(analysis$results$contributions$asDF, contrast == "A vs B")
    expect_identical(compact$feature, "x")
    expect_false(any(grepl("reason|pairCount|diagnostic", names(simper_test_detail_table(analysis$results)$asDF))))
    expect_false(any(grepl("reason|pairCount|diagnostic",
        names(simper_diagnostic_private(analysis)$.state$plotData))))
})

test_that("Range zeros do not create false absence claims", {
    data <- data.frame(x=c(1,1,1,1,3,4), y=1:6, w=6:1,
        group=factor(rep(c("A","B","C"), each=2L)))
    analysis <- simper_diagnostic_run(data, transform="range", simperDetails=TRUE)
    expect_false(analysis$results$guidance$visible)
    expect_identical(unname(simper_diagnostic_reasons(analysis)["x"]), "zero_present")
    note <- simper_diagnostic_private(analysis)$.detailNote(simper_diagnostic_private(analysis)$.state$descriptive$diagnostics)
    expect_false(grepl("absent", note, fixed=TRUE))
    expect_match(note, "transformed feature values", fixed=TRUE)
})

test_that("single-pair diagnosis has precedence but one versus many can have SD", {
    analysis <- simper_diagnostic_run(simper_diagnostic_data(TRUE), simperDetails=TRUE)
    expect_true(all(simper_diagnostic_reasons(analysis) == "single_pair"))
    warning <- miso_squish_result(analysis$results$warnings)
    expect_match(warning, "1 contrast with only one between-group sample pair", fixed=TRUE)
    expect_false(grepl("Replication|insufficient|for: x", warning))
    expect_match(miso_table_note(simper_test_detail_table(analysis$results), "meaning"),
        "fewer than two between-group sample pairs", fixed=TRUE)
    many <- simper_diagnostic_run(data.frame(x=c(1,3,4,6),y=c(2,1,2,3),
        group=factor(c("A","B","B","B"))), simperDetails=TRUE)
    expect_true(all(is.finite(simper_test_detail_table(many$results)$asDF$sd)))
    expect_length(simper_diagnostic_reasons(many), 0L)
    expect_false(many$results$warnings$visible)
})

test_that("unexpected raw statistics produce bounded detail warnings only", {
    original <- vegan::simper
    testthat::local_mocked_bindings(simper=function(...) {
        fit <- original(...)
        fit[[1L]]$sd[[1L]] <- NA_real_
        fit[[1L]]$ratio[[1L]] <- NA_real_
        fit
    }, .package="vegan")
    analysis <- simper_diagnostic_run(simperDetails=FALSE)
    expect_identical(unname(simper_diagnostic_reasons(analysis)["x"]), "unexpected")
    expect_false(analysis$results$warnings$visible)
    simper_diagnostic_set(analysis, "simperDetails", TRUE)
    expect_match(miso_squish_result(analysis$results$warnings),
        "1 feature row", fixed=TRUE)
    expect_match(miso_table_note(simper_test_detail_table(analysis$results), "meaning"),
        "finite value could not be calculated", fixed=TRUE)
    simper_diagnostic_set(analysis, "simperDetails", FALSE)
    expect_false(analysis$results$warnings$visible)
    expect_identical(analysis$results$warnings$content, "")
})

test_that("small finite SD values are not classified as zero", {
    analysis <- simper_diagnostic_run()
    prep <- list(group=factor(c("A","A","B","B")), comm=matrix(1,4,1,
        dimnames=list(NULL,"x")))
    tab <- data.frame(feature="x",average=1e-20,sd=1e-30,ratio=1e10)
    expect_length(simper_diagnostic_private(analysis)$.classifyDetailDiagnostics(
        tab, prep, c("A","B"), 1L), 0L)
})

test_that("detail presentation preserves numerical tables plots p-values and RNG", {
    original <- vegan::simper
    calls <- 0L
    testthat::local_mocked_bindings(simper=function(...) {
        calls <<- calls + 1L
        original(...)
    }, .package="vegan")
    analysis <- simper_diagnostic_run(simperAssess=TRUE, simperN=19,
        seed=123, simperPlots=TRUE, simperHeatmap=TRUE, simperDetails=FALSE)
    private <- simper_diagnostic_private(analysis)
    expect_identical(calls, 2L)
    snapshots <- lapply(c("contributions","assessment"),
        function(name) analysis$results[[name]]$asDF)
    fit <- private$.state$descriptive$fit
    plots <- lapply(analysis$results$contributionPlots$items, function(item) item$plot$state)
    heatmap <- analysis$results$heatmap$state
    rng <- .Random.seed
    seedNote <- miso_table_note(analysis$results$assessment, "seed")
    scope <- miso_table_note(analysis$results$assessment, "scope")
    expect_match(seedNote,"123",fixed=TRUE)
    for (details in c(TRUE,FALSE,TRUE)) {
        simper_diagnostic_set(analysis,"simperDetails",details)
        expect_identical(calls,2L)
        expect_identical(.Random.seed,rng)
        expect_identical(private$.state$descriptive$fit,fit)
        for (i in seq_along(snapshots))
            expect_identical(analysis$results[[c("contributions","assessment")[[i]]]]$asDF,
                snapshots[[i]])
        expect_identical(lapply(analysis$results$contributionPlots$items,
            function(item) item$plot$state),plots)
        expect_identical(analysis$results$heatmap$state,heatmap)
        expect_identical(miso_table_note(analysis$results$assessment,"seed"),seedNote)
        expect_identical(miso_table_note(analysis$results$assessment,"scope"),scope)
        if (!details) {
            expect_false(analysis$results$detailsByContrast$visible)
            expect_false(analysis$results$detailsByContrast$visible)
        }
    }
    # Same-key refresh must restore cleared native warning/notes without a fit.
    analysis$results$assessment$setNote("seed",note=NULL)
    analysis$results$assessment$setNote("scope",note=NULL)
    simper_test_detail_table(analysis$results)$setNote("meaning",note=NULL)
    suppressWarnings(suppressMessages(analysis$run()))
    expect_identical(calls,2L)
    expect_identical(.Random.seed,rng)
    expect_identical(miso_table_note(analysis$results$assessment,"seed"),seedNote)
    expect_identical(miso_table_note(analysis$results$assessment,"scope"),scope)
    expect_match(miso_table_note(simper_test_detail_table(analysis$results),"meaning"),"SD is zero",fixed=TRUE)
    expected <- suppressMessages(original(simper_diagnostic_data()[c("x","y","z")],
        simper_diagnostic_data()$group,permutations=0L))
    expect_identical(fit,expected)
    # A positive mean / zero SD may retain a finite p, whereas 0/0 is missing.
    set.seed(123)
    assessment <- suppressMessages(original(simper_diagnostic_data()[c("x","y","z")],
        simper_diagnostic_data()$group,permutations=19))
    expect_true(is.finite(assessment[[1L]]$p[["x"]]))
    expect_true(is.na(assessment[[1L]]$p[["y"]]))
    expect_true(is.na(assessment[[1L]]$p[["z"]]))
    output <- analysis$results$assessment$asDF
    for (index in seq_along(assessment)) {
        p <- assessment[[index]]$p
        padj <- rep(NA_real_,length(p)); names(padj) <- names(p)
        finite <- is.finite(p)
        padj[finite] <- stats::p.adjust(p[finite],method="holm")
        rows <- output[output$contrast == private$.state$descriptive$contrastRows[[index]]$contrast,,drop=FALSE]
        expectedRows <- Filter(function(row) row$contrastIndex == index &&
            row$feature %in% names(p)[finite], private$.state$descriptive$displayRows)
        expect_identical(rows$feature, vapply(expectedRows,
            function(row) row$feature, character(1)))
        expect_equal(rows$p,unname(p[rows$feature]),tolerance=0)
        expect_equal(rows$padj,unname(padj[rows$feature]),tolerance=0)
    }
})

test_that("same-key refresh restores warning visibility and operational warnings persist", {
    data <- simper_diagnostic_data(TRUE)
    data <- rbind(data,transform(data[1L,], x=NA_real_))
    analysis <- simper_diagnostic_run(data,simperDetails=TRUE)
    original <- analysis$results$warnings$content
    expect_match(original,"rows excluded",fixed=TRUE)
    expect_match(original,"one between-group sample pair",fixed=TRUE)
    analysis$results$warnings$setContent("")
    analysis$results$warnings$setVisible(FALSE)
    suppressWarnings(suppressMessages(analysis$run()))
    expect_identical(analysis$results$warnings$content,original)
    expect_true(analysis$results$warnings$visible)
    for (details in c(FALSE,TRUE,FALSE)) {
        simper_diagnostic_set(analysis,"simperDetails",details)
        warning <- miso_squish_result(analysis$results$warnings)
        expect_match(warning,"rows excluded",fixed=TRUE)
        expect_identical(grepl("one between-group sample pair",warning,fixed=TRUE),details)
    }
    simper_diagnostic_set(analysis,"vars",character())
    expect_identical(analysis$results$warnings$content,"")
    expect_false(analysis$results$warnings$visible)
    expect_false(analysis$results$detailsByContrast$visible)
    simper_diagnostic_set(analysis,"vars",c("x","y","z"))
    expect_match(miso_squish_result(analysis$results$warnings),"rows excluded",fixed=TRUE)
})

test_that("assessment failure remains visible across detail toggles", {
    original <- vegan::simper
    testthat::local_mocked_bindings(simper=function(...,permutations=999) {
        if (permutations > 0) stop("test assessment failure")
        original(...,permutations=permutations)
    },.package="vegan")
    analysis <- simper_diagnostic_run(simperAssess=TRUE,simperN=19,seed=123)
    for (details in c(TRUE,FALSE,TRUE)) {
        simper_diagnostic_set(analysis,"simperDetails",details)
        expect_match(miso_squish_result(analysis$results$warnings),"test assessment failure",fixed=TRUE)
        expect_false(analysis$results$assessment$visible)
        expect_null(analysis$results$assessment$.__enclos_env__$private$.notes[["seed"]])
    }
})

test_that("diagnostic output survives R serialization and native save-load reruns", {
    skip_if_not_installed("RProtoBuf")
    RProtoBuf::readProtoFiles(file=system.file("jamovi.proto",package="jmvcore"))
    data <- simper_diagnostic_data(TRUE)
    settings <- list(vars=c("x","y","z"),factor="group",simperDetails=TRUE,
        simperAssess=TRUE,simperN=19,seed=123)
    original <- simperClass$new(options=do.call(simperOptions$new,settings),data=data,analysisId=1L)
    suppressWarnings(suppressMessages(original$run()))
    clone <- unserialize(serialize(original,NULL))
    simper_diagnostic_set(clone,"simperDetails",FALSE)
    expect_false(grepl("one between-group sample pair",
        miso_squish_result(clone$results$warnings),fixed=TRUE))
    simper_diagnostic_set(clone,"simperDetails",TRUE)
    expect_identical(clone$results$warnings$content,original$results$warnings$content)
    path <- tempfile(); on.exit(unlink(path),add=TRUE)
    original$.setStatePathSource(function() path)
    original$.save()
    restored <- simperClass$new(options=do.call(simperOptions$new,settings),data=data,analysisId=1L)
    restored$.setStatePathSource(function() path)
    restored$init(); restored$.load(); restored$postInit()
    suppressWarnings(suppressMessages(restored$run()))
    for (name in c("contributions","assessment")) {
        expect_false(is.null(original$results[[name]]))
        expect_false(is.null(restored$results[[name]]))
        expect_identical(restored$results[[name]]$asDF,original$results[[name]]$asDF)
    }
    originalDetails <- original$results$detailsByContrast$items
    restoredDetails <- restored$results$detailsByContrast$items
    expect_gt(length(originalDetails),0L)
    expect_identical(vapply(restoredDetails,function(table) table$key,character(1)),
        vapply(originalDetails,function(table) table$key,character(1)))
    for (i in seq_along(originalDetails))
        expect_identical(restoredDetails[[i]]$asDF,originalDetails[[i]]$asDF)
    expect_identical(restored$results$warnings$content,original$results$warnings$content)
    expect_identical(miso_table_note(simper_test_detail_table(restored$results),"meaning"),
        miso_table_note(simper_test_detail_table(original$results),"meaning"))
    expect_identical(miso_table_note(restored$results$assessment,"seed"),
        miso_table_note(original$results$assessment,"seed"))
    cells <- simper_test_detail_table(original$results)$asProtoBuf()$table$columns
    sd <- Filter(function(column) column$name == "sd",cells)[[1L]]
    expect_true(any(vapply(sd$cells,function(cell) cell$has("o"),logical(1))))
    expect_false(any(vapply(sd$cells,function(cell) cell$has("d") && !is.finite(cell$d),logical(1))))
})
