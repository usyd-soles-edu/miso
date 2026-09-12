startup_cases <- list(
    permanova=list(core="table", extra=list(permN=19, seed=123),
        enabled=list(permPairwise=TRUE, showCompanionPcoa=TRUE, pcoaCentroids=TRUE),
        optional=c("pairwise", "companionPcoaSites", "companionPcoaCentroids")),
    anosim=list(core=c("global", "rankSummary"), extra=list(anosimN=19, seed=123),
        enabled=list(anosimPairwise=TRUE), optional="pairwise"),
    permdisp=list(core=c("anova", "distances"), extra=list(dispN=19, seed=123),
        enabled=list(dispPairwise=TRUE, showOrdinationPlot=TRUE),
        optional=c("pairwise", "ordinationScores")),
    simper=list(core=c("contrasts", "contributions"), extra=list(),
        enabled=list(simperDetails=TRUE, simperAssess=TRUE, simperHeatmap=TRUE),
        optional=c("variability", "means", "assessment", "heatmapValues")),
    nmds=list(core=c("sites", "stress", "shepardPairs"),
        extra=list(seed=123, nmdsTrymax=5),
        enabled=list(nmdsSpecies=TRUE, nmdsEnv="x"), optional=c("features", "envfit")),
    pcoa=list(core=c("sites", "eigenvalues"), extra=list(),
        enabled=list(showCentroids=TRUE), optional="centroids"),
    cluster=list(core="dendrogramStructure", extra=list(),
        enabled=list(defineClusters=TRUE), optional="membership"))

startup_data <- function() {
    data.frame(x=c(2,4,1,6,3,5,8,9,7), y=c(8,5,7,2,9,1,4,3,6),
        z=c(1,8,4,7,2,9,3,6,5), group=factor(rep(c("A","B","C"), each=3)))
}

startup_analysis <- function(name, enabled=FALSE, populated=FALSE) {
    args <- c(list(vars=if (populated) c("x","y","z") else character()),
        startup_cases[[name]]$extra,
        if (enabled) startup_cases[[name]]$enabled else list())
    if (populated && name %in% c("permanova","anosim","permdisp","simper"))
        args$factor <- "group"
    options <- do.call(get(paste0(name,"Options"))$new, args)
    get(paste0(name,"Class"))$new(options=options, data=startup_data())
}

startup_run <- function(analysis) {
    suppressWarnings(suppressMessages(analysis$.__enclos_env__$private$.run()))
}

startup_set <- function(analysis, name, value) {
    option <- analysis$options$option(name)
    option$.__enclos_env__$private$.value <- value
}

for (analysisName in names(startup_cases)) local({
    name <- analysisName
    test_that(paste(name, "starts with native tables and no tutorial"), {
        for (enabled in c(FALSE, TRUE)) {
            analysis <- startup_analysis(name, enabled)
            analysis$.__enclos_env__$private$.init()
            for (table in startup_cases[[name]]$core)
                expect_true(analysis$results[[table]]$visible, info=paste("initial",table))
            startup_run(analysis)
            result <- analysis$results
            expect_false(result$guidance$visible)
            expect_identical(result$guidance$content, "")
            tables <- c(startup_cases[[name]]$core,
                if (enabled) startup_cases[[name]]$optional)
            for (table in tables) {
                expect_true(result[[table]]$visible, info=table)
                expect_true(nrow(result[[table]]$asDF) >= 1L, info=paste(table,"footer refresh"))
                expect_false(any(grepl("To run|Results update|Getting started",
                    as.character(result[[table]]$asString()))))
            }
            # A cached rerun must retain the same empty result contract.
            startup_run(analysis)
            expect_false(result$guidance$visible)
            for (table in tables) expect_true(result[[table]]$visible, info=table)
        }
    })
    test_that(paste(name, "clears fitted output when required inputs are removed"), {
        analysis <- startup_analysis(name, populated=TRUE)
        startup_run(analysis)
        expect_false(analysis$results$guidance$visible)
        fitted <- lapply(startup_cases[[name]]$core,
            function(table) analysis$results[[table]]$asDF)
        startup_set(analysis, "vars", character())
        if (name %in% c("permanova","anosim","permdisp","simper"))
            startup_set(analysis, "factor", NULL)
        startup_run(analysis)
        expect_false(analysis$results$guidance$visible)
        for (table in startup_cases[[name]]$core) {
            expect_true(analysis$results[[table]]$visible, info=table)
            expect_length(miso_table_note(analysis$results[[table]]), 0L)
            cells <- unlist(analysis$results[[table]]$asDF, use.names=FALSE)
            expect_true(all(is.na(cells) | cells == ""), info=paste(table,"stale values"))
        }
        startup_set(analysis, "vars", c("x","y","z"))
        if (name %in% c("permanova","anosim","permdisp","simper"))
            startup_set(analysis, "factor", "group")
        startup_run(analysis)
        expect_false(analysis$results$guidance$visible)
        expect_equal(lapply(startup_cases[[name]]$core,
            function(table) analysis$results[[table]]$asDF), fitted)
    })
})

test_that("partly specified grouped analyses identify only the missing requirement", {
    for (name in c("permanova","anosim","permdisp","simper")) {
        analysis <- startup_analysis(name)
        startup_set(analysis,"factor","group")
        startup_run(analysis)
        expect_true(analysis$results$guidance$visible)
        expect_match(analysis$results$guidance$content,"Feature variables")
        expect_false(grepl("Grouping variable", analysis$results$guidance$content))
        startup_set(analysis,"factor",NULL)
        startup_set(analysis,"vars",c("x","y","z"))
        startup_run(analysis)
        expect_true(analysis$results$guidance$visible)
        expect_match(analysis$results$guidance$content,"Grouping variable")
    }
})


test_that("optional empty tables track toggles before and after fitting", {
    toggles <- list(
        permanova=list(option="showCompanionPcoa", tables="companionPcoaSites"),
        anosim=list(option="anosimPairwise", tables="pairwise"),
        permdisp=list(option="showOrdinationPlot", tables="ordinationScores"),
        simper=list(option="simperDetails", tables=c("variability","means")),
        nmds=list(option="nmdsShepard", tables="shepardPairs"),
        pcoa=list(option="showCentroids", tables="centroids"),
        cluster=list(option="defineClusters", tables="membership"))
    for (name in names(toggles)) {
        analysis <- startup_analysis(name)
        for (populated in c(FALSE, TRUE)) {
            if (populated) {
                startup_set(analysis,"vars",c("x","y","z"))
                if (name %in% c("permanova","anosim","permdisp","simper"))
                    startup_set(analysis,"factor","group")
                startup_run(analysis)
                startup_set(analysis,"vars",character())
                if (name %in% c("permanova","anosim","permdisp","simper"))
                    startup_set(analysis,"factor",NULL)
            }
            for (shown in c(TRUE,FALSE,TRUE)) {
                startup_set(analysis,toggles[[name]]$option,shown)
                startup_run(analysis)
                expect_false(analysis$results$guidance$visible)
                for (table in toggles[[name]]$tables)
                    expect_identical(analysis$results[[table]]$visible, shown,
                        info=paste(name,table,populated,shown))
            }
        }
    }
})

test_that("initialisation preserves fitted tables and notes on an existing analysis", {
    for (name in names(startup_cases)) {
        analysis <- startup_analysis(name, enabled=TRUE, populated=TRUE)
        if (name == "simper") startup_set(analysis, "simperN", 19)
        startup_run(analysis)
        tables <- c(startup_cases[[name]]$core, startup_cases[[name]]$optional)
        snapshot <- function() lapply(tables, function(table) {
            result <- analysis$results[[table]]
            list(visible=result$visible, values=result$asDF,
                notes=miso_table_note(result))
        })
        before <- snapshot()
        analysis$.__enclos_env__$private$.init()
        expect_identical(snapshot(), before, info=name)
    }
})

test_that("empty numeric cells serialize as missing rather than NaN", {
    skip_if_not_installed("RProtoBuf")
    RProtoBuf::readProtoFiles(file=system.file("jamovi.proto", package="jmvcore"))
    analysis <- startup_analysis("anosim")
    startup_run(analysis)
    table <- analysis$results$global$asProtoBuf()$table
    for (column in table$columns) {
        if (column$name == "statistic") next
        cell <- column$cells[[1L]]
        expect_true(cell$has("o"), info=column$name)
        expect_false(cell$has("d"), info=column$name)
    }
})
