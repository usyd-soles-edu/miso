simper_optional_data <- function() {
    i <- seq_len(12L)
    data.frame(x=1+i%%4, y=2+i%%5, z=3+i%%7,
        group=factor(rep(c("A","B","C"),each=4)))
}
simper_optional_run <- function(...) {
    analysis <- simperClass$new(options=simperOptions$new(
        vars=c("x","y","z"),factor="group",...), data=simper_optional_data(),analysisId=1L)
    suppressMessages(analysis$run()); analysis
}
simper_optional_set <- function(analysis,name,value) {
    option <- analysis$options$option(name); option$value <- value
    analysis$optionsChangedHandler(name); suppressMessages(analysis$run())
}

test_that("SIMPER selected details and plots are independent native outputs", {
    for (details in c(FALSE,TRUE)) for (plots in c(FALSE,TRUE)) {
        a <- simper_optional_run(simperDetails=details,simperPlots=plots,simperHeatmap=plots,simperTop=1)
        expect_identical(a$results$detailsByContrast$visible,details)
        expect_identical(a$results$contributionPlots$visible,plots)
        expect_identical(a$results$heatmap$visible,plots)
        expect_length(a$results$detailsByContrast$items,if(details)3L else 0L)
        expect_length(a$results$contributionPlots$items,if(plots)3L else 0L)
        for (table in a$results$detailsByContrast$items) {
            expect_equal(nrow(table$asDF),1L)
            expect_match(miso_table_note(table,"meaning"),"Top N",fixed=TRUE)
        }
        for(name in c("table","variability","means","heatmapValues"))
            expect_null(a$results[[name]])
        for(item in a$results$contributionPlots$items)
            expect_error(item$values,"does not exist",fixed=TRUE)
    }
    schema <- yaml::read_yaml(miso_fixture_path("jamovi","simper.a.yaml"))
    names <- vapply(schema$options,`[[`,character(1),"name")
    expect_false(any(c("simperPlotValues","simperHeatmapValues") %in% names))
})

test_that("SIMPER detail rows match selected full statistics without renormalising", {
    for(top in c(1,3)) for(cum in c(50,100)) {
        a <- simper_optional_run(simperDetails=TRUE,simperTop=top,simperCum=cum)
        private <- a$.__enclos_env__$private
        selected <- private$.state$descriptive$displayRows
        full <- private$.state$descriptive$fullRows
        actual <- simper_test_detail_frame(a$results)
        expect_equal(nrow(actual),length(selected))
        expect_equal(nrow(actual),nrow(a$results$contributions$asDF))
        for(i in seq_along(selected)) {
            row <- selected[[i]]
            expect_identical(actual$feature[[i]],row$feature)
            expect_identical(actual$contrast[[i]],row$contrast)
            for(name in c("meanFirst","meanSecond","average","sd","ratio"))
                expect_equal(actual[[name]][[i]],row[[name]])
            reference <- Filter(function(x) x$contrastIndex==row$contrastIndex && x$feature==row$feature,full)[[1L]]
            expect_identical(row,reference)
        }
        expect_equal(sum(vapply(full,function(x)x$contribution,numeric(1))),300)
        expect_lte(max(table(actual$contrast)),top)
    }
})

test_that("detail visibility uses cached statistics seeds and image resources", {
    original <- vegan::simper; calls <- 0L
    testthat::local_mocked_bindings(simper=function(...) {
        calls <<- calls+1L; original(...)
    },.package="vegan")
    a <- simper_optional_run(simperPlots=TRUE,simperHeatmap=TRUE,simperAssess=TRUE,simperN=19,seed=123)
    fit <- serialize(a$.__enclos_env__$private$.state$descriptive,NULL)
    rng <- .Random.seed; values <- a$results$contributions$asDF
    assessment <- a$results$assessment$asDF
    images <- c(lapply(a$results$contributionPlots$items,function(x)x$plot),list(a$results$heatmap))
    paths <- vapply(images,function(x)tempfile(),character(1)); on.exit(unlink(paths),add=TRUE)
    for(i in seq_along(images)) { file.create(paths[[i]]);images[[i]]$.setPath(paths[[i]]) }
    states <- lapply(images,function(x)serialize(x$state,NULL))
    simper_optional_set(a,"simperDetails",TRUE)
    tables <- a$results$detailsByContrast$items
    cells <- lapply(tables,miso_table_first_cell)
    for(shown in c(FALSE,TRUE,FALSE,TRUE)) {
        simper_optional_set(a,"simperDetails",shown)
        expect_identical(a$results$detailsByContrast$visible,shown)
        expect_identical(a$results$detailsByContrast$items,tables)
        expect_identical(lapply(tables,miso_table_first_cell),cells)
    }
    expect_identical(calls,2L)
    expect_identical(serialize(a$.__enclos_env__$private$.state$descriptive,NULL),fit)
    expect_identical(.Random.seed,rng)
    expect_identical(a$results$contributions$asDF,values)
    expect_identical(a$results$assessment$asDF,assessment)
    expect_identical(lapply(images,function(x)serialize(x$state,NULL)),states)
    expect_identical(vapply(images,function(x)x$.__enclos_env__$private$.filePath,character(1)),paths)
})

test_that("detail headers retain actual groups and full feature identities", {
    d <- simper_optional_data(); labels <- c('A vs B','C "quoted"','δ group')
    d$group <- factor(rep(labels,each=4))
    long <- paste(rep("Long taxon",8),collapse=" ");names(d)[1] <- long
    a <- simperClass$new(options=simperOptions$new(vars=c(long,'y','z'),factor='group',simperDetails=TRUE,simperTop=3,simperCum=100),data=d)
    suppressMessages(a$run())
    for(table in a$results$detailsByContrast$items) {
        expect_true(long %in% table$asDF$feature)
        row <- Filter(function(row)as.character(row$contrastIndex)==table$key,a$.__enclos_env__$private$.state$descriptive$displayRows)[[1L]]
        expect_identical(table$getColumn('meanFirst')$title,paste0(row$firstGroup,' mean'))
        expect_identical(table$getColumn('meanSecond')$title,paste0(row$secondGroup,' mean'))
        expect_identical(table$title,row$contrast)
    }
})

test_that("structural edits refresh bounded detail rows and discard stale output", {
    a <- simper_optional_run(simperDetails=TRUE,simperTop=1)
    before <- simper_test_detail_frame(a$results)
    a$.__enclos_env__$private$.data$x <- a$data$x+0.5;suppressMessages(a$run())
    expect_false(identical(simper_test_detail_frame(a$results),before))
    simper_optional_set(a,'simperTop',3);simper_optional_set(a,'simperCum',100)
    expect_equal(nrow(simper_test_detail_frame(a$results)),9L)
    a$.__enclos_env__$private$.data$group <- factor(rep(c('C','A'),each=6));suppressMessages(a$run())
    expect_length(a$results$detailsByContrast$items,1L)
    expect_identical(a$results$detailsByContrast$items[[1]]$title,'C vs A')
    simper_optional_set(a,'vars',character())
    expect_length(a$results$detailsByContrast$items,0L)
    recovered <- simper_optional_data(); recovered$group <- factor(rep(c('C','A'),each=6))
    a$.__enclos_env__$private$.data <- recovered
    simper_optional_set(a,'vars',c('x','y'))
    expect_length(a$results$detailsByContrast$items,1L)
    expect_equal(nrow(simper_test_detail_frame(a$results)),2L)
})

test_that("new saved results restore selected details images and effective seed", {
    skip_if_not_installed('RProtoBuf')
    RProtoBuf::readProtoFiles(file=system.file('jamovi.proto',package='jmvcore'))
    flags <- list(vars=c('x','y','z'),factor='group',simperDetails=TRUE,simperPlots=TRUE,simperHeatmap=TRUE,simperAssess=TRUE,simperN=19,seed=123)
    original <- simperClass$new(options=do.call(simperOptions$new,flags),data=simper_optional_data(),analysisId=1L)
    path <- tempfile();on.exit(unlink(path),add=TRUE);original$.setStatePathSource(function()path)
    suppressMessages(original$run());original$.save()
    restored <- simperClass$new(options=do.call(simperOptions$new,flags),data=simper_optional_data(),analysisId=1L)
    restored$.setStatePathSource(function()path);restored$init();restored$.load();restored$postInit();suppressMessages(restored$run())
    expect_equal(simper_test_detail_frame(restored$results),simper_test_detail_frame(original$results))
    expect_identical(restored$results$heatmap$state,original$results$heatmap$state)
    expect_identical(miso_table_note(restored$results$assessment,'seed'),miso_table_note(original$results$assessment,'seed'))
    expect_identical(lapply(restored$results$contributionPlots$items,function(x)x$plot$state),lapply(original$results$contributionPlots$items,function(x)x$plot$state))
})

test_that("bounded SIMPER details retain native schema and default disclosure", {
    options <- yaml::read_yaml(miso_fixture_path('jamovi','simper.a.yaml'))$options
    detail <- Filter(function(x)identical(x$name,'simperDetails'),options)[[1L]]
    expect_false(detail$default)
    ui <- yaml::read_yaml(miso_fixture_path('jamovi','simper.u.yaml'))
    tables <- Filter(function(x)identical(x$name,'tables'),ui$children)[[1L]]
    expect_true(tables$collapsed)
    expect_identical(vapply(tables$children,`[[`,character(1),'name'),'simperDetails')
    results <- yaml::read_yaml(miso_fixture_path('jamovi','simper.r.yaml'))$items
    detail <- Filter(function(x)identical(x$name,'detailsByContrast'),results)[[1L]]
    expect_identical(detail$type,'Array')
    expect_identical(detail$template$type,'Table')
    expect_identical(vapply(detail$template$columns,`[[`,character(1),'name'),
        c('feature','meanFirst','meanSecond','average','sd','ratio'))
})

test_that("diagnostics on omitted features stay cached without displayed warnings", {
    original <- vegan::simper
    testthat::local_mocked_bindings(simper=function(...) {
        fit <- original(...)
        i <- which.min(fit[[1]]$average)
        fit[[1]]$sd[[i]] <- NA_real_;fit[[1]]$ratio[[i]] <- NA_real_
        fit
    },.package='vegan')
    a <- simper_optional_run(simperDetails=TRUE,simperTop=1,simperCum=100)
    diagnostics <- a$.__enclos_env__$private$.state$descriptive$diagnostics
    expect_true(any(vapply(diagnostics,function(x)x$reason=='unexpected',logical(1))))
    expect_false(a$results$warnings$visible)
    expect_false(grepl('Other blank',miso_table_note(simper_test_detail_table(a$results),'meaning'),fixed=TRUE))
    simper_optional_set(a,'simperTop',3)
    expect_match(miso_squish_result(a$results$warnings),'1 feature row',fixed=TRUE)
    expect_match(miso_table_note(simper_test_detail_table(a$results),'meaning'),'Other blank',fixed=TRUE)
})

test_that("header-only SIMPER restores and renders the retained heatmap state", {
    skip_if_not_installed('RProtoBuf')
    RProtoBuf::readProtoFiles(file=system.file('jamovi.proto',package='jmvcore'))
    original <- simper_optional_run(simperDetails=TRUE,simperHeatmap=TRUE)
    path <- tempfile();pngPath <- tempfile(fileext='.png');on.exit(unlink(c(path,pngPath)),add=TRUE)
    original$.setStatePathSource(function()path);original$.save()
    restored <- simperClass$new(options=simperOptions$new(vars=c('x','y','z'),factor='group',simperDetails=TRUE,simperHeatmap=TRUE),
        data=simper_optional_data()[0,],analysisId=1L)
    restored$.setStatePathSource(function()path);restored$init()
    restored$.__enclos_env__$private$.dataProvided <- FALSE
    restored$.load();restored$postInit()
    expect_true(restored$results$heatmap$visible)
    expect_identical(restored$results$heatmap$state,original$results$heatmap$state)
    grDevices::png(pngPath);restored$.__enclos_env__$private$.plotHeatmap(restored$results$heatmap);grDevices::dev.off()
    expect_gt(file.info(pngPath)$size,0L)
    # Native dynamic Arrays require a normal data-backed run to reconstruct items.
    expect_length(restored$results$detailsByContrast$items,0L)
})

test_that("contrast detail identities do not depend on presentation labels", {
    a <- simper_optional_run(simperTop=1)
    private <- a$.__enclos_env__$private
    descriptive <- private$.state$descriptive
    descriptive$displayRows <- lapply(descriptive$displayRows,function(row) { row$contrast <- 'Same label';row })
    private$.populateOptionalDetailRows(descriptive)
    tables <- a$results$detailsByContrast$items
    expect_length(tables,3L)
    expect_length(unique(vapply(tables,function(x)x$key,character(1))),3L)
    expect_true(all(vapply(tables,function(x)x$title=='Same label',logical(1))))
    expect_equal(sum(vapply(tables,function(x)nrow(x$asDF),integer(1))),3L)
})
