simper_optional_data <- function() {
    i <- seq_len(12L)
    data.frame(x=1+i%%4, y=2+i%%5, z=3+i%%7,
        group=factor(rep(c("A","B","C"),each=4)))
}

simper_optional_run <- function(...) {
    analysis <- simperClass$new(options=simperOptions$new(
        vars=c("x","y","z"),factor="group",...),data=simper_optional_data(),analysisId=1L)
    suppressMessages(analysis$run())
    analysis
}

simper_optional_set <- function(analysis, name, value) {
    option <- analysis$options$option(name)
    option$value <- value
    analysis$optionsChangedHandler(name)
    suppressMessages(analysis$run())
}

test_that("SIMPER plot and value controls are independent with lazy tables", {
    for (details in c(FALSE,TRUE)) for (plot in c(FALSE,TRUE)) for (values in c(FALSE,TRUE)) {
        analysis <- simper_optional_run(simperPlots=plot,simperPlotValues=values,
            simperHeatmap=plot,simperHeatmapValues=values,simperDetails=details)
        result <- analysis$results
        expect_true(result$contributions$visible)
        expect_identical(result$contributionPlots$visible,plot || values)
        expect_identical(result$heatmap$visible,plot)
        expect_identical(result$heatmapValues$visible,values)
        expect_identical(nrow(result$heatmapValues$asDF)>0L,values)
        expect_length(result$contributionPlots$items,if(plot || values) 3L else 0L)
        for (item in result$contributionPlots$items) {
            expect_identical(item$plot$visible,plot)
            expect_identical(item$values$visible,values)
            expect_identical(nrow(item$values$asDF)>0L,values)
        }
        for (name in c("variability","means")) {
            expect_identical(result[[name]]$visible,details)
            expect_identical(length(result[[name]]$rowKeys)>0L,details)
        }
    }
    result <- simper(data=simper_optional_data(),vars=c("x","y","z"),factor="group")
    expect_error(result$table,"does not exist",fixed=TRUE)
})

test_that("SIMPER table toggles retain cells fit seeds and image resources", {
    original <- vegan::simper
    calls <- 0L
    testthat::local_mocked_bindings(simper=function(...) {
        calls <<- calls+1L
        original(...)
    },.package="vegan")
    analysis <- simper_optional_run(simperPlots=TRUE,simperHeatmap=TRUE,
        simperAssess=TRUE,simperN=19,seed=123)
    expect_identical(calls,2L)
    private <- analysis$.__enclos_env__$private
    fit <- serialize(private$.state$descriptive,NULL)
    rng <- .Random.seed
    contribution <- analysis$results$contributions$asDF
    assessment <- analysis$results$assessment$asDF
    note <- miso_table_note(analysis$results$assessment,"seed")
    items <- analysis$results$contributionPlots$items
    images <- c(lapply(items,function(item)item$plot),list(analysis$results$heatmap))
    paths <- vapply(images,function(image)tempfile(),character(1))
    on.exit(unlink(paths),add=TRUE)
    for (i in seq_along(images)) {
        file.create(paths[[i]])
        images[[i]]$.setPath(paths[[i]])
    }
    states <- lapply(images,function(image)serialize(image$state,NULL))
    for (option in c("simperDetails","simperPlotValues","simperHeatmapValues")) {
        simper_optional_set(analysis,option,TRUE)
        tables <- switch(option,simperDetails=list(analysis$results$variability,analysis$results$means),
            simperPlotValues=lapply(items,function(item)item$values),
            simperHeatmapValues=list(analysis$results$heatmapValues))
        cells <- lapply(tables,miso_table_first_cell)
        dfs <- lapply(tables,function(table)table$asDF)
        for (shown in c(FALSE,TRUE,FALSE,TRUE)) {
            simper_optional_set(analysis,option,shown)
            for(i in seq_along(tables)) {
                expect_identical(tables[[i]]$visible,shown)
                expect_identical(miso_table_first_cell(tables[[i]]),cells[[i]])
                expect_identical(tables[[i]]$asDF,dfs[[i]])
            }
        }
    }
    expect_identical(calls,2L)
    expect_identical(serialize(private$.state$descriptive,NULL),fit)
    expect_identical(.Random.seed,rng)
    expect_identical(analysis$results$contributions$asDF,contribution)
    expect_identical(analysis$results$assessment$asDF,assessment)
    expect_identical(miso_table_note(analysis$results$assessment,"seed"),note)
    for(i in seq_along(images)) {
        expect_identical(serialize(images[[i]]$state,NULL),states[[i]])
        expect_identical(images[[i]]$.__enclos_env__$private$.filePath,paths[[i]])
    }
    expect_identical(analysis$results$contributionPlots$items,items)
})

test_that("SIMPER bulk native rows match public API including protobuf and missing values", {
    skip_if_not_installed("RProtoBuf")
    RProtoBuf::readProtoFiles(file=system.file("jamovi.proto",package="jmvcore"))
    analysis <- simper_optional_run()
    writer <- analysis$.__enclos_env__$private$.populateRows
    schema <- yaml::read_yaml(miso_fixture_path("jamovi","simper.r.yaml"))$items
    by_name <- setNames(schema,vapply(schema,`[[`,character(1),"name"))
    shapes <- c(lapply(by_name[c("contrasts","contributions","variability","means","heatmapValues","assessment")],function(table)table$columns),
        list(plot=by_name$contributionPlots$template$items[[3L]]$columns))
    for (columns in shapes) {
        make <- function() jmvcore::Table$new(name="comparison",columns=columns)
        for(count in c(0L,1L,3L)) {
            rows <- lapply(seq_len(count),function(i) list(values=setNames(lapply(columns,function(column) {
                if(column$type=="text") paste0("value_",i)
                else if(i==2L) NA else if(column$type=="integer") as.integer(i) else i/3
            }),vapply(columns,`[[`,character(1),"name"))))
            fast <- make(); slow <- make()
            writer(fast,rows)
            for(i in seq_along(rows)) slow$addRow(rowKey=as.character(i),values=rows[[i]]$values)
            expect_identical(fast$asDF,slow$asDF)
            expect_identical(fast$rowKeys,slow$rowKeys)
            expect_identical(fast$asProtoBuf()$serialize(NULL),slow$asProtoBuf()$serialize(NULL))
            if(count>0L) {
                cell <- miso_table_first_cell(fast)
                writer(fast,rows)
                expect_identical(miso_table_first_cell(fast),cell)
            }
            writer(fast,list())
            expect_length(fast$rowKeys,0L)
            expect_length(fast$.__enclos_env__$private$.rowNames,0L)
        }
    }
    # Exercise the former slow scale without a wall-clock assertion.
    table <- jmvcore::Table$new(name="large",columns=shapes$variability)
    rows <- lapply(seq_len(990L),function(i)list(values=list(contrast="A vs B",feature=paste0("x",i),average=1,sd=NA,ratio=NA)))
    writer(table,rows)
    expect_equal(nrow(table$asDF),990L)
    expect_true(all(is.na(table$asDF$sd)))
})

test_that("optional SIMPER tables refresh across structural edits and invalid states", {
    analysis <- simper_optional_run(simperDetails=TRUE,simperPlotValues=TRUE,simperHeatmapValues=TRUE)
    before <- analysis$results$variability$asDF
    cell <- miso_table_first_cell(analysis$results$variability)
    analysis$.__enclos_env__$private$.data$x <- analysis$data$x+0.5
    suppressMessages(analysis$run())
    expect_false(identical(analysis$results$variability$asDF,before))
    expect_identical(miso_table_first_cell(analysis$results$variability),cell)
    simper_optional_set(analysis,"simperDetails",FALSE)
    analysis$.__enclos_env__$private$.data$z <- 0
    suppressMessages(analysis$run())
    simper_optional_set(analysis,"simperDetails",TRUE)
    expect_false("z" %in% analysis$results$variability$asDF$feature)
    for(item in analysis$results$contributionPlots$items)
        expect_false("z" %in% item$values$asDF$feature)
    analysis$.__enclos_env__$private$.data$group <- factor(rep(c("A","B"),each=6))
    suppressMessages(analysis$run())
    expect_length(analysis$results$contributionPlots$items,1L)
    simper_optional_set(analysis,"vars",character())
    for(name in c("variability","means","heatmapValues"))
        expect_miso_empty_table(analysis$results[[name]])
    expect_length(analysis$results$contributionPlots$items,0L)
})

test_that("SIMPER native save load restores independent outputs and stored seed", {
    skip_if_not_installed("RProtoBuf")
    RProtoBuf::readProtoFiles(file=system.file("jamovi.proto",package="jmvcore"))
    flags <- list(simperPlots=FALSE,simperPlotValues=TRUE,simperHeatmap=TRUE,
        simperHeatmapValues=FALSE,simperDetails=TRUE,simperAssess=TRUE,simperN=19,
        seed=0,useFixedSeed=FALSE)
    original <- do.call(simper_optional_run,flags)
    path <- tempfile();on.exit(unlink(path),add=TRUE)
    original$.setStatePathSource(function()path)
    original$.save()
    expect_gt(file.info(path)$size,0L)
    booleanFlags <- names(flags)[vapply(flags,is.logical,logical(1))]
    optionPB <- RProtoBuf::new(RProtoBuf::P("jamovi.coms.AnalysisOptions"))
    optionPB$hasNames <- TRUE
    optionPB$names <- booleanFlags
    optionPB$options <- lapply(booleanFlags,function(name) {
        option <- RProtoBuf::new(RProtoBuf::P("jamovi.coms.AnalysisOption"))
        option$o <- as.integer(flags[[name]])
        option
    })
    options <- simperOptions$new(vars=c("x","y","z"),factor="group",simperN=19)
    options$fromProtoBuf(RProtoBuf::read(RProtoBuf::P("jamovi.coms.AnalysisOptions"),optionPB$serialize(NULL)))
    for(name in booleanFlags) expect_identical(options[[name]],flags[[name]])
    restored <- simperClass$new(options=options,data=simper_optional_data(),analysisId=1L)
    restored$.setStatePathSource(function()path)
    restored$init();restored$.load();restored$postInit()
    suppressMessages(restored$run())
    for(name in c("contrasts","contributions","variability","means","assessment","heatmapValues")) {
        expect_identical(restored$results[[name]]$visible,original$results[[name]]$visible)
        expect_identical(restored$results[[name]]$asDF,original$results[[name]]$asDF)
    }
    # A fresh structural run reconstructs groups. Their saved image states
    # remain self-contained render sources; no full private fit is needed.
    private <- restored$.__enclos_env__$private
    private$.state$plotData <- NULL
    for(item in restored$results$contributionPlots$items) {
        file <- tempfile(fileext=".png")
        grDevices::png(file,width=580,height=430)
        private$.plotContribution(item$plot)
        grDevices::dev.off()
        expect_gt(file.info(file)$size,1000)
        unlink(file)
    }
    file <- tempfile(fileext=".png")
    grDevices::png(file,width=600,height=500)
    private$.plotHeatmap(restored$results$heatmap)
    grDevices::dev.off()
    expect_gt(file.info(file)$size,1000)
    unlink(file)
    expect_false(restored$results$contributionPlots$items[[1L]]$plot$visible)
    expect_true(restored$results$contributionPlots$items[[1L]]$values$visible)
    expect_identical(restored$results$seedState$state,original$results$seedState$state)
    expect_identical(miso_table_note(restored$results$assessment,"seed"),miso_table_note(original$results$assessment,"seed"))
})

test_that("native contribution array restore preserves images on value-option changes", {
    skip_if_not_installed("RProtoBuf")
    RProtoBuf::readProtoFiles(file=system.file("jamovi.proto",package="jmvcore"))
    analysis <- simper_optional_run(simperPlots=TRUE,simperPlotValues=TRUE)
    array <- analysis$results$contributionPlots
    images <- lapply(array$items,function(item)item$plot)
    paths <- vapply(images,function(image)tempfile(),character(1))
    on.exit(unlink(paths),add=TRUE)
    for(i in seq_along(images)) {
        file.create(paths[[i]])
        images[[i]]$.setPath(paths[[i]])
    }
    states <- lapply(images,function(image)image$state)
    pb <- array$asProtoBuf()
    for(image in images) {image$setState(NULL);image$.setPath(NULL)}
    array$fromProtoBuf(pb,oChanges="simperPlotValues",vChanges=character())
    for(i in seq_along(images)) {
        expect_identical(images[[i]]$state,states[[i]])
        expect_identical(images[[i]]$.__enclos_env__$private$.filePath,paths[[i]])
    }
})

test_that("header-only SIMPER recovers the supported heatmap image state", {
    skip_if_not_installed("RProtoBuf")
    RProtoBuf::readProtoFiles(file=system.file("jamovi.proto",package="jmvcore"))
    flags <- list(simperDetails=TRUE,simperHeatmap=TRUE,simperHeatmapValues=TRUE)
    original <- do.call(simper_optional_run,flags)
    path <- tempfile();on.exit(unlink(path),add=TRUE)
    original$.setStatePathSource(function()path)
    original$.save()
    restored <- simperClass$new(options=do.call(simperOptions$new,
        c(list(vars=c("x","y","z"),factor="group"),flags)),
        data=data.frame(x=numeric(),y=numeric(),z=numeric(),group=factor()),analysisId=1L)
    restored$.setStatePathSource(function()path)
    restored$init()
    restored$.__enclos_env__$private$.dataProvided <- FALSE
    restored$.load();restored$postInit()
    # Like dynamic arrays, native tables only restore Cells into existing
    # rows; new dynamic rows require the normal data-backed .run lifecycle.
    for(name in c("variability","means","heatmapValues"))
        expect_miso_empty_table(restored$results[[name]])
    expect_true(restored$results$heatmap$visible)
    expect_identical(restored$results$heatmap$state,original$results$heatmap$state)
    # Dynamic contribution groups require a normal data-backed structural run;
    # header-only recovery of those groups is not implemented by jmvcore Array.
    expect_length(restored$results$contributionPlots$items,0L)
})
