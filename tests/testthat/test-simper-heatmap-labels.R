heatmap_label_analysis <- function() {
    i <- seq_len(12L)
    data <- data.frame(x=1+i%%4, y=2+i%%5, z=3+i%%7,
        group=factor(rep(c("A", "B", "C"), each=4)))
    a <- simperClass$new(options=simperOptions$new(vars=c("x", "y", "z"),
        factor="group", simperHeatmap=TRUE, seed=123), data=data, analysisId=1L)
    suppressMessages(a$run())
    a
}

test_that("heatmap wrapping preserves full identities and Unicode graphemes", {
    a <- heatmap_label_analysis()
    values <- c("Short", "A feature with several words and a distinguishing suffix",
        paste0(strrep("shared_", 8), c("alpha", "beta")),
        "Feature  with   intentional spaces", "First line\nSecond line\n",
        paste0(strrep("e\u0301", 35), "_日本語"), "Rhytidoponera 'metallica'")
    dat <- expand.grid(feature=values, contrast=c("A vs B", "A vs C"),
        stringsAsFactors=FALSE)
    layout <- a$.__enclos_env__$private$.heatmapLayout(dat)
    expect_identical(names(layout$labels), values)
    expect_identical(gsub("\n", "", unname(layout$labels), fixed=TRUE),
        gsub("\n", "", values, fixed=TRUE))
    expect_identical(layout$labels[["First line\nSecond line\n"]], "First line\nSecond line\n")
    expect_false(any(grepl("\n\u0301", layout$labels, fixed=TRUE)))
    expect_length(unique(layout$labels), length(values))
    expect_true(all(nchar(unlist(strsplit(layout$labels, "\n", fixed=TRUE)), type="width") <= 30))
    expect_gt(layout$height, 500)
    expect_equal(a$results$heatmap$size, list(width=600L, height=500L))
})

test_that("heatmap sizing preserves scaled resources and invalidates changed layouts", {
    skip_if_not_installed("RProtoBuf")
    RProtoBuf::readProtoFiles(file=system.file("jamovi.proto", package="jmvcore"))
    a <- heatmap_label_analysis()
    private <- a$.__enclos_env__$private
    image <- a$results$heatmap
    for (dimension in c("width", "height")) {
        name <- paste("results", image$path, paste0(dimension, "Scale"), sep="/")
        a$options$.addOption(jmvcore::OptionNumber$new(name, value=1.25))
    }
    path <- tempfile(); file.create(path); on.exit(unlink(path), add=TRUE)
    image$.setPath(path)
    state <- image$state
    fit <- serialize(private$.state$descriptive, NULL)
    set.seed(321)
    rng <- .Random.seed
    private$.sizeHeatmap(state)
    expect_identical(image$size, list(width=750L, height=625L))
    expect_identical(image$asProtoBuf(status=jamovi.coms.AnalysisStatus$ANALYSIS_COMPLETE)$image$path, path)
    changed <- state
    changed$feature <- paste0(strrep("LongIdentifier", 15), changed$feature)
    image$setState(changed)
    private$.sizeHeatmap(changed)
    layout <- private$.heatmapLayout(changed)
    expect_identical(image$size, list(width=as.integer(round(layout$width*1.25)),
        height=as.integer(round(layout$height*1.25))))
    expect_identical(image$asProtoBuf(status=jamovi.coms.AnalysisStatus$ANALYSIS_COMPLETE)$image$path, "")
    image$.setPath(path)
    private$.sizeHeatmap(changed)
    expect_identical(image$asProtoBuf(status=jamovi.coms.AnalysisStatus$ANALYSIS_COMPLETE)$image$path, path)
    for (dimension in c("width", "height")) {
        name <- paste("results", image$path, paste0(dimension, "Scale"), sep="/")
        option <- a$options$option(name)
        option$value <- 0.001
    }
    private$.sizeHeatmap(state)
    expect_equal(image$size, list(width=32, height=32))
    private$.sizeHeatmap(changed)
    for (dimension in c("width", "height")) {
        name <- paste("results", image$path, paste0(dimension, "Scale"), sep="/")
        option <- a$options$option(name)
        option$value <- 1
    }
    expect_identical(image$size, list(width=layout$width, height=layout$height))
    expect_identical(serialize(private$.state$descriptive, NULL), fit)
    expect_identical(.Random.seed, rng)
})

test_that("saved heatmap restores dimensions labels and unchanged image resources", {
    skip_if_not_installed("RProtoBuf")
    RProtoBuf::readProtoFiles(file=system.file("jamovi.proto", package="jmvcore"))
    a <- heatmap_label_analysis()
    dat <- a$results$heatmap$state
    dat$feature <- paste0(strrep("A long feature name ", 3), dat$feature)
    a$results$heatmap$setState(dat)
    a$.__enclos_env__$private$.sizeHeatmap(dat)
    statePath <- tempfile(); imagePath <- tempfile(fileext=".png")
    on.exit(unlink(c(statePath, imagePath)), add=TRUE)
    grDevices::png(imagePath, width=a$results$heatmap$size$width,
        height=a$results$heatmap$size$height)
    a$.__enclos_env__$private$.plotHeatmap(a$results$heatmap)
    grDevices::dev.off()
    a$results$heatmap$.setPath(imagePath)
    a$.setStatePathSource(function() statePath); a$.save()
    restored <- simperClass$new(options=simperOptions$new(vars=c("x", "y", "z"),
        factor="group", simperHeatmap=TRUE), data=data.frame(x=numeric(), y=numeric(), z=numeric(), group=factor()), analysisId=1L)
    restored$.setStatePathSource(function() statePath); restored$init()
    restored$.__enclos_env__$private$.dataProvided <- FALSE
    restored$.load(); restored$postInit()
    expect_identical(restored$results$heatmap$size, a$results$heatmap$size)
    expect_identical(restored$results$heatmap$state, dat)
    expect_identical(restored$results$heatmap$asProtoBuf(status=jamovi.coms.AnalysisStatus$ANALYSIS_COMPLETE)$image$path, imagePath)
    expect_true(restored$results$heatmap$visible)
})
