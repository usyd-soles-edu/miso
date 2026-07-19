cluster_state_data <- function(n=18L, long_labels=FALSE) {
    index <- seq_len(n)
    labels <- if (isTRUE(long_labels))
        paste0("Sample_", index, "_", paste(rep("VeryLongLabel", 4L), collapse=""))
    else
        paste0("Sample ", index)
    data.frame(
        feature_01=(index * 3L) %% 17L + 1,
        feature_02=(index * 5L) %% 19L + 1,
        feature_03=(index * 7L) %% 23L + 1,
        feature_04=(index * 11L) %% 29L + 1,
        sample=factor(labels))
}

cluster_yaml_node <- function(node, name) {
    if (is.list(node) && identical(node$name, name))
        return(node)
    if (is.list(node)) {
        for (child in node) {
            found <- cluster_yaml_node(child, name)
            if (! is.null(found))
                return(found)
        }
    }
    NULL
}

expect_cluster_visibility <- function(result, visible, hidden) {
    for (name in visible)
        expect_true(result[[name]]$visible, info=paste(name, "should be visible"))
    for (name in hidden)
        expect_false(result[[name]]$visible, info=paste(name, "should be hidden"))
}

run_cluster_private <- function(data, ...) {
    args <- list(...)
    options <- do.call(clusterOptions$new, args)
    analysis <- clusterClass$new(options=options, data=data)
    suppressWarnings(suppressMessages(analysis$run()))
    analysis
}

test_that("cluster schema stays teaching-focused", {
    analysis <- yaml::read_yaml(
        test_path("..", "..", "jamovi", "cluster.a.yaml"))
    ui <- yaml::read_yaml(
        test_path("..", "..", "jamovi", "cluster.u.yaml"))
    results <- yaml::read_yaml(
        test_path("..", "..", "jamovi", "cluster.r.yaml"))$items
    options <- analysis$options
    by_name <- setNames(options, vapply(options, `[[`, character(1), "name"))
    result_by_name <- setNames(
        results,
        vapply(results, `[[`, character(1), "name"))

    expect_identical(
        names(by_name),
        c("data", "vars", "labels", "transform", "distance", "showLabels"))
    expect_identical(by_name$vars$title, "Required: Feature variables")
    expect_identical(by_name$labels$title, "Optional: Sample labels")
    expect_identical(by_name$labels$permitted, c("numeric", "factor", "id"))
    expect_identical(by_name$transform$default, "none")
    expect_identical(by_name$distance$default, "bray")
    expect_true(by_name$showLabels$default)
    expect_null(by_name$linkage)
    expect_null(by_name$cutree)

    expect_false(cluster_yaml_node(ui, "analysisChoices")$collapsed)
    expect_false(cluster_yaml_node(ui, "outputChoices")$collapsed)
    expect_match(
        cluster_yaml_node(ui, "outputChoices")$children[[2L]]$children[[2L]]$label,
        "Row numbers")

    expect_true(all(vapply(
        results,
        function(item) identical(item$visible, FALSE),
        logical(1))))
    expect_identical(
        names(result_by_name),
        c("guidance", "summary", "warnings", "dendrogram", "interpretation", "settings"))
    expect_identical(result_by_name$dendrogram$type, "Image")
    expect_identical(result_by_name$dendrogram$width, 650L)
    expect_identical(result_by_name$dendrogram$height, 480L)
})

test_that("new cluster analysis shows only complete getting-started guidance", {
    options <- clusterOptions$new(vars=character())
    analysis <- clusterClass$new(options=options, data=cluster_state_data())
    analysis$.__enclos_env__$private$.run()
    result <- analysis$results

    expect_match(as.character(result$guidance$asString()), "max-width: 44em", fixed=TRUE)
    expect_match(as.character(result$guidance$asString()), "groups samples with similar")
    expect_match(as.character(result$guidance$asString()), "Feature variables")
    expect_cluster_visibility(
        result,
        visible="guidance",
        hidden=c("summary", "warnings", "dendrogram", "interpretation", "settings"))
})

test_that("successful cluster analysis shows no empty result shells", {
    result <- cluster(
        data=cluster_state_data(),
        vars=paste0("feature_0", 1:4),
        labels="sample")

    expect_cluster_visibility(
        result,
        visible=c("summary", "dendrogram", "interpretation", "settings"),
        hidden=c("guidance", "warnings"))
    expect_equal(nrow(result$summary$asDF), 5L)
    expect_equal(nrow(result$settings$asDF), 5L)
    expect_match(
        tofu_squish_result(result$interpretation),
        "lower branch heights are more similar")
    expect_match(
        tofu_squish_result(result$interpretation),
        "ANOSIM or PERMANOVA")
})

test_that("cluster analysis matches vegdist and group-average hclust", {
    for (n in c(12L, 120L)) {
        data <- cluster_state_data(n)
        vars <- paste0("feature_0", 1:4)
        analysis <- run_cluster_private(
            data,
            vars=vars,
            labels="sample",
            transform="fourthroot",
            distance="bray",
            showLabels=n < 60L)
        actual <- analysis$.__enclos_env__$private$.state$fit
        expected <- stats::hclust(
            vegan::vegdist(data[, vars, drop=FALSE] ^ 0.25, method="bray"),
            method="average")

        expect_identical(actual$merge, expected$merge, info=paste("n =", n))
        expect_equal(actual$height, expected$height, tolerance=1e-12, info=paste("n =", n))
        expect_identical(actual$order, expected$order, info=paste("n =", n))
        expect_identical(actual$method, "average")
        expect_identical(actual$dist.method, "bray")
    }
})

test_that("cluster transformation paths match independent vegan fits", {
    data <- cluster_state_data(20L)
    vars <- paste0("feature_0", 1:4)
    community <- data[, vars, drop=FALSE]
    transformed <- list(
        none=community,
        sqrt=sqrt(community),
        fourthroot=community ^ 0.25,
        log=log(community + 1),
        pa=vegan::decostand(community, method="pa"))

    for (transform in names(transformed)) {
        analysis <- run_cluster_private(
            data,
            vars=vars,
            transform=transform,
            distance="bray")
        actual <- analysis$.__enclos_env__$private$.state$fit
        expected <- stats::hclust(
            vegan::vegdist(transformed[[transform]], method="bray"),
            method="average")

        expect_identical(actual$merge, expected$merge, info=transform)
        expect_equal(actual$height, expected$height, tolerance=1e-12, info=transform)
    }
})

test_that("sample labels are descriptive and never change the clustering", {
    data <- cluster_state_data(18L)
    data$sample <- as.character(data$sample)
    data$sample[c(2L, 3L)] <- "Repeated"
    data$sample[[4L]] <- NA_character_
    data$sample <- factor(data$sample)
    vars <- paste0("feature_0", 1:4)

    labelled <- run_cluster_private(
        data,
        vars=vars,
        labels="sample",
        showLabels=TRUE)
    unlabelled <- run_cluster_private(
        data,
        vars=vars,
        labels=NULL,
        showLabels=FALSE)
    labelled_private <- labelled$.__enclos_env__$private
    unlabelled_private <- unlabelled$.__enclos_env__$private

    expect_identical(labelled_private$.state$fit$merge, unlabelled_private$.state$fit$merge)
    expect_equal(labelled_private$.state$fit$height, unlabelled_private$.state$fit$height)
    expect_match(tofu_squish_result(labelled$results$warnings), "missing sample label")
    expect_match(tofu_squish_result(labelled$results$warnings), "Duplicate sample labels")
    expect_true(any(grepl("\\[row [23]\\]", labelled_private$.state$labels)))
    expect_true("Row 4" %in% labelled_private$.state$labels)
})

test_that("large labelled analyses give one actionable display warning", {
    shown <- cluster(
        data=cluster_state_data(80L),
        vars=paste0("feature_0", 1:4),
        labels="sample",
        showLabels=TRUE)
    hidden <- cluster(
        data=cluster_state_data(80L),
        vars=paste0("feature_0", 1:4),
        labels="sample",
        showLabels=FALSE)

    expect_match(tofu_squish_result(shown$warnings), "more than 60 samples")
    expect_false(hidden$warnings$visible)
})

test_that("cluster dendrogram rendering is bounded and read-only", {
    analysis <- run_cluster_private(
        cluster_state_data(35L, long_labels=TRUE),
        vars=paste0("feature_0", 1:4),
        labels="sample")
    private <- analysis$.__enclos_env__$private
    before <- unserialize(serialize(private$.state, NULL))
    bounded <- vapply(
        private$.state$labels,
        private$.boundedPlotLabel,
        character(1))
    path <- tempfile(fileext=".png")
    on.exit(unlink(path), add=TRUE)

    grDevices::png(path, width=900, height=700)
    private$.plotDendrogram(NULL)
    grDevices::dev.off()

    expect_gt(file.info(path)$size, 0)
    expect_true(all(nchar(bounded) <= 24L))
    expect_true(any(grepl("\u2026", bounded, fixed=TRUE)))
    expect_match(
        tofu_squish_result(analysis$results$warnings),
        "shortened with an ellipsis")
    expect_identical(private$.state, before)
})

test_that("cluster valid invalid valid transitions clear stale state", {
    data <- cluster_state_data()
    options <- clusterOptions$new(vars=paste0("feature_0", 1:4))
    analysis <- clusterClass$new(options=options, data=data)

    suppressWarnings(suppressMessages(analysis$run()))
    expect_true(analysis$results$dendrogram$visible)
    expect_false(is.null(analysis$.__enclos_env__$private$.state$fit))

    vars_option <- options$option("vars")
    vars_option$.__enclos_env__$private$.value <- character()
    analysis$run()
    expect_true(analysis$results$guidance$visible)
    expect_false(analysis$results$dendrogram$visible)
    expect_equal(length(analysis$results$summary$rowKeys), 0L)
    expect_null(analysis$.__enclos_env__$private$.state$fit)

    vars_option$.__enclos_env__$private$.value <- paste0("feature_0", 1:4)
    suppressWarnings(suppressMessages(analysis$run()))
    expect_false(analysis$results$guidance$visible)
    expect_true(analysis$results$dendrogram$visible)
    expect_false(is.null(analysis$.__enclos_env__$private$.state$fit))
})
