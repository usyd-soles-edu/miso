plot_contract_fixture <- function(analysis, suffix) {
    file.path(testthat::test_path("..", ".."), "jamovi",
        paste0(analysis, ".", suffix, ".yaml"))
}

# Task 6 source contracts: user-facing labels and native result titles are
# deliberately exact so later wording changes cannot reintroduce clutter.
test_that("Task 6 uses exact title-case labels and noun-phrase controls", {
    expected_targets <- list(
        permanova=c(vars="Feature Variables", factor="Grouping Variable",
            permFactors="Additional Factors", strata="Blocking Variable",
            covariates="Continuous Covariates"),
        anosim=c(vars="Feature Variables", factor="Grouping Variable",
            strata="Blocking Variable"),
        permdisp=c(vars="Feature Variables", factor="Grouping Variable"),
        simper=c(vars="Feature Variables", factor="Grouping Variable"),
        nmds=c(vars="Feature Variables", factor="Grouping Variable",
            nmdsEnv="Environmental Variables"),
        cluster=c(vars="Feature Variables", labels="Sample Labels"),
        pcoa=c(vars="Feature Variables", factor="Grouping Variable"))
    expected_controls <- c(
        nmdsShepard="Shepard Diagram", nmdsSpecies="Feature Scores",
        showCentroids="Group Centroids", pcoaCentroids="Group Centroids",
        showCompanionPcoa="Companion PCoA", permInteractions="Model Interactions",
        simperDetails="Detailed Statistics")
    find_node <- function(node, name) {
        if (is.list(node) && identical(node$name, name)) return(node)
        if (is.list(node)) for (child in node) {
            found <- find_node(child, name)
            if (!is.null(found)) return(found)
        }
        NULL
    }
    for (analysis in names(expected_targets)) {
        a <- yaml::read_yaml(plot_contract_fixture(analysis, "a"))
        u <- yaml::read_yaml(plot_contract_fixture(analysis, "u"))
        options <- setNames(a$options,
            vapply(a$options, `[[`, character(1), "name"))
        for (name in names(expected_targets[[analysis]])) {
            expect_identical(options[[name]]$title,
                unname(expected_targets[[analysis]][[name]]),
                info=paste(analysis, name, "analysis title"))
            node <- find_node(u, name)
            expect_false(is.null(node), info=paste(analysis, name, "UI node"))
            expect_false(grepl("\\((required|optional)\\)$",
                unname(expected_targets[[analysis]][[name]]), ignore.case=TRUE))
        }
    }
    for (name in names(expected_controls)) {
        analysis <- switch(name, nmdsShepard="nmds", nmdsSpecies="nmds",
            showCentroids="pcoa", pcoaCentroids="permanova", showCompanionPcoa="permanova",
            permInteractions="permanova", simperDetails="simper")
        node <- find_node(
            yaml::read_yaml(plot_contract_fixture(analysis, "u")), name)
        expect_identical(node$type, "CheckBox")
        option <- Filter(function(x) identical(x$name, name),
            yaml::read_yaml(plot_contract_fixture(analysis, "a"))$options)
        expect_length(option, 1L)
        expect_identical(option[[1L]]$title, unname(expected_controls[[name]]))
    }
})

test_that("Task 6 result schemas use native titles and omit clutter sections", {
    for (analysis in c("permanova", "anosim", "permdisp", "nmds",
                       "pcoa", "cluster", "simper")) {
        items <- yaml::read_yaml(plot_contract_fixture(analysis, "r"))$items
        names <- vapply(items, `[[`, character(1), "name")
        expect_false(any(grepl("^(summary|settings|note|interpretation)", names)),
            info=paste(analysis, "clutter result names"))
        expect_false(any(grepl("^(Data Summary|Analysis Settings|How to read)",
            vapply(items, function(x) if (is.null(x$title)) "" else x$title,
                character(1)), ignore.case=FALSE)), info=analysis)
        tables <- items[vapply(items, function(x) identical(x$type, "Table"),
            logical(1))]
        expect_true(all(nzchar(vapply(tables, `[[`, character(1), "title"))),
            info=paste(analysis, "native table titles"))
    }
})

collect_plot_contracts <- function(items, prefix="", ancestorHidden=FALSE) {
    found <- list()
    if (is.null(items))
        return(found)
    for (index in seq_along(items)) {
        item <- items[[index]]
        name <- if (is.null(item$name)) paste0("item", index) else item$name
        path <- if (nzchar(prefix)) paste(prefix, name, sep="/") else name
        hidden <- ancestorHidden || identical(item$visible, FALSE)
        if (identical(item$type, "Image")) {
            description <- if (index > 1L)
                items[[index - 1L]] else NULL
            nextItem <- if (index + 1L <= length(items))
                items[[index + 1L]] else NULL
            tablePurpose <- if (!is.null(nextItem) &&
                    identical(nextItem$type, "Html")) nextItem else NULL
            tableIndex <- if (is.null(tablePurpose)) index + 1L else index + 2L
            table <- if (tableIndex <= length(items))
                items[[tableIndex]] else NULL
            found[[path]] <- list(
                image=item,
                description=description,
                tablePurpose=tablePurpose,
                table=table,
                hidden=hidden)
        }
        if (identical(item$type, "Array") && !is.null(item$template$items))
            found <- c(found, collect_plot_contracts(
                item$template$items, path, hidden))
        if (identical(item$type, "Group") && !is.null(item$items))
            found <- c(found, collect_plot_contracts(item$items, path, hidden))
    }
    found
}

collect_result_sequences <- function(items, prefix="") {
    sequences <- list()
    if (is.null(items))
        return(sequences)
    names <- vapply(items, function(item) item$name, character(1))
    sequences[[if (nzchar(prefix)) prefix else "root"]] <- list(
        items=items, names=names)
    for (item in items) {
        path <- if (nzchar(prefix)) paste(prefix, item$name, sep="/") else
            item$name
        if (identical(item$type, "Group") && !is.null(item$items))
            sequences <- c(sequences,
                collect_result_sequences(item$items, path))
        if (identical(item$type, "Array") && !is.null(item$template$items))
            sequences <- c(sequences,
                collect_result_sequences(item$template$items, path))
    }
    sequences
}

test_that("result-native outputs do not depend on purpose-only headings", {
    for (analysis in c("permanova", "anosim", "permdisp", "nmds",
                       "pcoa", "cluster", "simper")) {
        items <- yaml::read_yaml(plot_contract_fixture(analysis, "r"))$items
        names <- vapply(items, `[[`, character(1), "name")
        expect_false(any(grepl("Purpose$|^(summary|settings|note|interpretation)$", names)))
        tables <- items[vapply(items, function(x) identical(x$type, "Table"), logical(1))]
        expect_true(all(nzchar(vapply(tables, `[[`, character(1), "title"))),
            info=paste(analysis, "native table titles"))
        expect_false(any(grepl("^(Data Summary|Analysis Settings|How to read)",
            vapply(items, function(x) if (is.null(x$title)) "" else x$title,
                character(1)))))
    }
})

test_that("Task 6 population attaches settings to result notes", {
    root <- testthat::test_path("..", "..")
    for (analysis in c("permanova", "anosim", "permdisp", "nmds",
                       "pcoa", "cluster", "simper")) {
        source <- paste(readLines(file.path(root, "R", paste0(analysis, ".b.R")),
            warn=FALSE), collapse="\\n")
        expect_false(grepl("miso_populate_summary|populateSettings|Pairwise output",
            source), info=analysis)
        expect_match(source, "\\$setNote", info=paste(analysis, "table note"))
    }
})

test_that("every plot has an adjacent semantic description and titled table", {
    expected <- list(
        permanova=c("companionPcoa"="companionPcoaDescription|companionPcoaSites"),
        anosim=c("rankPlot"="rankPlotDescription|rankSummary"),
        permdisp=c("plot"="plotDescription|distances", "ordinationPlot"="ordinationDescription|ordinationScores"),
        simper=c("contributionPlots/plot"="description|values", "heatmap"="heatmapDescription|heatmapValues"),
        nmds=c("ordination"="ordinationDescription|sites", "shepard"="shepardDescription|shepardPairs"),
        cluster=c("dendrogram"="dendrogramDescription|dendrogramStructure"),
        pcoa=c("ordination"="ordinationDescription|sites"))
    for (analysis in names(expected)) {
        schema <- yaml::read_yaml(plot_contract_fixture(analysis, "r"))
        contracts <- collect_plot_contracts(schema$items)
        expect_setequal(names(contracts), names(expected[[analysis]]))
        for (path in names(expected[[analysis]])) {
            contract <- contracts[[path]]
            expectedNames <- strsplit(expected[[analysis]][[path]], "|", fixed=TRUE)[[1L]]
            expect_identical(contract$description$type, "Html", info=paste(analysis, path))
            expect_identical(contract$table$type, "Table", info=paste(analysis, path))
            expect_identical(contract$table$name, expectedNames[[2L]], info=paste(analysis, path))
            expect_true(nzchar(contract$table$title), info=paste(analysis, path, "table title"))
            expect_true(contract$hidden, info=paste(analysis, path, "hidden until valid"))
        }
    }
})

find_contract_node <- function(node, name) {
    if (is.list(node) && identical(node$name, name))
        return(node)
    if (is.list(node))
        for (child in node) {
            found <- find_contract_node(child, name)
            if (!is.null(found))
                return(found)
        }
    NULL
}

test_that("required and optional assignment wording is consistent and compact", {
    expected <- list(
        permanova=c(vars="Feature Variables",
            factor="Grouping Variable",
            permFactors="Additional Factors",
            strata="Blocking Variable",
            covariates="Continuous Covariates"),
        anosim=c(vars="Feature Variables",
            factor="Grouping Variable",
            strata="Blocking Variable"),
        permdisp=c(vars="Feature Variables",
            factor="Grouping Variable"),
        simper=c(vars="Feature Variables",
            factor="Grouping Variable"),
        nmds=c(vars="Feature Variables",
            factor="Grouping Variable",
            nmdsEnv="Environmental Variables"),
        cluster=c(vars="Feature Variables",
            labels="Sample Labels"),
        pcoa=c(vars="Feature Variables",
            factor="Grouping Variable"))

    for (analysis in names(expected)) {
        a <- yaml::read_yaml(plot_contract_fixture(analysis, "a"))
        options <- setNames(a$options,
            vapply(a$options, `[[`, character(1), "name"))
        u <- yaml::read_yaml(plot_contract_fixture(analysis, "u"))
        source <- paste(readLines(plot_contract_fixture(analysis, "u"),
            warn=FALSE), collapse="\n")
        expect_false(grepl("(?:Required|Optional):", source), info=analysis)
        for (name in names(expected[[analysis]])) {
            expect_identical(options[[name]]$title,
                unname(expected[[analysis]][[name]]),
                info=paste(analysis, name, "analysis title"))
            node <- find_contract_node(u, name)
            if (!is.null(node)) {
                parentLabel <- NULL
                find_parent_label <- function(parent) {
                    if (!is.list(parent)) return(NULL)
                    children <- parent$children
                    if (!is.null(children) && any(vapply(children,
                            function(x) is.list(x) && identical(x$name, name),
                            logical(1))))
                        return(parent$label)
                    for (child in parent) {
                        value <- find_parent_label(child)
                        if (!is.null(value)) return(value)
                    }
                    NULL
                }
                parentLabel <- find_parent_label(u)
                expect_identical(parentLabel,
                    unname(expected[[analysis]][[name]]),
                    info=paste(analysis, name, "UI label"))
            }
        }
    }

    permanovaUi <- yaml::read_yaml(plot_contract_fixture("permanova", "u"))
    expect_identical(find_contract_node(
        permanovaUi, "pcoaDisplayVariables")$label, "Display Groups By")
    expect_identical(find_contract_node(
        permanovaUi, "pcoaDisplayFactor")$name, "pcoaDisplayFactor")
    expect_lte(nchar("Display Groups By"), 24L)
    expect_lte(nchar("Model Factor"), 24L)

    nmdsUi <- yaml::read_yaml(plot_contract_fixture("nmds", "u"))
    expect_identical(find_contract_node(nmdsUi, "plots")$label, "Plots")
    expect_true(find_contract_node(nmdsUi, "groupOptions")$collapsed)
    expect_true(find_contract_node(nmdsUi, "environmentAssessment")$collapsed)
})

relative_luminance <- function(hex) {
    rgb <- grDevices::col2rgb(hex) / 255
    rgb <- ifelse(rgb <= .04045, rgb / 12.92,
        ((rgb + .055) / 1.055) ^ 2.4)
    as.numeric(.2126 * rgb[1L, ] + .7152 * rgb[2L, ] +
        .0722 * rgb[3L, ])
}

contrast_against_white <- function(hex, alpha=1) {
    rgb <- grDevices::col2rgb(hex) / 255
    composite <- alpha * rgb + (1 - alpha)
    compositeHex <- grDevices::rgb(
        composite[1L, ], composite[2L, ], composite[3L, ])
    (1.05) / (relative_luminance(compositeHex) + .05)
}

test_that("shared palette and essential site marks meet contrast contracts", {
    aesthetics <- .misoGroupAesthetics(as.character(seq_len(64L)))
    palette <- unique(unname(aesthetics$colour))
    expect_true(all(contrast_against_white(palette) >= 4.5))
    expect_true(all(contrast_against_white(palette, alpha=.85) >= 4.5))
    expect_error(.misoGroupAesthetics(as.character(seq_len(65L))),
        "at most 64")

    pcoaSource <- paste(deparse(getFromNamespace(
        ".misoBuildPcoaPlot", "miso")), collapse="\n")
    permdispSource <- paste(deparse(getFromNamespace(
        ".buildPermdispOrdinationPlot", "miso")), collapse="\n")
    expect_match(pcoaSource,
        "size[[:space:]]*=[[:space:]]*2\\.2,[[:space:]]+alpha[[:space:]]*=[[:space:]]*0\\.85")
    expect_match(permdispSource,
        "size[[:space:]]*=[[:space:]]*2\\.2,[[:space:]]+alpha[[:space:]]*=[[:space:]]*0\\.9")

    # Lower-alpha jittered marks are non-essential because their exact
    # distributions are provided by the adjacent complete summary tables.
    expect_true(all(c("rankSummary", "distances") %in% c(
        yaml::read_yaml(plot_contract_fixture("anosim", "r"))$items |>
            vapply(`[[`, character(1), "name"),
        yaml::read_yaml(plot_contract_fixture("permdisp", "r"))$items |>
            vapply(`[[`, character(1), "name"))))
})

test_that("PCoA loading never steals keyboard focus", {
    js <- paste(readLines(miso_fixture_path("jamovi", "js", "pcoa.js"),
        warn=FALSE), collapse="\n")
    expect_match(js, "view_loaded")
    expect_false(grepl("\\.focus\\(", js))
})

test_that("every plot renderer consumes only its serialized image state", {
    renderers <- list(
        permanova=list(companionPcoa=".plotCompanionPcoa"),
        anosim=list(rankPlot=".plotRank"),
        permdisp=list(plot=".plotDistances", ordinationPlot=".plotOrdination"),
        nmds=list(ordination=".plotNmds", shepard=".plotShepard"),
        simper=list(`contributionPlots/plot`=".plotContribution",
            heatmap=".plotHeatmap"),
        cluster=list(dendrogram=".plotDendrogram"),
        pcoa=list(ordination=".plotPcoa"))

    for (analysis in names(renderers)) {
        class <- getFromNamespace(paste0(analysis, "Class"), "miso")
        for (path in names(renderers[[analysis]])) {
            name <- renderers[[analysis]][[path]]
            source <- paste(
                deparse(class$private_methods[[name]]),
                collapse="\n")
            expect_match(source, "image\\$state", info=name)
            expect_false(grepl("private\\$\\.state", source), info=name)
        }
    }
})
