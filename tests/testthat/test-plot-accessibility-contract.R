plot_contract_fixture <- function(analysis, suffix) {
    miso_fixture_path("jamovi", paste0(analysis, ".", suffix, ".yaml"))
}

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
            tablePurpose <- if (index + 1L <= length(items))
                items[[index + 1L]] else NULL
            table <- if (index + 2L <= length(items))
                items[[index + 2L]] else NULL
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

test_that("every substantive output follows title purpose output order", {
    expected <- list(
        permanova=c(
            summary="summaryPurpose", table="tablePurpose",
            companionPcoa="companionPcoaDescription",
            companionPcoaSites="companionPcoaSitesPurpose",
            companionPcoaCentroids="companionPcoaCentroidsPurpose",
            pairwise="pairwisePurpose", settings="settingsPurpose"),
        anosim=c(
            summary="summaryPurpose", global="globalPurpose",
            pairwise="pairwisePurpose", rankPlot="rankPlotDescription",
            rankSummary="rankSummaryPurpose", settings="settingsPurpose"),
        permdisp=c(
            summary="summaryPurpose", anova="anovaPurpose",
            pairwise="pairwisePurpose", plot="plotDescription",
            distances="distancesPurpose",
            ordinationPlot="ordinationDescription",
            ordinationScores="ordinationScoresPurpose",
            settings="settingsPurpose"),
        nmds=c(
            summary="summaryPurpose", ordination="ordinationDescription",
            sites="sitesPurpose", stress="stressPurpose",
            shepard="shepardDescription",
            shepardPairs="shepardPairsPurpose", envfit="envfitPurpose",
            features="featuresPurpose", settings="settingsPurpose"),
        simper=c(
            summary="summaryPurpose", contrasts="contrastsPurpose",
            contributions="contributionsPurpose",
            variability="variabilityPurpose", means="meansPurpose",
            `contributionPlots/plot`="description",
            `contributionPlots/values`="valuesPurpose",
            heatmap="heatmapDescription",
            heatmapValues="heatmapValuesPurpose",
            assessment="assessmentPurpose", settings="settingsPurpose"),
        cluster=c(
            summary="summaryPurpose", dendrogram="dendrogramDescription",
            dendrogramStructure="dendrogramStructurePurpose",
            membership="membershipPurpose", settings="settingsPurpose"),
        pcoa=c(
            summary="summaryPurpose", ordination="ordinationDescription",
            sites="sitesPurpose", centroids="centroidsPurpose",
            eigenvalues="eigenvaluesPurpose", settings="settingsPurpose"))

    for (analysis in names(expected)) {
        schema <- yaml::read_yaml(plot_contract_fixture(analysis, "r"))
        sequences <- collect_result_sequences(schema$items)
        for (path in names(expected[[analysis]])) {
            parts <- strsplit(path, "/", fixed=TRUE)[[1L]]
            sequenceName <- if (length(parts) == 1L) "root" else
                paste(parts[-length(parts)], collapse="/")
            output <- parts[[length(parts)]]
            sequence <- sequences[[sequenceName]]
            index <- match(output, sequence$names)
            purposeName <- unname(expected[[analysis]][[path]])
            expect_false(is.na(index), info=paste(analysis, path))
            expect_gt(index, 1L)
            if (is.na(index) || index <= 1L)
                next
            expect_identical(
                sequence$names[[index - 1L]], purposeName,
                info=paste(analysis, path))
            purpose <- sequence$items[[index - 1L]]
            outputItem <- sequence$items[[index]]
            expect_identical(purpose$type, "Html",
                info=paste(analysis, path))
            expect_true(nzchar(purpose$title),
                info=paste(analysis, path))
            expect_identical(outputItem$title, "",
                info=paste(analysis, path, "native output title"))
            expect_true(
                identical(purpose$visible, FALSE) ||
                    (startsWith(path, "contributionPlots/") &&
                        is.null(purpose$visible)),
                info=paste(analysis, path))
        }
    }
})

test_that("every plot has an adjacent semantic description and table", {
    expected <- list(
        permanova=c(
            "companionPcoa"="companionPcoaDescription|companionPcoaSites"),
        anosim=c("rankPlot"="rankPlotDescription|rankSummary"),
        permdisp=c(
            "plot"="plotDescription|distances",
            "ordinationPlot"="ordinationDescription|ordinationScores"),
        simper=c(
            "contributionPlots/plot"="description|values",
            "heatmap"="heatmapDescription|heatmapValues"),
        nmds=c(
            "ordination"="ordinationDescription|sites",
            "shepard"="shepardDescription|shepardPairs"),
        cluster=c(
            "dendrogram"="dendrogramDescription|dendrogramStructure"),
        pcoa=c("ordination"="ordinationDescription|sites"))

    for (analysis in names(expected)) {
        schema <- yaml::read_yaml(plot_contract_fixture(analysis, "r"))
        contracts <- collect_plot_contracts(schema$items)
        expect_setequal(names(contracts), names(expected[[analysis]]))
        for (path in names(expected[[analysis]])) {
            contract <- contracts[[path]]
            expectedNames <- strsplit(expected[[analysis]][[path]], "|",
                fixed=TRUE)[[1L]]
            expect_identical(contract$description$type, "Html",
                info=paste(analysis, path, "description"))
            expect_identical(contract$description$name, expectedNames[[1L]],
                info=paste(analysis, path, "description name"))
            expect_identical(contract$table$type, "Table",
                info=paste(analysis, path, "table"))
            expect_identical(contract$table$name, expectedNames[[2L]],
                info=paste(analysis, path, "table name"))
            expect_identical(contract$tablePurpose$type, "Html",
                info=paste(analysis, path, "table purpose"))
            expect_gt(length(contract$table$columns), 0L)
            expect_true(contract$hidden,
                info=paste(analysis, path, "hidden until valid"))
            expect_gte(contract$image$width, 580L)
            expect_lte(contract$image$width, 600L)
            expect_lte(contract$image$height, 650L)
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
        permanova=c(vars="Feature variables (required)",
            factor="Grouping variable (required)",
            permFactors="Additional factors (optional)",
            strata="Blocking variable (optional)",
            covariates="Continuous covariates (optional)"),
        anosim=c(vars="Feature variables (required)",
            factor="Grouping variable (required)",
            strata="Blocking variable (optional)"),
        permdisp=c(vars="Feature variables (required)",
            factor="Grouping variable (required)"),
        simper=c(vars="Feature variables (required)",
            factor="Grouping variable (required)"),
        nmds=c(vars="Feature variables (required)",
            factor="Grouping variable (optional)",
            nmdsEnv="Environmental variables (optional)"),
        cluster=c(vars="Feature variables (required)",
            labels="Sample labels (optional)"),
        pcoa=c(vars="Feature variables (required)",
            factor="Grouping variable (optional)"))

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
        permanovaUi, "pcoaDisplayVariables")$label, "Display groups by")
    expect_identical(find_contract_node(
        permanovaUi, "pcoaDisplayFactor")$name, "pcoaDisplayFactor")
    expect_lte(nchar("Display groups by"), 24L)
    expect_lte(nchar("Model factor"), 24L)

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
