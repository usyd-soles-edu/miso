# This file is a generated template, your changes will not be overwritten

clusterClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "clusterClass",
    inherit = clusterBase,
    private = list(
        .state = list(),
        .autoLabelLimit = 40L,

        .run = function() {
            private$.state <- list(
                prep=NULL,
                fit=NULL,
                labels=character(),
                labelSource="",
                labelMode="auto",
                labelModeSource="current",
                showLabels=FALSE,
                labelsShortened=FALSE,
                membership=NULL,
                cutLine=NULL,
                cutDescription="No clusters were defined.",
                clusterStyleAvailable=TRUE,
                warnings=character())
            private$.resetResults()

            if (length(self$options$vars) == 0L) {
                private$.showGuidance(
                    paste(
                        "Cluster analysis groups samples with similar multivariate composition.",
                        "1. Add one or more numeric Feature variables.",
                        "2. Optionally add a Sample labels variable.",
                        "Results update automatically.",
                        sep="\n"),
                    title="Getting started")
                return()
            }

            prep <- miso_prepare_resemblance(
                data=self$data,
                vars=self$options$vars,
                factor=NULL,
                transform=self$options$transform,
                distance=self$options$distance,
                seed=0,
                requireFactor=FALSE)
            if (prep$error) {
                private$.showGuidance(prep$message)
                return()
            }

            fit <- tryCatch(
                stats::hclust(prep$dist, method="average"),
                error=function(e) e)
            if (inherits(fit, "error")) {
                private$.showGuidance(paste(
                    "Cluster analysis could not construct the dendrogram.",
                    fit$message))
                return()
            }

            labelState <- private$.sampleLabels(prep)
            fit$labels <- labelState$labels
            labelChoice <- private$.effectiveSampleLabelMode()
            showLabels <- private$.labelsAreShown(
                prep$rowsUsed,
                labelChoice$mode)
            labelsShortened <- showLabels && any(nchar(labelState$labels) > 24L)
            clusterState <- private$.clusterDefinition(fit)
            membership <- if (isTRUE(clusterState$valid))
                clusterState$membership
            else
                NULL
            clusterCount <- if (is.null(membership))
                0L
            else
                length(unique(membership))

            private$.state <- list(
                prep=prep,
                fit=fit,
                labels=labelState$labels,
                labelSource=labelState$source,
                labelMode=labelChoice$mode,
                labelModeSource=labelChoice$source,
                showLabels=showLabels,
                labelsShortened=labelsShortened,
                membership=membership,
                cutLine=clusterState$cutLine,
                cutDescription=clusterState$description,
                clusterStyleAvailable=clusterCount <= 64L,
                warnings=unique(c(
                    prep$warnings,
                    labelState$warnings,
                    if (labelsShortened)
                        paste(
                            "Long sample labels are shortened only in the dendrogram;",
                            "full labels are retained in the membership table.")
                    else
                        character(),
                    clusterState$warning)))

            self$results$dendrogram$setState(list(
                fit=private$.state$fit,
                labels=private$.state$labels,
                showLabels=private$.state$showLabels,
                membership=private$.state$membership,
                cutLine=private$.state$cutLine,
                distanceLabel=private$.distanceLabel(self$options$distance)))

            private$.populateSummary(prep)
            private$.populatePurposes()
            private$.populateSettings(prep, labelState$source)
            private$.populateDendrogramStructure()
            private$.populateMembership()
            private$.setWarnings(private$.state$warnings)
            private$.populateDescription()
            self$results$interpretation$setContent(miso_html_block(paste(
                "Merge height shows dissimilarity.",
                "Branches can rotate around a merge without changing the clustering."),
                title="How to read this dendrogram"))
            private$.showSuccessfulResults()
        },

        .resetResults = function() {
            self$results$guidance$setContent("")
            miso_clear_table(self$results$summary)
            self$results$warnings$setContent("")
            self$results$dendrogramDescription$setContent("")
            miso_clear_table(self$results$dendrogramStructure)
            miso_clear_table(self$results$membership)
            self$results$interpretation$setContent("")
            miso_clear_table(self$results$settings)
            for (name in c(
                    "summaryPurpose", "dendrogramStructurePurpose",
                    "membershipPurpose", "settingsPurpose"))
                self$results[[name]]$setContent("")

            for (name in c(
                    "guidance", "summary", "summaryPurpose", "warnings", "dendrogram",
                    "dendrogramDescription", "dendrogramStructure",
                    "dendrogramStructurePurpose", "membership",
                    "membershipPurpose", "interpretation", "settings",
                    "settingsPurpose"))
                self$results[[name]]$setVisible(FALSE)
        },

        .showGuidance = function(content, title="Action needed") {
            self$results$guidance$setTitle(title)
            self$results$guidance$setContent(miso_html_block(content))
            self$results$guidance$setVisible(TRUE)
        },

        .showSuccessfulResults = function() {
            for (name in c(
                    "summary", "summaryPurpose", "dendrogram",
                    "dendrogramDescription", "dendrogramStructure",
                    "dendrogramStructurePurpose", "interpretation", "settings",
                    "settingsPurpose"))
                self$results[[name]]$setVisible(TRUE)
            if (!is.null(private$.state$membership)) {
                self$results$membership$setVisible(TRUE)
                self$results$membershipPurpose$setVisible(TRUE)
            }
        },

        .populatePurposes = function() {
            miso_populate_purposes(self$results, list(
                summaryPurpose=c(
                    "Data summary",
                    "Summarises included samples and features, including any exclusions."),
                dendrogramStructurePurpose=c(
                    "Dendrogram structure",
                    "Lists dendrogram merges and their heights."),
                membershipPurpose=c(
                    "Cluster membership",
                    "Lists each sample's cluster at the selected cut."),
                settingsPurpose=c(
                    "Analysis settings",
                    "Lists the options used for this analysis.")))
        },

        .setWarnings = function(warnings) {
            warnings <- unique(warnings[!is.na(warnings) & nzchar(warnings)])
            if (length(warnings) == 0L)
                return()
            self$results$warnings$setContent(miso_warning_block(warnings))
            self$results$warnings$setVisible(TRUE)
        },

        .sampleLabels = function(prep) {
            rowLabels <- as.character(prep$rowIndex)
            selected <- self$options$labels
            if (miso_is_missing_var(selected))
                return(list(
                    labels=rowLabels,
                    source="Data row numbers",
                    warnings=character()))

            selected <- as.character(selected[[1L]])
            if (! selected %in% names(self$data))
                return(list(
                    labels=rowLabels,
                    source="Data row numbers",
                    warnings=sprintf(
                        "Sample labels variable '%s' was not found; data row numbers are shown.",
                        selected)))

            values <- as.character(self$data[[selected]][prep$rowIndex])
            missing <- is.na(values) | ! nzchar(trimws(values))
            warnings <- character()
            if (any(missing)) {
                values[missing] <- paste0("Row ", prep$rowIndex[missing])
                warnings <- c(
                    warnings,
                    sprintf(
                        "%d missing sample label%s replaced with data row numbers.",
                        sum(missing),
                        if (sum(missing) == 1L) " was" else "s were"))
            }

            duplicatedValues <- duplicated(values) | duplicated(values, fromLast=TRUE)
            if (any(duplicatedValues)) {
                values[duplicatedValues] <- paste0(
                    values[duplicatedValues],
                    " [row ", prep$rowIndex[duplicatedValues], "]")
                warnings <- c(
                    warnings,
                    "Duplicate sample labels were distinguished using data row numbers.")
            }

            list(labels=values, source=selected, warnings=warnings)
        },

        .effectiveSampleLabelMode = function() {
            current <- self$options$sampleLabels
            if (length(current) == 1L && current %in% c("show", "hide"))
                return(list(mode=current, source="current"))
            legacy <- self$options$showLabels
            if (length(legacy) == 1L && is.logical(legacy) && !is.na(legacy))
                return(list(
                    mode=if (legacy) "show" else "hide",
                    source="legacy"))
            list(mode=current, source="current")
        },

        .labelsAreShown = function(sampleCount, mode) {
            identical(mode, "show") ||
                (identical(mode, "auto") && sampleCount <= private$.autoLabelLimit)
        },

        .clusterDefinition = function(fit) {
            if (!isTRUE(self$options$defineClusters))
                return(list(
                    valid=FALSE,
                    membership=NULL,
                    cutLine=NULL,
                    description="No clusters were defined.",
                    warning=character()))

            n <- length(fit$order)
            mode <- self$options$cutMode
            if (identical(mode, "number")) {
                k <- self$options$numberClusters
                valid <- length(k) == 1L && is.numeric(k) && is.finite(k) &&
                    k == floor(k) && k >= 2L && k <= n
                if (!valid)
                    return(list(
                        valid=FALSE,
                        membership=NULL,
                        cutLine=NULL,
                        description="Clusters were not defined because the requested number is invalid.",
                        warning=sprintf(
                            "Number of clusters must be a whole number from 2 to %d.",
                            n)))
                k <- as.integer(k)
                membership <- stats::cutree(fit, k=k)
                lowerIndex <- n - k
                lower <- if (lowerIndex > 0L) fit$height[[lowerIndex]] else 0
                upper <- fit$height[[lowerIndex + 1L]]
                cutLine <- NULL
                if (is.finite(lower) && is.finite(upper) && upper > lower) {
                    candidate <- mean(c(lower, upper))
                    heightMembership <- tryCatch(
                        stats::cutree(fit, h=candidate),
                        error=function(e) NULL)
                    samePartition <- !is.null(heightMembership) && identical(
                        match(membership, unique(membership)),
                        match(heightMembership, unique(heightMembership)))
                    if (samePartition)
                        cutLine <- candidate
                }
                description <- if (is.null(cutLine))
                    sprintf(paste(
                        "%d clusters were defined by number. The requested solution",
                        "is shown by membership and cluster styling, but no single",
                        "horizontal cut height represents it because merges are tied."),
                        k)
                else
                    sprintf(
                        "%d clusters were defined by number; the horizontal line marks height %s.",
                        k,
                        private$.formatHeight(cutLine))
                return(list(
                    valid=TRUE,
                    membership=membership,
                    cutLine=cutLine,
                    description=description,
                    warning=character()))
            }

            height <- self$options$cutHeight
            maxHeight <- max(fit$height)
            valid <- length(height) == 1L && is.numeric(height) &&
                is.finite(height) && height >= 0 && height < maxHeight
            if (!valid)
                return(list(
                    valid=FALSE,
                    membership=NULL,
                    cutLine=NULL,
                    description="Clusters were not defined because the requested height is invalid.",
                    warning=sprintf(
                        "Dissimilarity height must be at least 0 and below %s.",
                        private$.formatHeight(maxHeight))))
            membership <- stats::cutree(fit, h=height)
            list(
                valid=TRUE,
                membership=membership,
                cutLine=height,
                description=sprintf(
                    paste(
                        "%d clusters were defined at dissimilarity height %s;",
                        "the horizontal line marks that cut."),
                    length(unique(membership)),
                    private$.formatHeight(height)),
                warning=character())
        },

        .formatHeight = function(value) {
            formatC(value, digits=4L, format="fg", flag="#")
        },

        .populateDescription = function() {
            self$results$dendrogramDescription$setContent(miso_html_block(
                "Shows how samples merge into clusters as dissimilarity increases.",
                ariaLabel="About the cluster dendrogram",
                title="Group-average cluster dendrogram"))
        },

        .addSummary = function(item, value) {
            key <- as.character(length(self$results$summary$rowKeys) + 1L)
            self$results$summary$addRow(
                rowKey=key,
                values=list(item=item, value=as.character(value)))
        },

        .populateSummary = function(prep) {
            private$.addSummary("Samples used", prep$rowsUsed)
            private$.addSummary("Feature variables used", prep$varsUsed)
            private$.addSummary(
                "Rows excluded: missing feature values",
                prep$rowsMissingExcluded)
            private$.addSummary(
                "Rows excluded: all-zero feature values",
                prep$rowsZeroExcluded)
            private$.addSummary(
                "All-zero feature variables excluded",
                prep$featuresZeroExcluded)
        },

        .populateMembership = function() {
            membership <- private$.state$membership
            if (is.null(membership))
                return()
            for (i in seq_along(membership))
                self$results$membership$addRow(
                    rowKey=as.character(i),
                    values=list(
                        sample=private$.state$labels[[i]],
                        cluster=as.integer(membership[[i]])))
        },

        .populateDendrogramStructure = function() {
            fit <- private$.state$fit
            labels <- private$.state$labels
            if (is.null(fit) || length(labels) == 0L)
                return()

            rowKey <- 1L
            for (displayOrder in seq_along(fit$order)) {
                observation <- fit$order[[displayOrder]]
                self$results$dendrogramStructure$addRow(
                    rowKey=as.character(rowKey),
                    values=list(
                        recordType="Leaf",
                        displayOrder=as.integer(displayOrder),
                        sample=as.character(labels[[observation]]),
                        mergeStep="",
                        leftChild="",
                        rightChild="",
                        height=""))
                rowKey <- rowKey + 1L
            }

            childLabel <- function(child) {
                if (child < 0L)
                    return(as.character(labels[[-child]]))
                paste0("Merge ", child)
            }
            for (mergeStep in seq_len(nrow(fit$merge))) {
                children <- fit$merge[mergeStep, ]
                self$results$dendrogramStructure$addRow(
                    rowKey=as.character(rowKey),
                    values=list(
                        recordType="Merge",
                        displayOrder="",
                        sample="",
                        mergeStep=as.integer(mergeStep),
                        leftChild=childLabel(children[[1L]]),
                        rightChild=childLabel(children[[2L]]),
                        height=miso_num_or_na(fit$height[[mergeStep]])))
                rowKey <- rowKey + 1L
            }
        },

        .addSetting = function(setting, value) {
            key <- as.character(length(self$results$settings$rowKeys) + 1L)
            self$results$settings$addRow(
                rowKey=key,
                values=list(setting=setting, value=as.character(value)))
        },

        .populateSettings = function(prep, labelSource) {
            private$.addSetting(
                "Transformation",
                private$.transformLabel(self$options$transform))
            private$.addSetting(
                "Dissimilarity",
                private$.distanceLabel(self$options$distance))
            private$.addSetting("Linkage", "Group average (UPGMA)")
            private$.addSetting("Sample label source", labelSource)
            private$.addSetting(
                "Sample labels",
                sprintf(
                    "%s%s (%s)",
                    switch(private$.state$labelMode,
                        auto="Automatic",
                        show="Show",
                        hide="Hide"),
                    if (identical(
                            private$.state$labelModeSource,
                            "legacy"))
                        " \u2014 inherited from an earlier version"
                    else
                        "",
                    if (private$.state$showLabels) "shown" else "hidden"))
            private$.addSetting(
                "Clusters defined",
                if (is.null(private$.state$membership))
                    "No"
                else
                    private$.state$cutDescription)
        },

        .transformLabel = function(value) {
            switch(value,
                none="None",
                sqrt="Square root",
                fourthroot="Fourth root",
                log="Log(x + 1)",
                pa="Presence/absence",
                wisconsin="Wisconsin",
                hellinger="Hellinger",
                total="Total",
                max="Max",
                frequency="Frequency",
                normalize="Normalize",
                range="Range",
                standardize="Standardize",
                chi.square="Chi-square",
                rclr="Reverse CLR",
                value)
        },

        .distanceLabel = function(value) {
            switch(value,
                bray="Bray-Curtis",
                jaccard="Jaccard",
                euclidean="Euclidean",
                manhattan="Manhattan",
                canberra="Canberra",
                kulczynski="Kulczynski",
                gower="Gower",
                morisita="Morisita",
                horn="Horn-Morisita",
                mountford="Mountford",
                raup="Raup-Crick",
                binomial="Binomial",
                chao="Chao",
                cao="Cao",
                clark="Clark",
                altGower="Alternative Gower",
                mahalanobis="Mahalanobis",
                value)
        },

        .dendrogramData = function(fit, labels, membership=NULL) {
            n <- length(fit$order)
            nodeX <- numeric(2L * n - 1L)
            nodeHeight <- numeric(2L * n - 1L)
            leafPosition <- match(seq_len(n), fit$order)
            nodeX[seq_len(n)] <- leafPosition
            rows <- vector("list", 3L * (n - 1L))
            rowIndex <- 0L
            for (i in seq_len(n - 1L)) {
                children <- fit$merge[i, ]
                childNodes <- ifelse(children < 0L, -children, n + children)
                childX <- nodeX[childNodes]
                childHeight <- nodeHeight[childNodes]
                parentNode <- n + i
                parentHeight <- fit$height[[i]]
                nodeX[[parentNode]] <- mean(childX)
                nodeHeight[[parentNode]] <- parentHeight
                for (j in seq_len(2L)) {
                    rowIndex <- rowIndex + 1L
                    rows[[rowIndex]] <- data.frame(
                        merge=i,
                        segment="vertical",
                        child=j,
                        x=childX[[j]], xend=childX[[j]],
                        y=childHeight[[j]], yend=parentHeight)
                }
                rowIndex <- rowIndex + 1L
                rows[[rowIndex]] <- data.frame(
                    merge=i,
                    segment="horizontal",
                    child=NA_integer_,
                    x=min(childX), xend=max(childX),
                    y=parentHeight, yend=parentHeight)
            }
            segments <- do.call(rbind, rows[seq_len(rowIndex)])
            nodes <- data.frame(
                node=seq_len(2L * n - 1L),
                kind=c(rep("leaf", n), rep("merge", n - 1L)),
                merge=c(rep(NA_integer_, n), seq_len(n - 1L)),
                x=nodeX,
                y=nodeHeight,
                stringsAsFactors=FALSE)
            labelMap <- .misoUniqueShortLabels(labels, width=24L)
            leaf <- data.frame(
                sampleIndex=fit$order,
                x=seq_len(n),
                y=0,
                fullLabel=labels[fit$order],
                plotLabel=unname(labelMap[labels[fit$order]]),
                stringsAsFactors=FALSE)
            if (!is.null(membership)) {
                leaf$cluster <- as.integer(membership[fit$order])
                leaf$clusterKey <- as.character(leaf$cluster)
            }
            list(nodes=nodes, segments=segments, leaf=leaf)
        },

        .buildDendrogram = function (fit = private$.state$fit, labels = private$.state$labels, showLabels = private$.state$showLabels,
            membership = private$.state$membership, cutLine = private$.state$cutLine,
            distanceLabel = private$.distanceLabel(self$options$distance))
        {
            if (is.null(fit))
                return(NULL)
            plotData <- private$.dendrogramData(fit, labels, membership)
            leaf <- plotData$leaf
            plot <- ggplot2::ggplot() + ggplot2::geom_segment(data = plotData$segments, ggplot2::aes(x = x, xend = xend,
                y = y, yend = yend), colour = "#333333", linewidth = 0.55, lineend = "square")
            if (!is.null(cutLine))
                plot <- plot + ggplot2::geom_hline(yintercept = cutLine, colour = "#555555", linetype = "dashed",
                    linewidth = 0.65)
            clusterCount <- if (is.null(membership))
                0L
            else length(unique(membership))
            if (clusterCount > 0L && clusterCount <= 64L) {
                keys <- as.character(sort(unique(leaf$cluster)))
                aesthetics <- .misoGroupAesthetics(keys)
                plot <- plot + ggplot2::geom_point(data = leaf, ggplot2::aes(x = x, y = y, colour = clusterKey,
                    shape = clusterKey), size = 2.2, stroke = 0.55) + ggplot2::scale_colour_manual(name = "Cluster",
                    values = aesthetics$colour[keys]) + ggplot2::scale_shape_manual(name = "Cluster", values = aesthetics$shape[keys])
                if (clusterCount > 8L)
                    plot <- plot + ggplot2::geom_text(data = leaf, ggplot2::aes(x = x, y = y, label = cluster),
                        nudge_y = max(fit$height) * 0.025, size = 3.5, colour = "#222222") + ggplot2::guides(colour = "none",
                        shape = "none")
            }
            else if (clusterCount > 64L) {
                plot <- plot + ggplot2::geom_point(data = leaf, ggplot2::aes(x = x, y = y), shape = 1L, colour = "#444444",
                    size = 2.2)
            }
            axisLabels <- if (isTRUE(showLabels))
                leaf$plotLabel
            else NULL
            plot + ggplot2::scale_x_continuous(breaks = if (isTRUE(showLabels))
                leaf$x
            else NULL, labels = axisLabels, expand = ggplot2::expansion(mult = c(0.015, 0.015)), guide = ggplot2::guide_axis(check.overlap = FALSE)) +
                ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.02, 0.06))) + ggplot2::labs(x = "Samples",
                y = paste(distanceLabel, "dissimilarity")) + .misoPlotTheme() +
                ggplot2::theme(axis.text.x = if (isTRUE(showLabels))
                    ggplot2::element_text(angle = 55, hjust = 1, vjust = 1, size = 10)
                else ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank(), panel.grid.major.x = ggplot2::element_blank(),
                    plot.margin = ggplot2::margin(7, 8, 8, 8))
        }
,

        .plotDendrogram = function(image, ...) {
            plotData <- image$state
            if (is.null(plotData))
                return()
            plot <- private$.buildDendrogram(
                fit=plotData$fit,
                labels=plotData$labels,
                showLabels=plotData$showLabels,
                membership=plotData$membership,
                cutLine=plotData$cutLine,
                distanceLabel=plotData$distanceLabel)
            if (is.null(plot))
                return()
            suppressWarnings(print(plot))
            invisible(TRUE)
        }

    )
)
