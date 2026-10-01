# This file is a generated template, your changes will not be overwritten

clusterClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "clusterClass",
    inherit = clusterBase,
    private = list(
        .state = list(),
        .lastStructuralKey = NULL,
        .summaryRowNo = 0L,
        .settingsRowNo = 0L,
        .autoLabelLimit = 40L,

        .init = function() {
            if (is.null(private$.lastStructuralKey))
                private$.showEmptyTables()
        },

        .showEmptyTables = function() {
            selected <- intersect(setdiff(unique(self$options$contextVars),
                self$options$labels), names(self$data))
            identifier <- self$options$labels
            identifierTitle <- if (!miso_is_missing_var(identifier) &&
                    identifier %in% names(self$data)) identifier else "Sample"
            for (name in c("membership", "sampleOrder"))
                private$.contextColumns(self$results[[name]], selected, identifierTitle)
            miso_show_empty_tables(self$results, c(
                    sampleOrder=isTRUE(self$options$showSampleOrder),
                    mergeHistory=isTRUE(self$options$showMergeHistory),
                    membership=isTRUE(self$options$defineClusters)))
        },

        .fitTree = function(prep) {
            stats::hclust(prep$dist, method="average")
        },

        .run = function() {
            on.exit(miso_finish_empty_tables(self$results), add=TRUE)
            vars <- self$options$vars
            featureData <- self$data[, intersect(vars, names(self$data)), drop=FALSE]
            structuralKey <- serialize(list(
                vars=vars, data=miso_data_signature(featureData),
                rowNames=rownames(self$data),
                transform=self$options$transform, distance=self$options$distance), NULL)
            if (!identical(private$.lastStructuralKey, structuralKey) ||
                    (length(vars) > 0L && is.null(private$.state$fit))) {
                private$.lastStructuralKey <- structuralKey
                private$.state <- list(fit=NULL, prep=NULL)
                private$.clearResults()
                if (length(vars) > 0L) {
                    prep <- miso_prepare_resemblance(
                        data=self$data, vars=vars, factor=NULL,
                        transform=self$options$transform, distance=self$options$distance,
                        seed=0, requireFactor=FALSE)
                    if (isTRUE(prep$error)) {
                        private$.showGuidance(prep$message)
                        return()
                    }
                    fit <- tryCatch(private$.fitTree(prep), error=function(e) e)
                    if (inherits(fit, "error")) {
                        private$.showGuidance(paste(
                            "Cluster analysis could not construct the dendrogram.", fit$message))
                        return()
                    }
                    private$.state$prep <- prep
                    private$.state$fit <- fit
                }
            }
            if (is.null(private$.state$fit)) {
                private$.clearResults()
                if (!miso_is_missing_var(self$options$labels)) {
                    private$.showGuidance("Add one or more numeric Feature variables.")
                }
                return()
            }
            private$.refreshDisplayOnly()
        },

        .refreshDisplayOnly = function() {
            prep <- private$.state$prep
            labelState <- private$.sampleLabels(prep)
            labelChoice <- private$.effectiveSampleLabelMode()
            private$.state$labels <- labelState$labels
            private$.state$labelSource <- labelState$source
            private$.state$fit$labels <- labelState$labels
            clusterState <- private$.clusterDefinition(private$.state$fit)
            private$.state$labelMode <- labelChoice$mode
            private$.state$labelModeSource <- labelChoice$source
            private$.state$showLabels <- private$.labelsAreShown(prep$rowsUsed, labelChoice$mode)
            private$.state$membership <- clusterState$membership
            private$.state$cutLine <- clusterState$cutLine
            private$.state$cutDescription <- clusterState$description
            private$.state$clusterStyleAvailable <-
                length(unique(clusterState$membership)) <= 64L
            contextWarnings <- private$.populateTables()
            private$.state$baseWarnings <- unique(c(
                prep$warnings, labelState$warnings, clusterState$warning, contextWarnings))
            private$.refreshLabelWarnings()
            plotState <- list(fit=private$.state$fit, labels=private$.state$labels,
                showLabels=private$.state$showLabels,
                membership=private$.state$membership, cutLine=private$.state$cutLine,
                distanceLabel=private$.distanceLabel(self$options$distance))
            if (!identical(self$results$dendrogram$state, plotState))
                self$results$dendrogram$setState(plotState)
            private$.showSuccessfulResults()
        },

        .refreshLabelWarnings = function() {
            private$.state$labelsShortened <- private$.state$showLabels &&
                any(nchar(private$.state$labels) > 24L)
            private$.state$warnings <- unique(c(
                private$.state$baseWarnings,
                if (identical(private$.state$labelMode, "auto") &&
                        length(private$.state$labels) > private$.autoLabelLimit)
                    paste("Automatic hides sample labels above 40 samples.",
                        "Choose Sample labels > Show or Tables > Sample order."),
                if (private$.state$labelsShortened)
                    paste(
                        "Long sample labels are shortened only in the dendrogram;",
                        "full labels are available in Tables > Sample order.")))
            private$.setWarnings(private$.state$warnings)
        },

        .clearResults = function() {
            private$.summaryRowNo <- 0L
            private$.settingsRowNo <- 0L
            self$results$guidance$setContent("")
            self$results$warnings$setContent("")
            self$results$dendrogramDescription$setContent("")
            self$results$dendrogram$setState(NULL)
            miso_clear_table(self$results$sampleOrder)
            miso_clear_table(self$results$mergeHistory)
            miso_clear_table(self$results$membership)

            for (name in c(
                    "guidance", "warnings", "dendrogram",
                    "dendrogramDescription", "sampleOrder", "mergeHistory",
                    "membership"
                    ))
                self$results[[name]]$setVisible(FALSE)
            private$.showEmptyTables()
        },

        .showGuidance = function(content, title="Action needed") {
            self$results$guidance$setTitle(title)
            self$results$guidance$setContent(miso_html_block(content))
            self$results$guidance$setVisible(TRUE)
        },

        .showSuccessfulResults = function() {
            self$results$guidance$setVisible(FALSE)
            self$results$dendrogram$setVisible(TRUE)
            self$results$sampleOrder$setVisible(isTRUE(self$options$showSampleOrder))
            self$results$mergeHistory$setVisible(isTRUE(self$options$showMergeHistory))
            self$results$dendrogramDescription$setVisible(FALSE)
            self$results$membership$setVisible(!is.null(private$.state$membership))
        },


        .setWarnings = function(warnings) {
            warnings <- unique(warnings[!is.na(warnings) & nzchar(warnings)])
            if (length(warnings) == 0L) {
                self$results$warnings$setContent("")
                self$results$warnings$setVisible(FALSE)
                return()
            }
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
            self$results$dendrogramDescription$setContent("")
        },



        .contextColumns = function(table, selected, identifierTitle) {
            fixedColumns <- setdiff(names(table$columns), c("sample",
                grep("^context[0-9]+$", names(table$columns), value=TRUE)))
            fixedTitles <- vapply(table$columns[fixedColumns],
                function(col) col$title, character(1))
            sampleTitle <- if (identifierTitle %in% fixedTitles)
                paste0(identifierTitle, " (sample)") else identifierTitle
            table$getColumn("sample")$setTitle(sampleTitle)
            usedTitles <- c(sampleTitle, fixedTitles)
            for (i in seq_along(selected)) {
                key <- paste0("context", i)
                if (!key %in% names(table$columns)) {
                    clusterIndex <- match("cluster", names(table$columns))
                    table$addColumn(name=key, title=selected[[i]], type="text",
                        index=clusterIndex)
                }
                title <- selected[[i]]
                if (title %in% usedTitles) title <- paste0(title, " (context)")
                title <- make.unique(c(usedTitles, title))[[length(usedTitles) + 1L]]
                usedTitles <- c(usedTitles, title)
                col <- table$getColumn(key)
                col$setTitle(title)
                col$setVisible(TRUE)
            }
            inactive <- setdiff(grep("^context[0-9]+$", names(table$columns), value=TRUE),
                vapply(seq_along(selected), function(i) paste0("context", i), character(1)))
            for (key in inactive) {
                col <- table$getColumn(key)
                col$setTitle("")
                col$setVisible(FALSE)
                for (rowKey in table$rowKeys)
                    table$setCell(rowKey=rowKey, col=key, value="")
            }
        },

        .populateTables = function() {
            prep <- private$.state$prep
            fit <- private$.state$fit
            labels <- private$.state$labels
            identifier <- self$options$labels
            selected <- setdiff(unique(self$options$contextVars), identifier)
            missing <- setdiff(selected, names(self$data))
            selected <- intersect(selected, names(self$data))
            identifierTitle <- if (!miso_is_missing_var(identifier) &&
                    identifier %in% names(self$data)) identifier else "Sample"
            membership <- private$.state$membership
            methodNote <- paste("Linkage: Group average.", miso_method_note(
                private$.transformLabel(self$options$transform),
                private$.distanceLabel(self$options$distance)))
            contextValues <- function(observation) {
                values <- lapply(selected, function(name) {
                    value <- self$data[[name]][prep$rowIndex[[observation]]]
                    if (is.na(value)) NA else as.character(value)
                })
                setNames(values, vapply(seq_along(selected), function(i) paste0("context", i), character(1)))
            }
            for (name in c("membership", "sampleOrder"))
                private$.contextColumns(self$results[[name]], selected, identifierTitle)
            if (is.null(membership)) {
                miso_clear_table(self$results$membership)
                self$results$membership$setNote("method", note=NULL)
            } else {
                rows <- lapply(seq_along(membership), function(i) list(
                    key=as.character(prep$rowIndex[[i]]), values=c(
                        list(sample=labels[[i]], cluster=as.integer(membership[[i]])),
                        contextValues(i))))
                miso_reconcile_table_rows(self$results$membership, rows)
                cutNote <- if (identical(self$options$cutMode, "number"))
                    paste0(sprintf("Cut rule: %d clusters.", length(unique(membership))),
                        if (is.null(private$.state$cutLine))
                            " Tied merge heights prevent a single equivalent height cut." else "")
                else sprintf("Cut rule: dissimilarity height %s.",
                    private$.formatHeight(self$options$cutHeight))
                self$results$membership$setNote("method",
                    note=paste(methodNote, cutNote), init=FALSE)
            }
            if (isTRUE(self$options$showSampleOrder)) {
                rows <- lapply(seq_along(fit$order), function(position) {
                    i <- fit$order[[position]]
                    list(key=as.character(prep$rowIndex[[i]]), values=c(
                        list(position=as.integer(position), sample=labels[[i]]), contextValues(i)))
                })
                miso_reconcile_table_rows(self$results$sampleOrder, rows)
                self$results$sampleOrder$setNote("method", note=methodNote, init=FALSE)
            } else {
                miso_clear_table(self$results$sampleOrder)
                self$results$sampleOrder$setNote("method", note=NULL)
            }
            if (isTRUE(self$options$showMergeHistory)) {
                child <- function(value) if (value < 0L)
                    paste0("Sample: ", labels[[-value]]) else paste0("Merge ", value)
                rows <- lapply(seq_len(nrow(fit$merge)), function(i) list(
                    key=as.character(i), values=list(step=as.integer(i),
                        first=child(fit$merge[i, 1L]), second=child(fit$merge[i, 2L]),
                        height=miso_num_or_na(fit$height[[i]]))))
                miso_reconcile_table_rows(self$results$mergeHistory, rows)
                self$results$mergeHistory$setNote("method", note=methodNote, init=FALSE)
            } else {
                miso_clear_table(self$results$mergeHistory)
                self$results$mergeHistory$setNote("method", note=NULL)
            }
            if (length(missing) > 0L)
                sprintf("Sample context variables unavailable: %s.", paste(missing, collapse=", "))
            else character()
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
