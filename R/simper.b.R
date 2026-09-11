# This file is a generated template, your changes will not be overwritten

simperClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "simperClass",
    inherit = simperBase,
    private = list(
        .state = list(),
        .lastStructuralKey = NULL,
        .lastDisplayKey = NULL,

        .run = function() {
            structuralKey <- miso_options_signature(
                self$options, excluded=c("simperDetails", "simperPlots", "simperHeatmap"),
                data=self$data)
            if (!is.null(private$.lastStructuralKey) &&
                    identical(private$.lastStructuralKey, structuralKey)) {
                private$.refreshDisplayOnly()
                return()
            }
            private$.lastStructuralKey <- structuralKey
            private$.lastDisplayKey <- list(
                simperDetails=isTRUE(self$options$simperDetails),
                simperPlots=isTRUE(self$options$simperPlots),
                simperHeatmap=isTRUE(self$options$simperHeatmap))
            private$.state <- list(
                warnings=character(),
                plotData=NULL,
                contrastTotals=integer(),
                contrastLabels=character(),
                requestedPermutations=NA_integer_,
                effectivePermutations=NA_integer_,
                descriptive=NULL)
            private$.clearResults()

            hasVars <- length(self$options$vars) > 0L
            hasFactor <- private$.hasValue(self$options$factor)
            if (! hasVars && ! hasFactor) {
                private$.discardKeyedRows()
                private$.showGuidance(
                    paste(
                        "SIMPER (similarity percentages) explores which features contribute most to Bray-Curtis dissimilarity between groups.",
                        "1. Add numeric Feature variables, such as species abundances.",
                        "2. Add one categorical Grouping variable containing at least two observed groups.",
                        "A contrast is one pair of observed groups being compared.",
                        sep="\n"),
                    title="Getting started")
                return()
            }
            if (! hasVars) {
                private$.discardKeyedRows()
                private$.showGuidance(paste(
                    "SIMPER is waiting for Feature variables.",
                    "Add one or more numeric response columns, such as species abundances."))
                return()
            }
            if (! hasFactor) {
                private$.discardKeyedRows()
                private$.showGuidance(paste(
                    "SIMPER is waiting for a Grouping variable.",
                    "Add one categorical column containing at least two observed groups."))
                return()
            }

            if (self$options$transform %in% c("standardize", "rclr")) {
                private$.discardKeyedRows()
                private$.showGuidance(paste(
                    "This transformation cannot produce valid non-negative inputs for Bray-Curtis SIMPER.",
                    "Choose a compatible transformation in Analysis choices."))
                return()
            }

            prep <- tryCatch(
                miso_prepare_resemblance(
                    data=self$data,
                    vars=self$options$vars,
                    factor=self$options$factor,
                    transform=self$options$transform,
                    distance="bray",
                    seed=self$options$seed,
                    distBinary=FALSE),
                error=function(e) e)
            if (inherits(prep, "error")) {
                private$.discardKeyedRows()
                stop(prep)
            }
            if (isTRUE(prep$error)) {
                private$.discardKeyedRows()
                private$.showGuidance(prep$message)
                return()
            }

            transformed <- as.matrix(prep$transformed)
            if (any(! is.finite(transformed)) || any(transformed < 0) ||
                    any(rowSums(transformed) <= 0)) {
                private$.discardKeyedRows()
                private$.showGuidance(paste(
                    "The selected transformation did not produce valid Bray-Curtis SIMPER inputs.",
                    "Choose a transformation that leaves finite, non-negative values and a positive total for every sample."))
                return()
            }

            private$.state$warnings <- prep$warnings
            if (! identical(self$options$distance, "bray"))
                private$.state$warnings <- c(
                    private$.state$warnings,
                    sprintf(
                        "Saved distance '%s' was ignored; SIMPER used Bray-Curtis.",
                        private$.distanceLabel(self$options$distance)))
            if (isTRUE(self$options$distBinary))
                private$.state$warnings <- c(
                    private$.state$warnings,
                    paste(
                        "The saved binary-distance setting was ignored; Bray-Curtis used the selected transformation."))

            descriptive <- private$.runDescriptive(prep)
            if (is.null(descriptive)) {
                private$.discardKeyedRows()
                return()
            }
            private$.state$descriptive <- descriptive
            private$.populateDescriptive(descriptive)

            assessmentShown <- FALSE
            if (isTRUE(self$options$simperAssess))
                assessmentShown <- private$.runAssessment(prep, descriptive$displayRows)
            if (! assessmentShown)
                miso_clear_table(self$results$assessment)

            private$.setTableNotes(prep)
            if (! isTRUE(self$options$simperDetails)) {
                miso_clear_table(self$results$variability)
                miso_clear_table(self$results$means)
            }
            if (! isTRUE(self$options$simperHeatmap))
                miso_clear_table(self$results$heatmapValues)
            private$.setWarnings(private$.state$warnings)
            private$.showSuccessfulResults(assessmentShown)
        },

        .refreshDisplayOnly = function() {
            descriptive <- private$.state$descriptive
            details <- isTRUE(self$options$simperDetails)
            plots <- isTRUE(self$options$simperPlots)
            heatmap <- isTRUE(self$options$simperHeatmap)
            previousDisplay <- private$.lastDisplayKey
            private$.lastDisplayKey <- list(
                simperDetails=details,
                simperPlots=plots,
                simperHeatmap=heatmap)
            detailsChanged <- is.null(previousDisplay) ||
                !identical(previousDisplay$simperDetails, details)
            plotsChanged <- is.null(previousDisplay) ||
                !identical(previousDisplay$simperPlots, plots)
            heatmapChanged <- is.null(previousDisplay) ||
                !identical(previousDisplay$simperHeatmap, heatmap)

            if (detailsChanged) {
                miso_clear_table(self$results$variability)
                miso_clear_table(self$results$means)
                if (details && !is.null(descriptive))
                    private$.populateOptionalDetailRows(descriptive)
            }
            self$results$variability$setVisible(details && !is.null(descriptive))
            self$results$means$setVisible(details && !is.null(descriptive))

            if (plotsChanged) {
                self$results$contributionPlots$clear()
                if (plots && !is.null(descriptive))
                    private$.populateContributionPlots(descriptive)
            }
            self$results$contributionPlots$setVisible(plots && !is.null(descriptive))

            if (heatmapChanged) {
                miso_clear_table(self$results$heatmapValues)
                if (heatmap && !is.null(descriptive))
                    private$.populateOptionalHeatmap()
            }
            self$results$heatmap$setVisible(heatmap && !is.null(descriptive))
            self$results$heatmapDescription$setVisible(FALSE)
            self$results$heatmapValues$setVisible(heatmap && !is.null(descriptive))
        },

        .populateOptionalDetailRows = function(descriptive) {
            variabilityRows <- list()
            meansRows <- list()
            for (index in seq_along(descriptive$fullRows)) {
                values <- descriptive$fullRows[[index]]
                values$contrastIndex <- NULL
                values$firstGroup <- NULL
                values$secondGroup <- NULL
                variabilityRows[[index]] <- list(
                    key=as.character(index),
                    values=values[c("contrast", "feature", "average", "sd", "ratio")])
                meansRows[[index]] <- list(
                    key=as.character(index),
                    values=values[c("contrast", "feature", "meanFirst", "meanSecond")])
            }
            miso_reconcile_table_rows(self$results$variability, variabilityRows)
            miso_reconcile_table_rows(self$results$means, meansRows)
        },

        .populateOptionalHeatmap = function() {
            self$results$heatmapDescription$setContent(
                private$.heatmapDescription())
            heatmapData <- private$.heatmapData()
            self$results$heatmap$setState(heatmapData)
            if (is.null(heatmapData))
                return()
            heatmapRows <- lapply(seq_len(nrow(heatmapData)), function(row) {
                list(
                    key=as.character(row),
                    values=list(
                        contrast=as.character(heatmapData$contrast[[row]]),
                        feature=as.character(heatmapData$feature[[row]]),
                        contribution=if (isTRUE(heatmapData$missing[[row]]))
                            ""
                        else
                            miso_num_or_na(heatmapData$contribution[[row]]),
                        selected=if (isTRUE(heatmapData$missing[[row]]))
                            "No" else "Yes"))
            })
            miso_reconcile_table_rows(self$results$heatmapValues, heatmapRows)
        },

        .clearResults = function() {
            self$results$guidance$setContent("")
            self$results$warnings$setContent("")
            miso_clear_table_values(self$results$contrasts)
            miso_clear_table_values(self$results$contributions)
            miso_clear_table_values(self$results$variability)
            miso_clear_table_values(self$results$means)
            miso_clear_table_values(self$results$table)
            miso_clear_table_values(self$results$assessment)
            miso_clear_table_values(self$results$heatmapValues)
            self$results$contributionPlots$clear()
            self$results$contrasts$setNote(key="meaning", note=NULL, init=FALSE)
            self$results$contributions$setNote(key="meaning", note=NULL, init=FALSE)
            self$results$variability$setNote(key="meaning", note=NULL, init=FALSE)
            self$results$means$setNote(key="meaning", note=NULL, init=FALSE)
            self$results$table$setNote(key="meaning", note=NULL, init=FALSE)
            self$results$assessment$setNote(key="scope", note=NULL, init=FALSE)
            self$results$heatmapDescription$setContent("")
            self$results$heatmap$setSize(600, 500)

            for (name in c(
                    "guidance", "warnings",
                    "contrasts", "contributions",
                    "variability",
                    "means", "table",
                    "contributionPlots", "heatmap", "heatmapDescription",
                    "heatmapValues", "assessment"
                    ))
                self$results[[name]]$setVisible(FALSE)
            for (name in c("contrasts", "contributions"))
                self$results[[name]]$setVisible(TRUE)
        },

        # Destructive row removal for keyed result tables on rerun paths that
        # do not repopulate them: guidance, rejections, and failed descriptive
        # or assessment runs must not leave stale or blank rows behind.
        .discardKeyedRows = function() {
            miso_clear_table(self$results$contrasts)
            miso_clear_table(self$results$contributions)
            miso_clear_table(self$results$variability)
            miso_clear_table(self$results$means)
            miso_clear_table(self$results$table)
            miso_clear_table(self$results$assessment)
            miso_clear_table(self$results$heatmapValues)
        },

        .showGuidance = function(content, title="Action needed") {
            self$results$guidance$setTitle(title)
            self$results$guidance$setContent(miso_html_block(content))
            self$results$guidance$setVisible(TRUE)
        },

        .showSuccessfulResults = function(assessmentShown=FALSE) {
            self$results$guidance$setVisible(FALSE)
            details <- isTRUE(self$options$simperDetails)
            self$results$variability$setVisible(details)
            self$results$means$setVisible(details)
            for (name in c(
                    "contrasts",
                    "contributions"
                    ))
                self$results[[name]]$setVisible(TRUE)
            self$results$contributionPlots$setVisible(
                isTRUE(self$options$simperPlots))
            self$results$heatmap$setVisible(isTRUE(self$options$simperHeatmap))
            self$results$heatmapDescription$setVisible(FALSE)
            self$results$heatmapValues$setVisible(
                isTRUE(self$options$simperHeatmap))
            self$results$assessment$setVisible(isTRUE(assessmentShown))
        },


        .setWarnings = function(warnings) {
            warnings <- unique(warnings[! is.na(warnings) & nzchar(warnings)])
            if (length(warnings) == 0L) {
                self$results$warnings$setContent("")
                self$results$warnings$setVisible(FALSE)
                return()
            }
            self$results$warnings$setContent(miso_warning_block(warnings))
            self$results$warnings$setVisible(TRUE)
        },

        .runDescriptive = function(prep) {
            pairs <- private$.contrastPairs(prep$group)
            fit <- tryCatch(
                vegan::simper(
                    prep$transformed,
                    prep$group,
                    permutations=0L),
                error=function(e) e)
            if (inherits(fit, "error")) {
                private$.showGuidance(paste(
                    "SIMPER could not calculate feature contributions.",
                    fit$message))
                return(NULL)
            }
            if (length(fit) != length(pairs)) {
                private$.showGuidance(paste(
                    "SIMPER returned an unexpected set of group contrasts.",
                    "Check the grouping variable and try the analysis again."))
                return(NULL)
            }

            summaries <- summary(fit)
            topN <- as.integer(self$options$simperTop)
            threshold <- as.numeric(self$options$simperCum) / 100
            contrastRows <- list()
            displayRows <- list()
            fullRows <- list()
            contrastTotals <- integer()
            failed <- character()

            for (index in seq_along(pairs)) {
                pair <- pairs[[index]]
                label <- private$.contrastLabel(pair)
                tab <- as.data.frame(summaries[[index]])
                if (nrow(tab) == 0L || ! "average" %in% names(tab)) {
                    failed <- c(failed, label)
                    next
                }

                tab$feature <- rownames(tab)
                usable <- is.finite(tab$average) & tab$average >= 0
                tab <- tab[usable, , drop=FALSE]
                if (nrow(tab) == 0L) {
                    failed <- c(failed, label)
                    next
                }
                tab <- tab[order(-tab$average, tab$feature), , drop=FALSE]
                total <- sum(tab$average)
                if (! is.finite(total) || total <= 0) {
                    failed <- c(failed, label)
                    next
                }

                tab$contribution <- tab$average / total
                tab$cumulative <- cumsum(tab$contribution)
                unavailable <- ! is.finite(tab$sd) | ! is.finite(tab$ratio)
                if (any(unavailable))
                    private$.state$warnings <- c(
                        private$.state$warnings,
                        sprintf(
                            "%s has unavailable Contribution SD or Average divided by SD for: %s. Replication is insufficient for those values, so blank cells are shown.",
                            label,
                            paste(tab$feature[unavailable], collapse=", ")))
                crossing <- which(tab$cumulative >= threshold)[1L]
                if (is.na(crossing))
                    crossing <- nrow(tab)
                displayN <- min(topN, crossing, nrow(tab))
                contrastTotals[[label]] <- nrow(tab)

                contrastRows[[length(contrastRows) + 1L]] <- list(
                    contrast=label,
                    nFirst=sum(as.character(prep$group) == pair[[1L]]),
                    nSecond=sum(as.character(prep$group) == pair[[2L]]),
                    overall=miso_num_or_na(fit[[index]]$overall))

                contrastFeatureRows <- lapply(seq_len(nrow(tab)), function(row) {
                    list(
                        contrastIndex=index,
                        contrast=label,
                        firstGroup=pair[[1L]],
                        secondGroup=pair[[2L]],
                        feature=tab$feature[[row]],
                        average=miso_num_or_na(tab$average[[row]]),
                        sd=miso_num_or_na(tab$sd[[row]]),
                        ratio=miso_num_or_na(tab$ratio[[row]]),
                        meanFirst=miso_num_or_na(tab$ava[[row]]),
                        meanSecond=miso_num_or_na(tab$avb[[row]]),
                        contribution=100 * tab$contribution[[row]],
                        cumulative=100 * tab$cumulative[[row]])
                })
                fullRows <- c(fullRows, contrastFeatureRows)
                displayRows <- c(
                    displayRows,
                    contrastFeatureRows[seq_len(displayN)])
            }

            if (length(failed) > 0L)
                private$.state$warnings <- c(
                    private$.state$warnings,
                    paste0(
                        "No usable descriptive contributions were returned for: ",
                        paste(failed, collapse=", "), "."))
            if (length(displayRows) == 0L) {
                private$.showGuidance(paste(
                    "SIMPER could not calculate non-missing feature contributions for these data.",
                    "Check that groups contain usable, non-identical samples."))
                return(NULL)
            }

            list(
                fit=fit,
                pairs=pairs,
                contrastRows=contrastRows,
                fullRows=fullRows,
                displayRows=displayRows,
                contrastTotals=contrastTotals)
        },


        .populateDescriptive = function(descriptive) {
            contrastRows <- lapply(seq_along(descriptive$contrastRows), function(index)
                list(
                    key=as.character(index),
                    values=descriptive$contrastRows[[index]]))
            miso_reconcile_table_rows(self$results$contrasts, contrastRows)

            tableRows <- list()
            variabilityRows <- list()
            meansRows <- list()
            for (index in seq_along(descriptive$fullRows)) {
                values <- descriptive$fullRows[[index]]
                values$contrastIndex <- NULL
                values$firstGroup <- NULL
                values$secondGroup <- NULL
                tableRows[[index]] <- list(key=as.character(index), values=values)
                if (isTRUE(self$options$simperDetails)) {
                    variabilityRows[[index]] <- list(
                        key=as.character(index),
                        values=values[c(
                            "contrast", "feature", "average", "sd", "ratio")])
                    meansRows[[index]] <- list(
                        key=as.character(index),
                        values=values[c(
                            "contrast", "feature", "meanFirst", "meanSecond")])
                }
            }
            miso_reconcile_table_rows(self$results$table, tableRows)
            if (length(variabilityRows) > 0L) {
                miso_reconcile_table_rows(self$results$variability, variabilityRows)
                miso_reconcile_table_rows(self$results$means, meansRows)
            }

            contributionRows <- lapply(
                seq_along(descriptive$displayRows), function(index) {
                    values <- descriptive$displayRows[[index]]
                    list(
                        key=as.character(index),
                        values=values[c(
                            "contrast", "feature", "contribution", "cumulative")])
                })
            miso_reconcile_table_rows(self$results$contributions, contributionRows)

            private$.state$plotData <- do.call(
                rbind,
                lapply(descriptive$displayRows, as.data.frame, stringsAsFactors=FALSE))
            private$.state$contrastLabels <- unique(private$.state$plotData$contrast)
            private$.state$contrastTotals <- descriptive$contrastTotals
            if (isTRUE(self$options$simperPlots))
                private$.populateContributionPlots(descriptive)
            if (isTRUE(self$options$simperHeatmap))
                private$.populateOptionalHeatmap()
        },

        .populateContributionPlots = function(descriptive) {
            for (contrast in private$.state$contrastLabels) {
                item <- self$results$contributionPlots$addItem(contrast)
                item$setTitle(contrast)
                item$plot$setSize(580, 430)
                item$description$setContent(
                    private$.plotDescription(contrast))
                item$description$setVisible(FALSE)
                rows <- private$.state$plotData[
                    private$.state$plotData$contrast == contrast, , drop=FALSE]
                item$plot$setState(list(
                    rows=rows,
                    simperTop=self$options$simperTop,
                    simperCum=self$options$simperCum,
                    isFiltered=nrow(rows) < descriptive$contrastTotals[[contrast]]))
                for (row in seq_len(nrow(rows))) {
                    direction <- if (!is.finite(rows$meanFirst[[row]]) ||
                            !is.finite(rows$meanSecond[[row]])) {
                        "Mean unavailable"
                    } else if (rows$meanFirst[[row]] > rows$meanSecond[[row]]) {
                        paste0(rows$firstGroup[[row]], " higher")
                    } else if (rows$meanSecond[[row]] > rows$meanFirst[[row]]) {
                        paste0(rows$secondGroup[[row]], " higher")
                    } else {
                        "Equal means"
                    }
                    item$values$addRow(
                        rowKey=as.character(row),
                        values=list(
                            feature=as.character(rows$feature[[row]]),
                            average=miso_num_or_na(rows$average[[row]]),
                            contribution=miso_num_or_na(
                                rows$contribution[[row]]),
                            cumulative=miso_num_or_na(rows$cumulative[[row]]),
                            firstGroup=as.character(rows$firstGroup[[row]]),
                            meanFirst=miso_num_or_na(rows$meanFirst[[row]]),
                            secondGroup=as.character(rows$secondGroup[[row]]),
                            meanSecond=miso_num_or_na(rows$meanSecond[[row]]),
                            direction=direction))
                }
            }
        },

        .runAssessment = function(prep, displayRows) {
            private$.state$requestedPermutations <- as.integer(self$options$simperN)
            miso_set_seed(prep)
            fit <- tryCatch(
                vegan::simper(
                    prep$transformed,
                    prep$group,
                    permutations=private$.state$requestedPermutations),
                error=function(e) e)
            if (inherits(fit, "error")) {
                private$.state$warnings <- c(
                    private$.state$warnings,
                    paste0(
                        "Exploratory permutation assessment could not be calculated: ",
                        fit$message))
                return(FALSE)
            }

            pairs <- private$.contrastPairs(prep$group)
            if (length(fit) != length(pairs)) {
                private$.state$warnings <- c(
                    private$.state$warnings,
                    "Exploratory permutation assessment returned an unexpected set of contrasts and was omitted.")
                return(FALSE)
            }

            effective <- suppressWarnings(as.integer(attr(fit, "permutations")))
            if (length(effective) > 0L && is.finite(effective[[1L]]))
                private$.state$effectivePermutations <- effective[[1L]]

            adjusted <- vector("list", length(fit))
            missingContrasts <- character()
            for (index in seq_along(fit)) {
                p <- suppressWarnings(as.numeric(fit[[index]]$p))
                names(p) <- names(fit[[index]]$p)
                finite <- is.finite(p)
                if (! any(finite)) {
                    adjusted[[index]] <- setNames(numeric(), character())
                    missingContrasts <- c(
                        missingContrasts,
                        private$.contrastLabel(pairs[[index]]))
                    next
                }
                padj <- rep(NA_real_, length(p))
                if (identical(self$options$simperAdjust, "none"))
                    padj[finite] <- p[finite]
                else
                    padj[finite] <- stats::p.adjust(
                        p[finite], method=self$options$simperAdjust)
                names(padj) <- names(p)
                adjusted[[index]] <- cbind(p=p, padj=padj)
            }

            row <- 1L
            assessmentRows <- list()
            for (values in displayRows) {
                result <- adjusted[[values$contrastIndex]]
                if (is.null(dim(result)) || ! values$feature %in% rownames(result))
                    next
                p <- miso_num_or_na(result[values$feature, "p"])
                padj <- miso_num_or_na(result[values$feature, "padj"])
                if (! is.finite(p))
                    next
                assessmentRows[[length(assessmentRows) + 1L]] <- list(
                    key=as.character(length(assessmentRows) + 1L),
                    values=list(
                        contrast=values$contrast,
                        feature=values$feature,
                        p=p,
                        padj=padj))
                row <- row + 1L
            }
            miso_reconcile_table_rows(self$results$assessment, assessmentRows)

            if (length(missingContrasts) > 0L)
                private$.state$warnings <- c(
                    private$.state$warnings,
                    paste0(
                        "Permutation values were unavailable for: ",
                        paste(missingContrasts, collapse=", "), "."))
            if (row == 1L) {
                private$.state$warnings <- c(
                    private$.state$warnings,
                    "No usable permutation p-values were available; descriptive SIMPER output is retained.")
                return(FALSE)
            }
            TRUE
        },

        .setTableNotes = function(prep) {
            transformation <- if (identical(self$options$transform, "none")) NULL else
                sprintf("Transformation: %s.", private$.transformLabel(self$options$transform))
            descriptive <- private$.state$descriptive
            filtered <- length(descriptive$displayRows) < length(descriptive$fullRows)
            unavailable <- any(vapply(descriptive$fullRows, function(row)
                !is.finite(row$sd) || !is.finite(row$ratio), logical(1)))
            self$results$contrasts$setNote(
                key="meaning", note=transformation, init=FALSE)
            self$results$contributions$setNote(
                key="meaning",
                note=if (filtered) paste(c(transformation,
                    "Percentages use all usable features before display filtering and are not renormalised."), collapse=" ")
                    else transformation, init=FALSE)
            self$results$variability$setNote(
                key="meaning",
                note=paste0(
                    "SD describes variation in contributions across between-group sample pairs.",
                    if (unavailable) " Blank SD or ratio cells indicate unavailable values." else ""),
                init=FALSE)
            self$results$means$setNote(
                key="meaning", note=transformation, init=FALSE)
            self$results$table$setNote(
                key="meaning",
                note=paste(
                    "Average contribution is the mean feature contribution to Bray-Curtis dissimilarity.",
                    "Contribution SD describes variation across sample pairs; Average divided by SD describes consistency and is not a significance test.",
                    "First- and second-group means follow the named contrast and use the transformed scale.",
                    "Percentages use all usable features before display filtering; retained rows are not renormalised.",
                    "Blank SD or ratio cells mean that a finite value was unavailable."),
                init=FALSE)
            if (isTRUE(self$options$simperAssess))
                self$results$assessment$setNote(
                    key="scope",
                    note=paste(
                        "Group-label permutation tests of average contributions.",
                        if (identical(self$options$simperAdjust, "none")) "P-values are unadjusted." else sprintf(
                            "%s adjustment across all finite feature p-values within each contrast, before display filtering.",
                            private$.adjustLabel(self$options$simperAdjust))),
                    init=FALSE)
        },

        .plotDescription = function(contrast) "",

        .heatmapDescription = function() "",

        .buildContributionPlot = function (rows, contrast, top = self$options$simperTop,
            cumulative = self$options$simperCum, isFiltered = NULL)
        {
            if (is.null(rows) || nrow(rows) == 0L)
                return(NULL)
            # Older saved image states have no filtering flag. A cumulative
            # total below 100 establishes omission without consulting live options.
            if (is.null(isFiltered))
                isFiltered <- max(rows$cumulative, na.rm=TRUE) < 100 - 1e-8
            subtitle <- if (isTRUE(isFiltered)) sprintf(
                "Top %s or %s%% cumulative; crossing feature included", top, cumulative) else NULL
            rows <- rows[order(rows$contribution, rows$feature), , drop = FALSE]
            rows$feature <- factor(as.character(rows$feature), levels = unique(as.character(rows$feature)))
            featureLabels <- .misoUniqueShortLabels(levels(rows$feature), width = 24L)
            groupLabels <- .misoUniqueShortLabels(c(as.character(rows$firstGroup[[1L]]), as.character(rows$secondGroup[[1L]])),
                width = 18L)
            first <- unname(groupLabels[[1L]])
            second <- unname(groupLabels[[2L]])
            firstDirection <- paste0(first, " higher")
            secondDirection <- paste0(second, " higher")
            directionLevels <- c(firstDirection, secondDirection, "Equal means", "Mean unavailable")
            rows$direction <- ifelse(!is.finite(rows$meanFirst) | !is.finite(rows$meanSecond), "Mean unavailable",
                ifelse(rows$meanFirst > rows$meanSecond, firstDirection, ifelse(rows$meanSecond > rows$meanFirst,
                    secondDirection, "Equal means")))
            rows$direction <- factor(rows$direction, levels = directionLevels)
            rows$valueLabel <- sprintf("%.1f%% (cumulative %.1f%%)", rows$contribution, rows$cumulative)
            directions <- directionLevels[directionLevels %in% rows$direction]
            palette <- stats::setNames(c("#0072B2", "#D55E00", "#666666", "#B3B3B3"), directionLevels)
            lineTypes <- stats::setNames(c("solid", "longdash", "dotted", "dotdash"), directionLevels)
            ggplot2::ggplot(rows, ggplot2::aes(x = contribution, y = feature, fill = direction, linetype = direction)) +
                ggplot2::geom_col(width = 0.72, colour = "#222222", linewidth = 0.7) + ggplot2::geom_text(ggplot2::aes(label = valueLabel),
                hjust = -0.05, size = 3.5, colour = "#222222") + ggplot2::scale_fill_manual(values = palette,
                breaks = directions, labels = directions, drop = FALSE, name = "Transformed group mean") + ggplot2::scale_linetype_manual(values = lineTypes,
                breaks = directions, labels = directions, drop = FALSE, name = "Transformed group mean") + ggplot2::scale_x_continuous(labels = function(x) paste0(x,
                "%"), expand = ggplot2::expansion(mult = c(0, 0.42))) + ggplot2::scale_y_discrete(labels = featureLabels) +
                ggplot2::labs(x = "Contribution to average dissimilarity (%)", y = NULL, subtitle = subtitle) + .misoPlotTheme() + ggplot2::theme(legend.position = "bottom",
                plot.subtitle = ggplot2::element_text(size = 10, colour = "#444444"))
        }
,

        .plotContribution = function(image, ...) {
            contrast <- as.character(image$key)
            plotData <- image$state
            plot <- private$.buildContributionPlot(
                plotData$rows,
                contrast,
                top=plotData$simperTop,
                cumulative=plotData$simperCum,
                isFiltered=plotData$isFiltered)
            if (is.null(plot))
                return()
            suppressWarnings(print(plot))
            invisible(TRUE)
        },

        .heatmapData = function() {
            dat <- private$.state$plotData
            if (is.null(dat) || nrow(dat) == 0L)
                return(NULL)
            features <- unique(dat$feature)
            contrasts <- private$.state$contrastLabels
            grid <- expand.grid(
                feature=features,
                contrast=contrasts,
                stringsAsFactors=FALSE)
            key <- paste(dat$feature, dat$contrast, sep="\r")
            gridKey <- paste(grid$feature, grid$contrast, sep="\r")
            matched <- match(gridKey, key)
            grid$contribution <- dat$contribution[matched]
            grid$missing <- is.na(matched)
            grid
        },

        .buildHeatmapPlot = function (dat = private$.heatmapData())
        {
            if (is.null(dat))
                return(NULL)
            dat$feature <- factor(dat$feature, levels = unique(dat$feature))
            dat$contrast <- factor(dat$contrast, levels = unique(dat$contrast))
            featureLabels <- .misoUniqueShortLabels(levels(dat$feature), width = 20L)
            contrastLabels <- .misoUniqueShortLabels(levels(dat$contrast), width = 24L)
            grid <- dat[, c("feature", "contrast", "missing"), drop = FALSE]
            selected <- dat[!dat$missing, , drop = FALSE]
            ggplot2::ggplot(grid, ggplot2::aes(x = contrast, y = feature)) + ggplot2::geom_tile(fill = "#D9D9D9",
                colour = "white", linewidth = 0.35) + ggplot2::geom_tile(data = selected, ggplot2::aes(fill = contribution),
                colour = "white", linewidth = 0.35) + ggplot2::scale_fill_viridis_c(option = "C", direction = -1,
                na.value = "#D9D9D9", name = "Contribution (%)") + ggplot2::scale_x_discrete(labels = contrastLabels) +
                ggplot2::scale_y_discrete(labels = featureLabels) + ggplot2::labs(x = "Contrast", y = "Feature") +
                .misoPlotTheme() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1,
                vjust = 1))
        }
,

        .plotHeatmap = function(image, ...) {
            plot <- private$.buildHeatmapPlot(image$state)
            if (is.null(plot))
                return()
            suppressWarnings(print(plot))
            invisible(TRUE)
        },

        .contrastPairs = function(group) {
            observed <- as.character(unique(group))
            utils::combn(observed, 2L, simplify=FALSE)
        },

        .contrastLabel = function(pair) {
            quote <- any(grepl("\\bvs\\b", pair, ignore.case=TRUE))
            if (quote)
                paste0("\u201c", pair[[1L]], "\u201d vs \u201c", pair[[2L]], "\u201d")
            else
                paste(pair, collapse=" vs ")
        },

        .hasValue = function(x) {
            ! is.null(x) && length(x) > 0L && ! all(is.na(x) | x == "")
        },

        .htmlEscape = function(x) {
            x <- gsub("&", "&amp;", x, fixed=TRUE)
            x <- gsub("<", "&lt;", x, fixed=TRUE)
            x <- gsub(">", "&gt;", x, fixed=TRUE)
            x <- gsub('"', "&quot;", x, fixed=TRUE)
            x
        },

        .transformLabel = function(code) {
            labels <- c(
                none="None", sqrt="Square root", fourthroot="Fourth root",
                log="Log(x + 1)", pa="Presence/absence", wisconsin="Wisconsin",
                hellinger="Hellinger", total="Total", max="Max",
                frequency="Frequency", normalize="Normalize", range="Range",
                standardize="Standardize", chi.square="Chi-square",
                rclr="Reverse CLR")
            if (code %in% names(labels)) unname(labels[[code]]) else code
        },

        .distanceLabel = function(code) {
            labels <- c(
                bray="Bray-Curtis", jaccard="Jaccard", euclidean="Euclidean",
                manhattan="Manhattan", canberra="Canberra",
                kulczynski="Kulczynski", gower="Gower", morisita="Morisita",
                horn="Horn-Morisita", mountford="Mountford", raup="Raup-Crick",
                binomial="Binomial", chao="Chao", cao="Cao", clark="Clark",
                altGower="Alternative Gower", mahalanobis="Mahalanobis")
            if (code %in% names(labels)) unname(labels[[code]]) else code
        },

        .adjustLabel = function(code) {
            labels <- c(
                holm="Holm", bonferroni="Bonferroni",
                BH="Benjamini-Hochberg", BY="Benjamini-Yekutieli", none="None")
            if (code %in% names(labels)) unname(labels[[code]]) else code
        }
    )
)
