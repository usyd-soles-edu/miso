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
                self$options, excluded=c("simperDetails", "simperHeatmap"))
            if (!is.null(private$.lastStructuralKey) &&
                    identical(private$.lastStructuralKey, structuralKey)) {
                private$.refreshDisplayOnly()
                return()
            }
            private$.lastStructuralKey <- structuralKey
            private$.lastDisplayKey <- list(
                simperDetails=isTRUE(self$options$simperDetails),
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
                private$.showGuidance(paste(
                    "SIMPER is waiting for Feature variables.",
                    "Add one or more numeric response columns, such as species abundances."))
                return()
            }
            if (! hasFactor) {
                private$.showGuidance(paste(
                    "SIMPER is waiting for a Grouping variable.",
                    "Add one categorical column containing at least two observed groups."))
                return()
            }

            if (self$options$transform %in% c("standardize", "rclr")) {
                private$.showGuidance(paste(
                    "This transformation cannot produce valid non-negative inputs for Bray-Curtis SIMPER.",
                    "Choose a compatible transformation in Analysis choices."))
                return()
            }

            prep <- miso_prepare_resemblance(
                data=self$data,
                vars=self$options$vars,
                factor=self$options$factor,
                transform=self$options$transform,
                distance="bray",
                seed=self$options$seed,
                distBinary=FALSE)
            if (prep$error) {
                private$.showGuidance(prep$message)
                return()
            }

            transformed <- as.matrix(prep$transformed)
            if (any(! is.finite(transformed)) || any(transformed < 0) ||
                    any(rowSums(transformed) <= 0)) {
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
                        "The saved legacy distance request '%s' was ignored; SIMPER used Bray-Curtis.",
                        private$.distanceLabel(self$options$distance)))
            if (isTRUE(self$options$distBinary))
                private$.state$warnings <- c(
                    private$.state$warnings,
                    paste(
                        "The saved legacy Binary dissimilarity request was ignored.",
                        "This is separate from the visible Presence/absence transformation; SIMPER used abundance-based Bray-Curtis."))

            descriptive <- private$.runDescriptive(prep)
            if (is.null(descriptive))
                return()
            private$.state$descriptive <- descriptive

            private$.populateSummary(prep, length(descriptive$contrastRows))
            private$.populatePurposes()
            private$.populateDescriptive(descriptive)

            assessmentShown <- FALSE
            if (isTRUE(self$options$simperAssess))
                assessmentShown <- private$.runAssessment(prep, descriptive$displayRows)

            private$.setTableNotes(prep)
            private$.setInterpretation(prep, assessmentShown)
            private$.populateSettings(prep, assessmentShown)
            private$.setWarnings(private$.state$warnings)
            private$.showSuccessfulResults(assessmentShown)
        },

        .refreshDisplayOnly = function() {
            descriptive <- private$.state$descriptive
            details <- isTRUE(self$options$simperDetails)
            heatmap <- isTRUE(self$options$simperHeatmap)
            previousDisplay <- private$.lastDisplayKey
            private$.lastDisplayKey <- list(
                simperDetails=details,
                simperHeatmap=heatmap)
            detailsChanged <- is.null(previousDisplay) ||
                !identical(previousDisplay$simperDetails, details)
            heatmapChanged <- is.null(previousDisplay) ||
                !identical(previousDisplay$simperHeatmap, heatmap)

            if (detailsChanged) {
                miso_clear_table(self$results$variability)
                miso_clear_table(self$results$means)
                if (details && !is.null(descriptive))
                    private$.populateOptionalDetailRows(descriptive)
            }
            self$results$variability$setVisible(details && !is.null(descriptive))
            self$results$variabilityPurpose$setVisible(details && !is.null(descriptive))
            self$results$means$setVisible(details && !is.null(descriptive))
            self$results$meansPurpose$setVisible(details && !is.null(descriptive))

            if (heatmapChanged) {
                miso_clear_table(self$results$heatmapValues)
                if (heatmap && !is.null(descriptive))
                    private$.populateOptionalHeatmap()
            }
            self$results$heatmap$setVisible(heatmap && !is.null(descriptive))
            self$results$heatmapDescription$setVisible(heatmap && !is.null(descriptive))
            self$results$heatmapValues$setVisible(heatmap && !is.null(descriptive))
            self$results$heatmapValuesPurpose$setVisible(heatmap && !is.null(descriptive))
        },

        .populateOptionalDetailRows = function(descriptive) {
            for (index in seq_along(descriptive$fullRows)) {
                values <- descriptive$fullRows[[index]]
                values$contrastIndex <- NULL
                values$firstGroup <- NULL
                values$secondGroup <- NULL
                self$results$variability$addRow(
                    rowKey=as.character(index),
                    values=values[c("contrast", "feature", "average", "sd", "ratio")])
                self$results$means$addRow(
                    rowKey=as.character(index),
                    values=values[c("contrast", "feature", "meanFirst", "meanSecond")])
            }
        },

        .populateOptionalHeatmap = function() {
            self$results$heatmapDescription$setContent(
                private$.heatmapDescription())
            heatmapData <- private$.heatmapData()
            self$results$heatmap$setState(heatmapData)
            if (is.null(heatmapData))
                return()
            for (row in seq_len(nrow(heatmapData)))
                self$results$heatmapValues$addRow(
                    rowKey=as.character(row),
                    values=list(
                        contrast=as.character(heatmapData$contrast[[row]]),
                        feature=as.character(heatmapData$feature[[row]]),
                        contribution=if (isTRUE(heatmapData$missing[[row]]))
                            ""
                        else
                            miso_num_or_na(heatmapData$contribution[[row]]),
                        selected=if (isTRUE(heatmapData$missing[[row]]))
                            "No" else "Yes"))
        },

        .clearResults = function() {
            self$results$guidance$setContent("")
            self$results$warnings$setContent("")
            miso_clear_fixed_table(self$results$summary, 5L)
            miso_clear_table(self$results$contrasts)
            miso_clear_table(self$results$contributions)
            miso_clear_table(self$results$variability)
            miso_clear_table(self$results$means)
            miso_clear_table(self$results$table)
            miso_clear_table(self$results$assessment)
            miso_clear_table(self$results$heatmapValues)
            self$results$contributionPlots$clear()
            self$results$contrasts$setNote(key="meaning", note="")
            self$results$contributions$setNote(key="meaning", note="")
            self$results$variability$setNote(key="meaning", note="")
            self$results$means$setNote(key="meaning", note="")
            self$results$table$setNote(key="meaning", note="")
            self$results$assessment$setNote(key="scope", note="")
            self$results$heatmapDescription$setContent("")
            self$results$note$setContent("")
            miso_clear_table(self$results$settings)
            for (name in c(
                    "summaryPurpose", "contrastsPurpose",
                    "contributionsPurpose", "variabilityPurpose",
                    "meansPurpose", "heatmapValuesPurpose",
                    "assessmentPurpose", "settingsPurpose"))
                self$results[[name]]$setContent("")
            self$results$heatmap$setSize(600, 500)

            for (name in c(
                    "guidance", "summary", "summaryPurpose", "warnings",
                    "contrasts", "contrastsPurpose", "contributions",
                    "contributionsPurpose", "variability",
                    "variabilityPurpose", "means", "meansPurpose", "table",
                    "contributionPlots", "heatmap", "heatmapDescription",
                    "heatmapValues", "heatmapValuesPurpose", "assessment",
                    "assessmentPurpose", "note", "settings",
                    "settingsPurpose"))
                self$results[[name]]$setVisible(FALSE)
            for (name in c("summaryPurpose", "summary", "contrastsPurpose", "contrasts", "contributionsPurpose", "contributions", "settingsPurpose", "settings"))
                self$results[[name]]$setVisible(TRUE)
        },

        .showGuidance = function(content, title="Action needed") {
            self$results$guidance$setTitle(title)
            self$results$guidance$setContent(miso_html_block(content))
            self$results$guidance$setVisible(TRUE)
        },

        .showSuccessfulResults = function(assessmentShown=FALSE) {
            self$results$guidance$setVisible(FALSE)
            for (name in c(
                    "summary", "summaryPurpose", "contrasts",
                    "contrastsPurpose", "contributions",
                    "contributionsPurpose", "contributionPlots", "note",
                    "settings", "settingsPurpose"))
                self$results[[name]]$setVisible(TRUE)
            self$results$heatmap$setVisible(isTRUE(self$options$simperHeatmap))
            self$results$heatmapDescription$setVisible(
                isTRUE(self$options$simperHeatmap))
            self$results$heatmapValues$setVisible(
                isTRUE(self$options$simperHeatmap))
            self$results$heatmapValuesPurpose$setVisible(
                isTRUE(self$options$simperHeatmap))
            self$results$variability$setVisible(isTRUE(self$options$simperDetails))
            self$results$variabilityPurpose$setVisible(
                isTRUE(self$options$simperDetails))
            self$results$means$setVisible(isTRUE(self$options$simperDetails))
            self$results$meansPurpose$setVisible(
                isTRUE(self$options$simperDetails))
            self$results$table$setVisible(FALSE)
            self$results$assessment$setVisible(isTRUE(assessmentShown))
            self$results$assessmentPurpose$setVisible(isTRUE(assessmentShown))
        },

        .populatePurposes = function() {
            miso_populate_purposes(self$results, list(
                summaryPurpose=c(
                    "Data summary",
                    "Summarises included samples and features, including any exclusions."),
                contrastsPurpose=c(
                    "Contrast summary",
                    "Summarises group sizes and mean Bray-Curtis dissimilarity for each contrast."),
                contributionsPurpose=c(
                    "Descriptive feature contributions",
                    "Ranks features by their contribution to average dissimilarity."),
                variabilityPurpose=c(
                    "Contribution variability",
                    "Shows how consistently each feature contributes across sample pairs."),
                meansPurpose=c(
                    "Group means",
                    "Shows each feature's mean in both groups on the analysis scale."),
                heatmapValuesPurpose=c(
                    "SIMPER contrast heatmap values",
                    "Lists the values represented in the contribution heatmap."),
                assessmentPurpose=c(
                    "Exploratory permutation assessment",
                    "Compares observed feature contributions with contributions from permuted group labels."),
                settingsPurpose=c(
                    "Analysis settings",
                    "Lists the options used for this analysis.")))
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

        .populateSummary = function(prep, contrastCount) {
            rows <- list(
                c("Analysed samples", prep$rowsUsed),
                c("Retained features", prep$varsUsed),
                c("Grouping variable", prep$primary),
                c("Observed groups", length(unique(as.character(prep$group)))),
                c("Contrasts", contrastCount))
            for (index in seq_along(rows))
                miso_set_fixed_row(
                    self$results$summary, index,
                    values=list(item=rows[[index]][[1L]], value=as.character(rows[[index]][[2L]])))
        },

        .populateDescriptive = function(descriptive) {
            for (index in seq_along(descriptive$contrastRows))
                self$results$contrasts$addRow(
                    rowKey=as.character(index),
                    values=descriptive$contrastRows[[index]])

            for (index in seq_along(descriptive$fullRows)) {
                values <- descriptive$fullRows[[index]]
                values$contrastIndex <- NULL
                values$firstGroup <- NULL
                values$secondGroup <- NULL
                self$results$table$addRow(
                    rowKey=as.character(index),
                    values=values)
                if (isTRUE(self$options$simperDetails)) {
                    self$results$variability$addRow(
                        rowKey=as.character(index),
                        values=values[c(
                            "contrast", "feature", "average", "sd", "ratio")])
                    self$results$means$addRow(
                        rowKey=as.character(index),
                        values=values[c(
                            "contrast", "feature", "meanFirst", "meanSecond")])
                }
            }

            for (index in seq_along(descriptive$displayRows)) {
                values <- descriptive$displayRows[[index]]
                self$results$contributions$addRow(
                    rowKey=as.character(index),
                    values=values[c(
                        "contrast", "feature", "contribution", "cumulative")])
            }

            private$.state$plotData <- do.call(
                rbind,
                lapply(descriptive$displayRows, as.data.frame, stringsAsFactors=FALSE))
            private$.state$contrastLabels <- unique(private$.state$plotData$contrast)
            private$.state$contrastTotals <- descriptive$contrastTotals
            for (contrast in private$.state$contrastLabels) {
                item <- self$results$contributionPlots$addItem(contrast)
                item$setTitle(contrast)
                item$plot$setSize(580, 430)
                item$description$setContent(
                    private$.plotDescription(contrast))
                item$valuesPurpose$setContent(miso_html_block(
                    "Lists the values represented in this contribution plot.",
                    ariaLabel=paste(
                        "About SIMPER contribution values for", contrast),
                    title="SIMPER contribution values"))
                rows <- private$.state$plotData[
                    private$.state$plotData$contrast == contrast, , drop=FALSE]
                item$plot$setState(list(
                    rows=rows,
                    simperTop=self$options$simperTop,
                    simperCum=self$options$simperCum))
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
            if (isTRUE(self$options$simperHeatmap))
                private$.populateOptionalHeatmap()
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
            for (values in displayRows) {
                result <- adjusted[[values$contrastIndex]]
                if (is.null(dim(result)) || ! values$feature %in% rownames(result))
                    next
                p <- miso_num_or_na(result[values$feature, "p"])
                padj <- miso_num_or_na(result[values$feature, "padj"])
                if (! is.finite(p))
                    next
                self$results$assessment$addRow(
                    rowKey=as.character(row),
                    values=list(
                        contrast=values$contrast,
                        feature=values$feature,
                        p=p,
                        padj=padj))
                row <- row + 1L
            }

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
            self$results$contrasts$setNote(
                key="meaning",
                note=paste(
                    "Mean dissimilarity is the full average Bray-Curtis quantity decomposed for each contrast.",
                    "n (first) and n (second) follow the named contrast order."))
            self$results$contributions$setNote(
                key="meaning",
                note=paste(
                    "Percentages use all usable features before display filtering; retained rows are not renormalised."))
            self$results$variability$setNote(
                key="meaning",
                note=paste(
                    "Average is the mean feature contribution to Bray-Curtis dissimilarity.",
                    "SD describes variation in contributions across sample pairs.",
                    "Average/SD describes consistency and is not a significance test.",
                    "Blank cells mean that a finite value was unavailable."))
            self$results$means$setNote(
                key="meaning",
                note=paste(
                    "First and second means follow the named contrast order and use the transformed scale."))
            self$results$table$setNote(
                key="meaning",
                note=paste(
                    "Average contribution is the mean feature contribution to Bray-Curtis dissimilarity.",
                    "Contribution SD describes variation across sample pairs; Average divided by SD describes consistency and is not a significance test.",
                    "First- and second-group means follow the named contrast and use the transformed scale.",
                    "Percentages use all usable features before display filtering; retained rows are not renormalised.",
                    "Blank SD or ratio cells mean that a finite value was unavailable."))
            if (isTRUE(self$options$simperAssess))
                self$results$assessment$setNote(
                    key="scope",
                    note=paste(
                        "P-values compare observed average contributions with random group-label assignments.",
                        sprintf(
                            "%s adjustment was applied separately within each contrast across all finite feature p-values before display filtering.",
                            private$.adjustLabel(self$options$simperAdjust))))
        },

        .plotDescription = function(contrast) {
            miso_html_block(
                "Shows the leading feature contributions for each contrast.",
                ariaLabel=paste("About SIMPER contribution plot for", contrast),
                title="Contribution plot")
        },

        .heatmapDescription = function() {
            if (is.null(private$.heatmapData()))
                return("")
            miso_html_block(
                "Compares leading feature contributions across contrasts.",
                ariaLabel="About the SIMPER contrast heatmap",
                title="Contrast overview heatmap")
        },

        .setInterpretation = function(prep, assessmentShown) {
            self$results$note$setContent(miso_html_block(paste(
                "SIMPER contributions are descriptive.",
                "Average/SD shows consistency; permutation p does not replace an overall test."),
                title="How to read these results"))
        },

        .populateSettings = function(prep, assessmentShown) {
            add <- function(setting, value) {
                key <- as.character(length(self$results$settings$rowKeys) + 1L)
                self$results$settings$addRow(
                    rowKey=key,
                    values=list(setting=setting, value=as.character(value)))
            }

            add("Transformation", private$.transformLabel(self$options$transform))
            add("Effective dissimilarity", "Bray-Curtis (fixed for SIMPER)")
            if (! identical(self$options$distance, "bray"))
                add("Ignored legacy distance request", private$.distanceLabel(self$options$distance))
            if (isTRUE(self$options$distBinary))
                add("Ignored legacy Binary request", "Yes; Presence/absence transformation is separate")
            add("Analysed samples", prep$rowsUsed)
            add("Retained features", prep$varsUsed)
            add("Grouping variable", prep$primary)
            add("Observed groups", length(unique(as.character(prep$group))))
            add("Contrasts", length(private$.state$contrastLabels))
            add("Top features cap", self$options$simperTop)
            add("Cumulative contribution threshold", paste0(self$options$simperCum, "%"))
            add("Detailed statistics", if (isTRUE(self$options$simperDetails)) "Shown" else "Hidden")
            add("Permutation assessment", if (isTRUE(self$options$simperAssess)) "Enabled" else "Disabled")
            if (isTRUE(self$options$simperAssess)) {
                add("Requested permutations", private$.state$requestedPermutations)
                effective <- private$.state$effectivePermutations
                add(
                    "Effective permutations",
                    if (is.finite(effective)) effective else "Unavailable")
                add("P-value adjustment", private$.adjustLabel(self$options$simperAdjust))
                add(
                    "Random seed",
                    if (is.na(prep$seed)) "Random on recalculation" else prep$seed)
                add("Assessment results shown", if (isTRUE(assessmentShown)) "Yes" else "No")
            }
        },

        .buildContributionPlot = function (rows, contrast, top = self$options$simperTop,
            cumulative = self$options$simperCum)
        {
            if (is.null(rows) || nrow(rows) == 0L)
                return(NULL)
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
                ggplot2::labs(x = "Contribution to average dissimilarity (%)", y = NULL, subtitle = sprintf("Top %s or %s%% cumulative; threshold-crossing feature included",
                    top, cumulative)) + .misoPlotTheme() + ggplot2::theme(legend.position = "bottom",
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
                cumulative=plotData$simperCum)
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
