# This file is a generated template, your changes will not be overwritten

simperClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "simperClass",
    inherit = simperBase,
    private = list(
        .state = list(),
        .lastStructuralKey = NULL,

        .init = function() {
            if (is.null(private$.lastStructuralKey))
                private$.showEmptyTables()
        },

        .postInit = function() {
            # Header-only loads can recover scalar image state without a fit.
            if (identical(private$.dataProvided, FALSE)) {
                if (!is.null(self$results$heatmap$state))
                    private$.sizeHeatmap(self$results$heatmap$state)
                self$results$heatmap$setVisible(isTRUE(self$options$simperHeatmap) &&
                    !is.null(self$results$heatmap$state))
            }
        },

        .showEmptyTables = function() {
            miso_show_empty_tables(self$results, c(
                    contrasts=TRUE, contributions=TRUE,
                    assessment=isTRUE(self$options$simperAssess)))
        },

        .run = function() {
            on.exit(miso_finish_empty_tables(self$results), add=TRUE)
            effectiveSeed <- miso_effective_seed(
                self$options$useFixedSeed, self$options$seed,
                enabled=isTRUE(self$options$simperAssess))
            structuralKey <- miso_options_signature(
                self$options,
                excluded=c("simperDetails", "simperPlots", "simperHeatmap",
                    "simperTop", "simperCum", "seed", "useFixedSeed"),
                data=self$data,
                extra=list(effectiveSeed=effectiveSeed))
            if (!is.null(private$.lastStructuralKey) &&
                    identical(private$.lastStructuralKey, structuralKey)) {
                private$.refreshDisplayOnly()
                return()
            }
            private$.lastStructuralKey <- structuralKey
            private$.state <- list(
                operationalWarnings=character(),
                assessmentShown=FALSE,
                assessmentSeedNote=NULL,
                plotData=NULL,
                contrastTotals=integer(),
                contrastLabels=character(),
                requestedPermutations=NA_integer_,
                effectivePermutations=NA_integer_,
                detailsReady=FALSE,
                contributionPlotsReady=FALSE,
                heatmapReady=FALSE,
                heatmapDataReady=FALSE,
                heatmapData=NULL,
                selectionSettings=NULL,
                assessmentValues=NULL,
                descriptive=NULL)
            private$.clearResults()

            hasVars <- length(self$options$vars) > 0L
            hasFactor <- private$.hasValue(self$options$factor)
            if (! hasVars && ! hasFactor) {
                private$.discardKeyedRows()
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
                    seed=effectiveSeed,
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

            private$.state$operationalWarnings <- prep$warnings
            if (! identical(self$options$distance, "bray"))
                private$.state$operationalWarnings <- c(
                    private$.state$operationalWarnings,
                    sprintf(
                        "Saved distance '%s' was ignored; SIMPER used Bray-Curtis.",
                        private$.distanceLabel(self$options$distance)))
            if (isTRUE(self$options$distBinary))
                private$.state$operationalWarnings <- c(
                    private$.state$operationalWarnings,
                    paste(
                        "The saved binary-distance setting was ignored; Bray-Curtis used the selected transformation."))

            descriptive <- private$.runDescriptive(prep)
            if (is.null(descriptive)) {
                private$.discardKeyedRows()
                return()
            }
            descriptive$displayRows <- private$.selectDisplayRows(descriptive$fullRows)
            private$.state$descriptive <- descriptive
            private$.populateDescriptive(descriptive)
            private$.state$selectionSettings <- c(self$options$simperTop, self$options$simperCum)

            assessmentShown <- FALSE
            if (isTRUE(self$options$simperAssess)) {
                prep <- miso_record_seed(prep, self$results$seedState)
                assessmentShown <- private$.runAssessment(prep, descriptive$displayRows)
            }
            if (! assessmentShown)
                miso_clear_table(self$results$assessment)

            private$.state$assessmentShown <- assessmentShown
            private$.state$assessmentSeedNote <- if (!is.null(private$.state$assessmentValues))
                paste0("Random seed: ", miso_seed_label(prep), ".") else NULL
            private$.refreshDisplayOnly()
        },

        .refreshDisplayOnly = function() {
            descriptive <- private$.state$descriptive
            if (is.null(descriptive)) {
                private$.showEmptyTables()
                return()
            }
            selection <- c(self$options$simperTop, self$options$simperCum)
            if (!identical(private$.state$selectionSettings, selection)) {
                descriptive$displayRows <- private$.selectDisplayRows(descriptive$fullRows)
                private$.state$descriptive <- descriptive
                private$.populateDescriptive(descriptive)
                if (!is.null(private$.state$assessmentValues))
                    private$.state$assessmentShown <- private$.populateAssessment(
                        descriptive$displayRows, private$.state$assessmentValues)
                private$.state$detailsReady <- FALSE
                private$.state$contributionPlotsReady <- FALSE
                private$.state$heatmapReady <- FALSE
                private$.state$heatmapDataReady <- FALSE
                self$results$heatmap$.setPath(NULL)
                for (item in self$results$contributionPlots$items)
                    item$plot$.setPath(NULL)
                private$.state$selectionSettings <- selection
            }
            details <- isTRUE(self$options$simperDetails)
            plots <- isTRUE(self$options$simperPlots)
            heatmap <- isTRUE(self$options$simperHeatmap)
            if (details && !private$.state$detailsReady) {
                private$.populateOptionalDetailRows(descriptive)
                private$.state$detailsReady <- TRUE
            }
            self$results$detailsByContrast$setVisible(details)
            if (plots && !private$.state$contributionPlotsReady) {
                private$.populateContributionPlots(descriptive)
                private$.state$contributionPlotsReady <- TRUE
            }
            self$results$contributionPlots$setVisible(plots)
            if (heatmap && !private$.state$heatmapDataReady) {
                private$.state$heatmapData <- private$.heatmapData()
                private$.state$heatmapDataReady <- TRUE
            }
            if (heatmap && !private$.state$heatmapReady) {
                self$results$heatmap$setState(private$.state$heatmapData)
                private$.sizeHeatmap(private$.state$heatmapData)
                private$.state$heatmapReady <- TRUE
            }
            self$results$heatmap$setVisible(heatmap)
            self$results$heatmapDescription$setVisible(FALSE)
            self$results$contrasts$setVisible(TRUE)
            self$results$contributions$setVisible(TRUE)
            self$results$assessment$setVisible(private$.state$assessmentShown)
            private$.refreshPresentation()
        },

        # Reuse existing cells when the row shape is unchanged; build new rows
        # through the documented Table API.
        .populateRows = function(table, rows) {
            keys <- as.character(seq_along(rows))
            if (length(rows) > 0L && identical(
                    as.character(unlist(table$rowKeys, use.names=FALSE)), keys)) {
                for (i in seq_along(rows))
                    table$setRow(rowNo=i, values=rows[[i]]$values)
                return()
            }
            miso_clear_table(table)
            for (i in seq_along(rows))
                table$addRow(rowKey=keys[[i]], values=rows[[i]]$values)
        },

        .populateOptionalDetailRows = function(descriptive) {
            rows <- descriptive$displayRows
            indices <- unique(vapply(rows, function(row) row$contrastIndex, integer(1)))
            for (index in indices) {
                selected <- Filter(function(row) row$contrastIndex == index, rows)
                key <- as.character(index)
                table <- if (key %in% unlist(self$results$detailsByContrast$itemKeys))
                    self$results$detailsByContrast$get(key=key)
                else
                    self$results$detailsByContrast$addItem(key)
                table$setTitle(selected[[1L]]$contrast)
                table$getColumn("meanFirst")$setTitle(paste0(selected[[1L]]$firstGroup, " mean"))
                table$getColumn("meanSecond")$setTitle(paste0(selected[[1L]]$secondGroup, " mean"))
                values <- lapply(selected, function(row) list(values=row[c(
                    "feature", "meanFirst", "meanSecond", "average", "sd", "ratio")]))
                private$.populateRows(table, values)

            }
        },

        .clearResults = function() {
            self$results$assessment$setNote(key="seed", note="", init=FALSE)
            # A new effective seed can refit without a raw-option clearWith hit.
            self$results$heatmap$.setPath(NULL)
            self$results$guidance$setContent("")
            self$results$warnings$setContent("")
            miso_clear_table_values(self$results$contrasts)
            miso_clear_table_values(self$results$contributions)
            miso_clear_table_values(self$results$assessment)
            self$results$detailsByContrast$clear()
            self$results$contributionPlots$clear()
            self$results$contrasts$setNote(key="meaning", note=NULL, init=FALSE)
            self$results$contributions$setNote(key="meaning", note=NULL, init=FALSE)
            self$results$assessment$setNote(key="scope", note=NULL, init=FALSE)
            self$results$heatmapDescription$setContent("")
            self$results$heatmap$setSize(600, 500)

            for (name in c(
                    "guidance", "warnings",
                    "contrasts", "contributions",
                    "detailsByContrast",
                    "contributionPlots", "heatmap", "heatmapDescription",
                    "assessment"
                    ))
                self$results[[name]]$setVisible(FALSE)
            private$.showEmptyTables()
        },

        # Destructive row removal for keyed result tables on rerun paths that
        # do not repopulate them: guidance, rejections, and failed descriptive
        # or assessment runs must not leave stale or blank rows behind.
        .discardKeyedRows = function() {
            miso_clear_table(self$results$contrasts)
            miso_clear_table(self$results$contributions)
            miso_clear_table(self$results$assessment)
        },

        .showGuidance = function(content, title="Action needed") {
            self$results$guidance$setTitle(title)
            self$results$guidance$setContent(miso_html_block(content))
            self$results$guidance$setVisible(TRUE)
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
                    private$.simperGroup(prep$group),
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
            contrastRows <- list()
            fullRows <- list()
            contrastTotals <- integer()
            diagnostics <- list()
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
                diagnostics <- c(diagnostics,
                    private$.classifyDetailDiagnostics(tab, prep, pair, index))
                contrastTotals[[as.character(index)]] <- nrow(tab)

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
            }

            if (length(failed) > 0L)
                private$.state$operationalWarnings <- c(
                    private$.state$operationalWarnings,
                    paste0(
                        "No usable descriptive contributions were returned for: ",
                        paste(failed, collapse=", "), "."))
            if (length(fullRows) == 0L) {
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
                contrastTotals=contrastTotals,
                diagnostics=diagnostics)
        },

        .selectDisplayRows = function(rows) {
            indices <- unique(vapply(rows, function(row) row$contrastIndex, integer(1)))
            selected <- list()
            for (index in indices) {
                contrast <- Filter(function(row) row$contrastIndex == index, rows)
                cumulative <- vapply(contrast, function(row) row$cumulative, numeric(1))
                crossing <- which(cumulative >= self$options$simperCum)[1L]
                if (is.na(crossing))
                    crossing <- length(contrast)
                count <- min(as.integer(self$options$simperTop), crossing, length(contrast))
                selected <- c(selected, contrast[seq_len(count)])
            }
            selected
        },

        .populateDescriptive = function(descriptive) {
            contrastRows <- lapply(seq_along(descriptive$contrastRows), function(index)
                list(
                    key=as.character(index),
                    values=descriptive$contrastRows[[index]]))
            private$.populateRows(self$results$contrasts, contrastRows)

            contributionRows <- lapply(
                seq_along(descriptive$displayRows), function(index) {
                    values <- descriptive$displayRows[[index]]
                    list(
                        key=as.character(index),
                        values=values[c(
                            "contrast", "feature", "contribution", "cumulative")])
                })
            private$.populateRows(self$results$contributions, contributionRows)

            private$.state$plotData <- do.call(
                rbind,
                lapply(descriptive$displayRows, as.data.frame, stringsAsFactors=FALSE))
            indices <- !duplicated(private$.state$plotData$contrastIndex)
            private$.state$contrastLabels <- private$.state$plotData$contrast[indices]
            private$.state$contrastTotals <- descriptive$contrastTotals
        },

        .populateContributionPlots = function(descriptive) {
            for (index in unique(private$.state$plotData$contrastIndex)) {
                rows <- private$.state$plotData[
                    private$.state$plotData$contrastIndex == index, , drop=FALSE]
                contrast <- rows$contrast[[1L]]
                key <- as.character(index)
                item <- if (key %in% unlist(self$results$contributionPlots$itemKeys))
                    self$results$contributionPlots$get(key=key)
                else
                    self$results$contributionPlots$addItem(key)
                item$setTitle(contrast)
                item$plot$setSize(580, 430)
                item$description$setContent(
                    private$.plotDescription(contrast))
                item$description$setVisible(FALSE)
                item$plot$setState(list(
                    rows=rows,
                    simperTop=self$options$simperTop,
                    simperCum=self$options$simperCum,
                    isFiltered=nrow(rows) < descriptive$contrastTotals[[as.character(index)]]))
            }
        },

        .runAssessment = function(prep, displayRows) {
            private$.state$requestedPermutations <- as.integer(self$options$simperN)
            miso_set_seed(prep)
            fit <- tryCatch(
                vegan::simper(
                    prep$transformed,
                    private$.simperGroup(prep$group),
                    permutations=private$.state$requestedPermutations),
                error=function(e) e)
            if (inherits(fit, "error")) {
                private$.state$operationalWarnings <- c(
                    private$.state$operationalWarnings,
                    paste0(
                        "Exploratory permutation assessment could not be calculated: ",
                        fit$message))
                return(FALSE)
            }

            pairs <- private$.contrastPairs(prep$group)
            if (length(fit) != length(pairs)) {
                private$.state$operationalWarnings <- c(
                    private$.state$operationalWarnings,
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

            private$.state$assessmentValues <- adjusted
            if (length(missingContrasts) > 0L)
                private$.state$operationalWarnings <- c(
                    private$.state$operationalWarnings,
                    paste0(
                        "Permutation values were unavailable for: ",
                        paste(missingContrasts, collapse=", "), "."))
            private$.populateAssessment(displayRows, adjusted)
        },

        .populateAssessment = function(displayRows, adjusted) {
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
            }
            private$.populateRows(self$results$assessment, assessmentRows)
            length(assessmentRows) > 0L
        },

        .classifyDetailDiagnostics = function(tab, prep, pair, contrastIndex) {
            group <- as.character(prep$group)
            pairRows <- group %in% pair
            pairCount <- sum(group == pair[[1L]]) * sum(group == pair[[2L]])
            diagnostics <- list()
            for (i in seq_len(nrow(tab))) {
                sd <- tab$sd[[i]]
                ratio <- tab$ratio[[i]]
                if (is.finite(sd) && is.finite(ratio))
                    next
                reason <- "unexpected"
                if (!is.finite(sd) && pairCount < 2L) {
                    reason <- "single_pair"
                } else if (is.finite(sd) && sd == 0 && !is.finite(ratio)) {
                    if (tab$average[[i]] == 0) {
                        # Range can map positive abundances to zero; absence
                        # must be established before transformation.
                        absent <- all(prep$comm[pairRows, tab$feature[[i]]] == 0)
                        reason <- if (absent) "absent" else "zero_present"
                    } else if (tab$average[[i]] > 0) {
                        reason <- "constant_positive"
                    }
                }
                diagnostics[[length(diagnostics) + 1L]] <- list(
                    contrastIndex=contrastIndex, feature=tab$feature[[i]],
                    pairCount=pairCount, reason=reason)
            }
            diagnostics
        },

        .detailDiagnostics = function(index=NULL) {
            rows <- private$.state$descriptive$displayRows
            keys <- vapply(rows, function(row)
                paste(row$contrastIndex, row$feature, sep="\r"), character(1))
            Filter(function(issue)
                (is.null(index) || issue$contrastIndex == index) &&
                paste(issue$contrastIndex, issue$feature, sep="\r") %in% keys,
                private$.state$descriptive$diagnostics)
        },

        .detailNote = function(diagnostics=private$.detailDiagnostics()) {
            reasons <- vapply(diagnostics,
                function(issue) issue$reason, character(1))
            paste(c(
                "SD describes variation in contributions across between-group sample pairs.",
                "Average divided by SD describes consistency and is not a significance test.",
                if (any(reasons %in% c("absent", "zero_present", "constant_positive")))
                    "A blank ratio can occur when SD is zero.",
                if ("absent" %in% reasons)
                    "Features absent in the retained samples from both groups have zero contribution; their ratio is undefined.",
                if ("zero_present" %in% reasons)
                    "Zero contribution can also occur when the transformed feature values are identical across sample pairs.",
                if ("constant_positive" %in% reasons)
                    "A feature can have a positive contribution with SD zero when its contribution is the same across all sample pairs.",
                if ("single_pair" %in% reasons)
                    "SD and Average divided by SD are blank when a contrast has fewer than two between-group sample pairs.",
                if ("unexpected" %in% reasons)
                    "Other blank SD or ratio cells indicate that a finite value could not be calculated."),
                collapse=" ")
        },

        .refreshPresentation = function() {
            warnings <- private$.state$operationalWarnings
            if (!is.null(private$.state$assessmentValues) &&
                    !isTRUE(private$.state$assessmentShown))
                warnings <- c(warnings,
                    "No usable permutation p-values were available; descriptive SIMPER output is retained.")
            if (isTRUE(self$options$simperDetails)) {
                diagnostics <- private$.detailDiagnostics()
                singlePairs <- Filter(function(issue)
                    identical(issue$reason, "single_pair"), diagnostics)
                count <- length(unique(vapply(singlePairs,
                    function(issue) issue$contrastIndex, integer(1))))
                if (count > 0L)
                    warnings <- c(warnings, sprintf(
                        "Contribution SD could not be calculated for %d %s with only one between-group sample pair. See Group Means and Contribution Variability for details.",
                        count, if (count == 1L) "contrast" else "contrasts"))
                unexpected <- sum(vapply(diagnostics,
                    function(issue) identical(issue$reason, "unexpected"), logical(1)))
                if (unexpected > 0L)
                    warnings <- c(warnings, sprintf(
                        "A finite SD or ratio could not be calculated for %d %s. See Group Means and Contribution Variability for details.",
                        unexpected, if (unexpected == 1L) "feature row" else "feature rows"))
            }
            private$.setWarnings(warnings)
            private$.setTableNotes()
        },

        .setTableNotes = function() {
            transformation <- if (identical(self$options$transform, "none")) NULL else
                sprintf("Transformation: %s.", private$.transformLabel(self$options$transform))
            descriptive <- private$.state$descriptive
            filtered <- length(descriptive$displayRows) < length(descriptive$fullRows)
            self$results$contrasts$setNote(
                key="meaning", note=transformation, init=FALSE)
            self$results$contributions$setNote(
                key="meaning",
                note=if (filtered) paste(c(transformation,
                    "Percentages use all usable features before display filtering and are not renormalised."), collapse=" ")
                    else transformation, init=FALSE)
            for (table in self$results$detailsByContrast$items) {
                table$setNote(key="meaning", note=paste(
                    "Features follow the Top N or cumulative contribution limit; the threshold-crossing feature is included.",
                    if (identical(self$options$transform, "none")) "Means use untransformed values."
                    else sprintf("Means use %s-transformed values.", private$.transformLabel(self$options$transform)),
                    private$.detailNote(private$.detailDiagnostics(as.integer(table$key)))), init=FALSE)
            }
            self$results$assessment$setNote(
                key="scope", note=if (isTRUE(private$.state$assessmentShown)) paste(
                    "Group-label permutation tests of average contributions.",
                    if (identical(self$options$simperAdjust, "none")) "P-values are unadjusted." else sprintf(
                        "%s adjustment across all finite feature p-values within each contrast, before display filtering.",
                        private$.adjustLabel(self$options$simperAdjust))) else NULL,
                init=FALSE)
            self$results$assessment$setNote(key="seed",
                note=private$.state$assessmentSeedNote, init=FALSE)
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
            contrasts <- unique(dat$contrastIndex)
            grid <- expand.grid(
                feature=features,
                contrastIndex=contrasts,
                stringsAsFactors=FALSE)
            grid$contrast <- dat$contrast[match(grid$contrastIndex, dat$contrastIndex)]
            key <- paste(match(dat$feature, features), dat$contrastIndex, sep=":")
            gridKey <- paste(match(grid$feature, features), grid$contrastIndex, sep=":")
            matched <- match(gridKey, key)
            grid$contribution <- dat$contribution[matched]
            grid$missing <- is.na(matched)
            grid
        },

        .heatmapLayout = function(dat) {
            features <- unique(dat$feature)
            labels <- stats::setNames(vapply(features, function(value) {
                characters <- regmatches(value, gregexpr("\\X", value, perl=TRUE))[[1L]]
                lines <- character()
                while (length(characters) > 0L) {
                    newline <- match("\n", characters, nomatch=length(characters) + 1L)
                    widths <- cumsum(nchar(characters, type="width"))
                    end <- which(widths > 30L)[1L]
                    if (is.na(end)) end <- length(characters) + 1L
                    if (newline <= end) {
                        lines <- c(lines, paste0(characters[seq_len(newline - 1L)], collapse=""))
                        characters <- characters[-seq_len(newline)]
                        if (length(characters) == 0L) lines <- c(lines, "")
                    } else {
                        end <- max(1L, end - 1L)
                        spaces <- which(grepl("^[[:blank:]]$", characters[seq_len(end)]))
                        if (end < length(characters) && length(spaces) > 0L)
                            end <- tail(spaces, 1L)
                        lines <- c(lines, paste0(characters[seq_len(end)], collapse=""))
                        characters <- characters[-seq_len(end)]
                    }
                }
                paste(lines, collapse="\n")
            }, character(1)), features)
            lines <- strsplit(labels, "\n", fixed=TRUE)
            labelWidth <- max(0L, nchar(unlist(lines), type="width"))
            depth <- max(1L, lengths(lines))
            list(labels=labels,
                width=max(600L, 80L + 8L * labelWidth + 32L * length(unique(
                    if (is.null(dat$contrastIndex)) dat$contrast else dat$contrastIndex))),
                height=max(500L, 230L + length(features) * max(22L, 18L * depth + 6L)))
        },

        .sizeHeatmap = function(dat) {
            layout <- private$.heatmapLayout(dat)
            image <- self$results$heatmap
            previous <- image$size
            image$setSize(layout$width, layout$height)
            if (!identical(image$size, previous))
                image$.setPath(NULL)
        },

        .buildHeatmapPlot = function (dat = private$.heatmapData())
        {
            if (is.null(dat))
                return(NULL)
            # Older saved image states used display labels as the x coordinate.
            if (is.null(dat$contrastIndex))
                dat$contrastIndex <- match(dat$contrast, unique(dat$contrast))
            dat$feature <- factor(dat$feature, levels = unique(dat$feature))
            dat$contrast <- factor(dat$contrast, levels = unique(dat$contrast))
            dat$contrastIndex <- factor(dat$contrastIndex, levels = unique(dat$contrastIndex))
            featureLabels <- private$.heatmapLayout(dat)$labels
            firstRows <- !duplicated(dat$contrastIndex)
            contrastLabels <- stats::setNames(
                .misoUniqueShortLabels(as.character(dat$contrast[firstRows]), width = 24L),
                as.character(dat$contrastIndex[firstRows]))
            grid <- dat[, c("feature", "contrast", "contrastIndex", "missing"), drop = FALSE]
            selected <- dat[!dat$missing, , drop = FALSE]
            ggplot2::ggplot(grid, ggplot2::aes(x = contrastIndex, y = feature)) + ggplot2::geom_tile(fill = "#D9D9D9",
                colour = "white", linewidth = 0.35) + ggplot2::geom_tile(data = selected, ggplot2::aes(fill = contribution),
                colour = "white", linewidth = 0.35) + ggplot2::scale_fill_viridis_c(option = "C", direction = -1,
                na.value = "#D9D9D9", name = "Contribution (%)") + ggplot2::scale_x_discrete(labels = contrastLabels) +
                ggplot2::scale_y_discrete(labels = featureLabels) + ggplot2::labs(x = "Contrast", y = "Feature",
                caption = paste("Grey cells show features omitted by the display limits",
                    "and do not necessarily mean zero contribution.", sep = "\n")) +
                .misoPlotTheme() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1,
                vjust = 1), axis.text.y = ggplot2::element_text(lineheight = 0.9),
                plot.caption = ggplot2::element_text(hjust = 0))
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

        .simperGroup = function(group) {
            # vegan keys contrasts by joining group names with an underscore.
            # First-observed integer codes keep those keys unique and preserve
            # contrast order, including the order of permutation draws.
            match(group, unique(group))
        },

        .contrastLabel = function(pair) {
            miso_contrast_label(pair)
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
