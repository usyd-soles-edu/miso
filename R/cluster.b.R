
# This file is a generated template, your changes will not be overwritten

clusterClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "clusterClass",
    inherit = clusterBase,
    private = list(
        .state = list(),

        .run = function() {
            private$.state <- list(
                prep=NULL,
                fit=NULL,
                labels=character(),
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

            prep <- tofu_prepare_resemblance(
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
            private$.state <- list(
                prep=prep,
                fit=fit,
                labels=labelState$labels,
                warnings=unique(c(
                    prep$warnings,
                    labelState$warnings,
                    if (isTRUE(self$options$showLabels) &&
                            any(nchar(labelState$labels) > 24L))
                        paste(
                            "Long sample labels are shortened with an ellipsis",
                            "in the dendrogram; the source data are unchanged.")
                    else
                        character(),
                    if (isTRUE(self$options$showLabels) && prep$rowsUsed > 60L)
                        paste(
                            "Sample labels may be crowded with more than 60 samples.",
                            "Turn off Show sample labels for a clearer overview.")
                    else
                        character())))

            private$.populateSummary(prep)
            private$.populateSettings(prep, labelState$source)
            private$.setWarnings(private$.state$warnings)
            self$results$interpretation$setContent(tofu_html_block(c(
                "Samples joined at lower branch heights are more similar under the selected transformation and dissimilarity index.",
                "Branch order can rotate without changing the clusters; use ANOSIM or PERMANOVA to test predefined groups.")))
            private$.showSuccessfulResults()
        },

        .resetResults = function() {
            self$results$guidance$setContent("")
            tofu_clear_table(self$results$summary)
            self$results$warnings$setContent("")
            self$results$interpretation$setContent("")
            tofu_clear_table(self$results$settings)

            for (name in c(
                    "guidance", "summary", "warnings", "dendrogram",
                    "interpretation", "settings"))
                self$results[[name]]$setVisible(FALSE)
        },

        .showGuidance = function(content, title="Action needed") {
            self$results$guidance$setTitle(title)
            self$results$guidance$setContent(tofu_html_block(content))
            self$results$guidance$setVisible(TRUE)
        },

        .showSuccessfulResults = function() {
            for (name in c(
                    "summary", "dendrogram", "interpretation", "settings"))
                self$results[[name]]$setVisible(TRUE)
        },

        .setWarnings = function(warnings) {
            warnings <- unique(warnings[! is.na(warnings) & nzchar(warnings)])
            if (length(warnings) == 0L)
                return()
            self$results$warnings$setContent(tofu_html_block(warnings))
            self$results$warnings$setVisible(TRUE)
        },

        .sampleLabels = function(prep) {
            rowLabels <- as.character(prep$rowIndex)
            selected <- self$options$labels
            if (tofu_is_missing_var(selected))
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
                    paste(
                        "Duplicate sample labels were distinguished using data row numbers."))
            }

            list(labels=values, source=selected, warnings=warnings)
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
                "Sample labels shown",
                if (isTRUE(self$options$showLabels)) "Yes" else "No")
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

        .boundedPlotLabel = function(value, width=24L) {
            value <- gsub("[[:space:]]+", " ", trimws(as.character(value)))
            if (nchar(value) <= width)
                return(value)
            paste0(substr(value, 1L, width - 1L), "\u2026")
        },

        .plotDendrogram = function(image, ...) {
            fit <- private$.state$fit
            if (is.null(fit))
                return()

            plotted <- fit
            if (isTRUE(self$options$showLabels))
                plotted$labels <- vapply(
                    private$.state$labels,
                    private$.boundedPlotLabel,
                    character(1))
            else
                plotted$labels <- rep("", length(private$.state$labels))

            sampleCount <- length(plotted$order)
            labelCex <- max(0.42, min(0.78, 18 / max(sampleCount, 1L)))
            bottomMargin <- if (isTRUE(self$options$showLabels)) 7.5 else 3.5
            op <- graphics::par(mar=c(bottomMargin, 4.5, 2.5, 1.5))
            on.exit(graphics::par(op), add=TRUE)
            graphics::plot(
                plotted,
                hang=-1,
                labels=plotted$labels,
                cex=labelCex,
                main="",
                xlab="Samples",
                sub="",
                ylab=paste(
                    private$.distanceLabel(self$options$distance),
                    "dissimilarity"))
            TRUE
        }

    )
)
