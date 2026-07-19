# This file is a generated template, your changes will not be overwritten

simperClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "simperClass",
    inherit = simperBase,
    private = list(
        .state = list(),

        .run = function() {
            private$.state <- list(
                warnings=character(),
                plotData=NULL,
                contrastLabels=character(),
                requestedPermutations=NA_integer_,
                effectivePermutations=NA_integer_)
            private$.resetResults()

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

            prep <- tofu_prepare_resemblance(
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

            private$.populateSummary(prep, length(descriptive$contrastRows))
            private$.populateDescriptive(descriptive)

            assessmentShown <- FALSE
            if (isTRUE(self$options$simperAssess))
                assessmentShown <- private$.runAssessment(prep, descriptive$displayRows)

            private$.setTableNotes(prep)
            private$.setPlotDescription(descriptive$displayRows)
            private$.setInterpretation(prep, assessmentShown)
            private$.populateSettings(prep, assessmentShown)
            private$.setWarnings(private$.state$warnings)
            private$.showSuccessfulResults(assessmentShown)
        },

        .resetResults = function() {
            self$results$guidance$setContent("")
            self$results$warnings$setContent("")
            tofu_clear_table(self$results$summary)
            tofu_clear_table(self$results$contrasts)
            tofu_clear_table(self$results$contributions)
            tofu_clear_table(self$results$variability)
            tofu_clear_table(self$results$means)
            tofu_clear_table(self$results$table)
            tofu_clear_table(self$results$assessment)
            self$results$contrasts$setNote(key="meaning", note="")
            self$results$contributions$setNote(key="meaning", note="")
            self$results$variability$setNote(key="meaning", note="")
            self$results$means$setNote(key="meaning", note="")
            self$results$table$setNote(key="meaning", note="")
            self$results$assessment$setNote(key="scope", note="")
            self$results$plotDescription$setContent("")
            self$results$note$setContent("")
            tofu_clear_table(self$results$settings)
            self$results$plot$setTitle(
                "SIMPER contribution percentages by group contrast")
            self$results$plot$setSize(580, 450)

            for (name in c(
                    "guidance", "summary", "warnings", "contrasts",
                    "contributions", "variability", "means", "table", "plot",
                    "plotDescription", "assessment", "note", "settings"))
                self$results[[name]]$setVisible(FALSE)
        },

        .showGuidance = function(content, title="Action needed") {
            content <- private$.htmlEscape(content)
            content <- gsub("\n", "<br>", content, fixed=TRUE)
            self$results$guidance$setTitle(title)
            self$results$guidance$setContent(paste0(
                '<div style="margin: 0; max-width: 100%; white-space: normal; overflow-wrap: anywhere;">',
                content,
                '</div>'))
            self$results$guidance$setVisible(TRUE)
        },

        .showSuccessfulResults = function(assessmentShown=FALSE) {
            for (name in c(
                    "summary", "contrasts", "contributions", "plot",
                    "plotDescription", "note", "settings"))
                self$results[[name]]$setVisible(TRUE)
            self$results$variability$setVisible(isTRUE(self$options$simperDetails))
            self$results$means$setVisible(isTRUE(self$options$simperDetails))
            self$results$table$setVisible(FALSE)
            self$results$assessment$setVisible(isTRUE(assessmentShown))
        },

        .setWarnings = function(warnings) {
            warnings <- unique(warnings[! is.na(warnings) & nzchar(warnings)])
            if (length(warnings) == 0L) {
                self$results$warnings$setContent("")
                self$results$warnings$setVisible(FALSE)
                return()
            }
            self$results$warnings$setContent(paste(warnings, collapse="\n"))
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
                shown <- tab[seq_len(displayN), , drop=FALSE]

                contrastRows[[length(contrastRows) + 1L]] <- list(
                    contrast=label,
                    nFirst=sum(as.character(prep$group) == pair[[1L]]),
                    nSecond=sum(as.character(prep$group) == pair[[2L]]),
                    overall=tofu_num_or_na(fit[[index]]$overall))

                for (row in seq_len(nrow(shown))) {
                    displayRows[[length(displayRows) + 1L]] <- list(
                        contrastIndex=index,
                        contrast=label,
                        feature=shown$feature[[row]],
                        average=tofu_num_or_na(shown$average[[row]]),
                        sd=tofu_num_or_na(shown$sd[[row]]),
                        ratio=tofu_num_or_na(shown$ratio[[row]]),
                        meanFirst=tofu_num_or_na(shown$ava[[row]]),
                        meanSecond=tofu_num_or_na(shown$avb[[row]]),
                        contribution=100 * shown$contribution[[row]],
                        cumulative=100 * shown$cumulative[[row]])
                }
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
                displayRows=displayRows)
        },

        .populateSummary = function(prep, contrastCount) {
            rows <- list(
                c("Analysed samples", prep$rowsUsed),
                c("Retained features", prep$varsUsed),
                c("Grouping variable", prep$primary),
                c("Observed groups", length(unique(as.character(prep$group)))),
                c("Contrasts", contrastCount))
            for (index in seq_along(rows))
                self$results$summary$addRow(
                    rowKey=as.character(index),
                    values=list(item=rows[[index]][[1L]], value=as.character(rows[[index]][[2L]])))
        },

        .populateDescriptive = function(descriptive) {
            for (index in seq_along(descriptive$contrastRows))
                self$results$contrasts$addRow(
                    rowKey=as.character(index),
                    values=descriptive$contrastRows[[index]])

            for (index in seq_along(descriptive$displayRows)) {
                values <- descriptive$displayRows[[index]]
                values$contrastIndex <- NULL
                self$results$table$addRow(
                    rowKey=as.character(index),
                    values=values)
                self$results$contributions$addRow(
                    rowKey=as.character(index),
                    values=values[c(
                        "contrast", "feature", "contribution", "cumulative")])
                self$results$variability$addRow(
                    rowKey=as.character(index),
                    values=values[c(
                        "contrast", "feature", "average", "sd", "ratio")])
                self$results$means$addRow(
                    rowKey=as.character(index),
                    values=values[c(
                        "contrast", "feature", "meanFirst", "meanSecond")])
            }

            private$.state$plotData <- do.call(
                rbind,
                lapply(descriptive$displayRows, as.data.frame, stringsAsFactors=FALSE))
            private$.state$contrastLabels <- unique(private$.state$plotData$contrast)
            height <- max(450, 220 * length(private$.state$contrastLabels))
            self$results$plot$setSize(580, height)
        },

        .runAssessment = function(prep, displayRows) {
            private$.state$requestedPermutations <- as.integer(self$options$simperN)
            tofu_set_seed(prep)
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
                p <- tofu_num_or_na(result[values$feature, "p"])
                padj <- tofu_num_or_na(result[values$feature, "padj"])
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

        .setPlotDescription = function(displayRows) {
            labels <- unique(vapply(displayRows, `[[`, character(1), "contrast"))
            describedContrasts <- utils::head(labels, 10L)
            omittedContrasts <- length(labels) - length(describedContrasts)
            labelDetails <- character()
            for (contrast in describedContrasts) {
                rows <- displayRows[vapply(
                    displayRows, `[[`, character(1), "contrast") == contrast]
                describedRows <- utils::head(rows, 10L)
                entries <- vapply(
                    describedRows,
                    function(row) paste0(
                        private$.htmlEscape(row$feature), ": ",
                        sprintf("%.1f%%", row$contribution)),
                    character(1))
                omitted <- length(rows) - length(describedRows)
                suffix <- if (omitted > 0L)
                    sprintf("; %s more shown in the table", omitted)
                else
                    ""
                labelDetails <- c(
                    labelDetails,
                    paste0(
                        '<p style="margin: 0 0 0.4em 0;"><strong>',
                        private$.htmlEscape(contrast), ':</strong> ',
                        paste(entries, collapse="; "), suffix, '.</p>'))
            }
            content <- paste0(
                '<div style="margin: 0; max-width: 100%; white-space: normal; overflow-wrap: anywhere;">',
                '<p style="margin: 0 0 0.65em 0;">',
                'First contrasts described below: ',
                private$.htmlEscape(paste(describedContrasts, collapse=", ")),
                if (omittedContrasts > 0L)
                    paste0('; plus ', omittedContrasts,
                           ' further contrasts shown in the plot and listed in the results tables.')
                else
                    '.',
                ' ',
                'The plot uses the same rows as Descriptive feature contributions. ',
                'Features stop when the cumulative contribution reaches ',
                private$.htmlEscape(as.character(self$options$simperCum)),
                '% or the Top features cap of ',
                private$.htmlEscape(as.character(self$options$simperTop)),
                ' is reached, whichever occurs first; the threshold-crossing feature is included.',
                '</p>',
                '<p style="margin: 0 0 0.4em 0;"><strong>Full labels and percentages for the contrasts described below.</strong></p>',
                paste(labelDetails, collapse=""),
                '</div>')
            self$results$plotDescription$setContent(content)
        },

        .setInterpretation = function(prep, assessmentShown) {
            paragraphs <- c(
                paste(
                    "SIMPER decomposes each contrast's average Bray-Curtis dissimilarity into feature contributions.",
                    "Large average or percentage contributions identify features worth examining; they do not prove a cause or an overall group difference."),
                paste(
                    "High contributions can reflect high abundance or within-group variability.",
                    "Average divided by SD describes consistency and is not a significance test.",
                    "The first- and second-group means show descriptive direction on the transformed scale."),
                paste(
                    "Use PERMANOVA or ANOSIM for the overall group-comparison context.",
                    sprintf(
                        "The table and plot include features until the cumulative contribution reaches %s%% or %s features are shown, whichever occurs first.",
                        self$options$simperCum,
                        self$options$simperTop)))
            if (isTRUE(assessmentShown))
                paragraphs <- c(
                    paragraphs,
                    paste(
                        "The exploratory permutation p-value is the proportion of random group-label assignments whose average contribution is at least as large as observed.",
                        "It does not test contribution percentage, establish causation, show which group mean is higher, or replace an overall group test.",
                        "Multiple-testing adjustment was performed separately within each contrast."))
            html <- paste0(
                '<div style="margin: 0; max-width: 100%; white-space: normal; overflow-wrap: anywhere;">',
                paste0('<p style="margin: 0 0 0.65em 0;">',
                       private$.htmlEscape(paragraphs), '</p>', collapse=""),
                '</div>')
            self$results$note$setContent(html)
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

        .boundedPlotLabel = function(value, width=18L, maxLines=2L) {
            value <- gsub("[[:space:]]+", " ", trimws(as.character(value)))
            if (! nzchar(value))
                return("")
            width <- max(2L, as.integer(width))
            maxLines <- max(1L, as.integer(maxLines))
            remaining <- value
            lines <- character()
            while (nzchar(remaining) && length(lines) < maxLines) {
                if (nchar(remaining) <= width) {
                    lines <- c(lines, remaining)
                    remaining <- ""
                    next
                }
                candidate <- substr(remaining, 1L, width)
                spaces <- gregexpr("[[:space:]]", candidate)[[1L]]
                spaces <- spaces[spaces > 1L]
                if (length(spaces) > 0L) {
                    splitAt <- max(spaces)
                    lines <- c(lines, trimws(substr(remaining, 1L, splitAt - 1L)))
                    remaining <- trimws(substr(
                        remaining, splitAt + 1L, nchar(remaining)))
                } else {
                    lines <- c(lines, candidate)
                    remaining <- substr(remaining, width + 1L, nchar(remaining))
                }
            }
            if (nzchar(remaining)) {
                last <- lines[[length(lines)]]
                last <- substr(last, 1L, min(nchar(last), width - 1L))
                lines[[length(lines)]] <- paste0(last, "\u2026")
            }
            paste(lines, collapse="\n")
        },

        .plotContributions = function(image, ...) {
            dat <- private$.state$plotData
            if (is.null(dat) || nrow(dat) == 0L)
                return()

            contrasts <- unique(dat$contrast)
            op <- graphics::par(
                mfrow=c(length(contrasts), 1L),
                mar=c(4.5, 10, 3.2, 1.5),
                oma=c(0, 0, 0, 0))
            on.exit(graphics::par(op), add=TRUE)
            for (contrast in contrasts) {
                rows <- dat[dat$contrast == contrast, , drop=FALSE]
                rows <- rows[rev(seq_len(nrow(rows))), , drop=FALSE]
                graphics::barplot(
                    height=rows$contribution,
                    names.arg=vapply(
                        rows$feature,
                        private$.boundedPlotLabel,
                        character(1),
                        width=18L,
                        maxLines=2L),
                    horiz=TRUE,
                    las=1,
                    col="#277da1",
                    border="#1b566f",
                    xlab="Contribution (%)",
                    main=private$.boundedPlotLabel(
                        contrast, width=28L, maxLines=2L))
            }
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
