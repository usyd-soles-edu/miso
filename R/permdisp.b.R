
# This file is a generated template, your changes will not be overwritten

permdispClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "permdispClass",
    inherit = permdispBase,
    private = list(
        .state = list(),

        .run = function() {
            private$.state <- list(
                warnings=character(),
                distances=NULL,
                fit=NULL,
                distanceDiagnostic=NULL,
                ordination=NULL,
                pValue=NA_real_,
                restriction=NULL,
                cl=NULL)
            private$.resetResults()

            hasVars <- length(self$options$vars) > 0L
            hasFactor <- private$.hasValue(self$options$factor)
            if (! hasVars && ! hasFactor) {
                private$.showGuidance(
                    paste(
                        "PERMDISP tests whether groups differ in multivariate spread.",
                        "1. Add one or more numeric Feature variables.",
                        "2. Add one categorical Grouping variable with at least two groups.",
                        "Results update automatically.",
                        sep="\n"),
                    title="Getting started")
                return()
            }
            if (! hasVars) {
                private$.showGuidance(paste(
                    "PERMDISP is waiting for Feature variables.",
                    "Add one or more numeric response columns, such as species abundances."))
                return()
            }
            if (! hasFactor) {
                private$.showGuidance(paste(
                    "PERMDISP is waiting for a Grouping variable.",
                    "Add one categorical variable containing at least two groups."))
                return()
            }

            private$.state$cl <- miso_parallel(self$options$useParallel)
            on.exit(miso_parallel_stop(private$.state$cl), add=TRUE)

            prep <- miso_prepare_resemblance(
                data=self$data,
                vars=self$options$vars,
                factor=self$options$factor,
                transform=self$options$transform,
                distance=self$options$distance,
                seed=self$options$seed,
                distBinary=self$options$distBinary)
            if (prep$error) {
                private$.showGuidance(prep$message)
                return()
            }

            miso_populate_summary(
                self$results,
                prep,
                self$options$transform,
                self$options$distance)
            private$.populatePurposes()
            private$.state$warnings <- c(
                private$.state$warnings,
                prep$warnings)
            private$.state$restriction <- private$.restrictionState()
            private$.state$warnings <- c(
                private$.state$warnings,
                private$.state$restriction$warning)

            outcome <- private$.runDispersion(prep)
            if (! isTRUE(outcome$success))
                return()

            private$.setInterpretation(prep, outcome$pairwiseShown)
            private$.populateSettings(prep, outcome$pairwiseShown)
            private$.setWarnings(private$.state$warnings)
            private$.showSuccessfulResults(outcome$pairwiseShown)
        },

        .resetResults = function() {
            self$results$guidance$setContent("")
            self$results$warnings$setContent("")
            miso_clear_table(self$results$summary)
            miso_clear_table(self$results$distances)
            miso_clear_table(self$results$anova)
            miso_clear_table(self$results$pairwise)
            miso_clear_table(self$results$ordinationScores)
            ordinationScores <- self$results$ordinationScores
            ordinationScores$.__enclos_env__$private$.rowNames <- character()
            self$results$anova$setNote(
                key="structuralCells",
                note="")
            self$results$pairwise$setNote(
                key="scope",
                note="")
            self$results$note$setContent("")
            self$results$plotDescription$setContent("")
            self$results$ordinationDescription$setContent("")
            miso_clear_table(self$results$settings)
            for (name in c(
                    "summaryPurpose", "anovaPurpose", "pairwisePurpose",
                    "distancesPurpose", "ordinationScoresPurpose",
                    "settingsPurpose"))
                self$results[[name]]$setContent("")

            for (name in c(
                    "guidance", "summary", "summaryPurpose", "warnings",
                    "distances", "distancesPurpose", "anova", "anovaPurpose",
                    "pairwise", "pairwisePurpose", "plot", "plotDescription",
                    "ordinationPlot", "ordinationDescription",
                    "ordinationScores", "ordinationScoresPurpose", "note",
                    "settings", "settingsPurpose"))
                self$results[[name]]$setVisible(FALSE)
        },

        .showGuidance = function(content, title="Action needed") {
            self$results$guidance$setTitle(title)
            self$results$guidance$setContent(miso_html_block(content))
            self$results$guidance$setVisible(TRUE)
        },

        .showSuccessfulResults = function(pairwiseShown=FALSE) {
            for (name in c(
                    "summary", "summaryPurpose", "distances",
                    "distancesPurpose", "anova", "anovaPurpose", "note",
                    "settings", "settingsPurpose"))
                self$results[[name]]$setVisible(TRUE)
            self$results$pairwise$setVisible(isTRUE(pairwiseShown))
            self$results$pairwisePurpose$setVisible(isTRUE(pairwiseShown))
            showDistance <- isTRUE(self$options$showDistancePlot) &&
                !is.null(private$.state$distanceDiagnostic)
            for (name in c("plot", "plotDescription"))
                self$results[[name]]$setVisible(showDistance)

            requestedOrdination <- isTRUE(self$options$showOrdinationPlot)
            ordinationAvailable <- requestedOrdination &&
                !is.null(private$.state$ordination) &&
                isTRUE(private$.state$ordination$available)
            ordinationTableAvailable <- requestedOrdination &&
                !is.null(private$.state$ordination) &&
                isTRUE(private$.state$ordination$tableAvailable)
            self$results$ordinationPlot$setVisible(ordinationAvailable)
            self$results$ordinationScores$setVisible(ordinationTableAvailable)
            self$results$ordinationScoresPurpose$setVisible(
                ordinationTableAvailable)
            self$results$ordinationDescription$setVisible(requestedOrdination)
        },

        .populatePurposes = function() {
            miso_populate_purposes(self$results, list(
                summaryPurpose=c(
                    "Data summary",
                    "Summarises included samples and features, including any exclusions."),
                anovaPurpose=c(
                    "Dispersion test",
                    "Tests whether groups differ in mean distance to their centres."),
                pairwisePurpose=c(
                    "Pairwise dispersion comparisons",
                    "Compares mean dispersion between each pair of groups."),
                distancesPurpose=c(
                    "Distance-to-centre summary",
                    "Summarises the centre, spread and range of each group's distances."),
                ordinationScoresPurpose=c(
                    "Ordination coordinates",
                    "Lists plotted ordination coordinates for identification or reuse."),
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

        .runDispersion = function(prep) {
            miso_set_seed(prep)
            fit <- tryCatch(
                vegan::betadisper(
                    prep$dist,
                    prep$group,
                    type=self$options$dispType,
                    bias.adjust=self$options$dispBias,
                    sqrt.dist=isTRUE(self$options$distSqrt),
                    add=if (identical(self$options$distAdd, "none"))
                        FALSE
                    else
                        self$options$distAdd),
                error=function(e) e)
            if (inherits(fit, "error")) {
                private$.showGuidance(paste(
                    "PERMDISP could not calculate distances to the group centres.",
                    fit$message))
                return(list(success=FALSE, pairwiseShown=FALSE))
            }

            private$.state$fit <- fit
            private$.state$distances <- data.frame(
                group=fit$group,
                distance=fit$distances,
                check.names=FALSE)
            private$.state$distanceDiagnostic <-
                .preparePermdispDistanceDiagnostic(
                    fit, centre=self$options$dispType)
            self$results$plot$setState(private$.state$distanceDiagnostic)
            if (is.null(private$.state$distanceDiagnostic)) {
                private$.showGuidance(paste(
                    "PERMDISP could not produce finite distances to group centres.",
                    "Check that each group contains usable, non-identical samples."))
                return(list(success=FALSE, pairwiseShown=FALSE))
            }
            private$.populateDistanceSummary(
                private$.state$distanceDiagnostic$summaries)
            private$.populateDistanceDescription()

            private$.state$ordination <- .preparePermdispOrdination(
                fit, rowIndex=prep$rowIndex)
            self$results$ordinationPlot$setState(private$.state$ordination)
            private$.populateOrdination()

            restriction <- private$.state$restriction$effectiveCode
            perm <- tryCatch(
                vegan::permutest(
                    fit,
                    permutations=miso_permutation(
                        self$options$permN,
                        restriction,
                        NULL),
                    parallel=private$.state$cl),
                error=function(e) e)
            if (inherits(perm, "error")) {
                private$.showGuidance(paste(
                    "PERMDISP could not run the permutation test.",
                    perm$message))
                return(list(success=FALSE, pairwiseShown=FALSE))
            }

            atab <- as.data.frame(perm$tab)
            rn <- rownames(atab)
            for (i in seq_len(nrow(atab))) {
                source <- trimws(rn[[i]])
                notApplicable <- source %in% c("Residual", "Residuals")
                values <- list(
                    source=source,
                    df=miso_num_or_na(atab[i, "Df"]),
                    sumsqs=miso_num_or_na(atab[i, "Sum Sq"]),
                    meansq=miso_num_or_na(atab[i, "Mean Sq"]),
                    f=if (notApplicable) "" else miso_num_or_na(atab[i, "F"]),
                    p=if (notApplicable) "" else miso_num_or_na(atab[i, "Pr(>F)"]))
                self$results$anova$addRow(
                    rowKey=as.character(i),
                    values=values)
                if (! notApplicable && is.na(private$.state$pValue))
                    private$.state$pValue <- miso_num_or_na(atab[i, "Pr(>F)"])
            }
            self$results$anova$setNote(
                key="structuralCells",
                note=paste(
                    "Blank F and Permutation p cells are not applicable",
                    "to the Residual row."))

            pairwiseShown <- FALSE
            if (isTRUE(self$options$dispPairwise) && nlevels(prep$group) >= 3L)
                pairwiseShown <- private$.runPairwise(fit, restriction)

            list(success=TRUE, pairwiseShown=pairwiseShown)
        },

        .runPairwise = function(fit, restriction) {
            pt <- tryCatch(
                vegan::permutest(
                    fit,
                    permutations=miso_permutation(
                        self$options$permN,
                        restriction,
                        NULL),
                    pairwise=TRUE,
                    parallel=private$.state$cl),
                error=function(e) e)
            if (inherits(pt, "error")) {
                private$.state$warnings <- c(
                    private$.state$warnings,
                    paste(
                        "Pairwise dispersion comparisons could not run:",
                        pt$message))
                return(FALSE)
            }
            if (is.null(pt$pairwise) || length(pt$pairwise$permuted) == 0L) {
                private$.state$warnings <- c(
                    private$.state$warnings,
                    "Pairwise dispersion comparisons returned no usable contrasts.")
                return(FALSE)
            }

            labels <- names(pt$pairwise$permuted)
            tstat <- as.numeric(pt$statistic[-1L])
            pperm <- as.numeric(pt$pairwise$permuted)
            padj <- if (identical(self$options$dispAdjust, "none"))
                pperm
            else
                stats::p.adjust(pperm, method=self$options$dispAdjust)
            failed <- character()
            shown <- 0L
            for (i in seq_along(pperm)) {
                label <- if (length(labels) >= i && nzchar(labels[[i]]))
                    labels[[i]]
                else
                    paste("Contrast", i)
                if (! is.finite(pperm[[i]]) ||
                        length(tstat) < i || ! is.finite(tstat[[i]])) {
                    failed <- c(failed, label)
                    next
                }
                shown <- shown + 1L
                self$results$pairwise$addRow(
                    rowKey=as.character(shown),
                    values=list(
                        contrast=gsub("-", " vs ", label, fixed=TRUE),
                        statistic=miso_num_or_na(tstat[[i]]),
                        p=miso_num_or_na(pperm[[i]]),
                        padj=miso_num_or_na(padj[[i]])))
            }
            if (length(failed) > 0L)
                private$.state$warnings <- c(
                    private$.state$warnings,
                    paste0(
                        "Pairwise dispersion comparisons were unavailable for: ",
                        paste(failed, collapse=", "),
                        "."))
            if (shown == 0L)
                return(FALSE)

            self$results$pairwise$setNote(
                key="scope",
                note=paste(
                    "These comparisons test differences in dispersion,",
                    "not differences in group location."))
            TRUE
        },

        .populateDistanceSummary = function(summaries) {
            for (i in seq_len(nrow(summaries))) {
                values <- summaries[i, , drop=FALSE]
                self$results$distances$addRow(
                    rowKey=as.character(i),
                    values=list(
                        group=values$group,
                        n=values$n,
                        centre=values$centre,
                        distance=miso_num_or_na(values$mean),
                        median=miso_num_or_na(values$median),
                        q1=miso_num_or_na(values$q1),
                        q3=miso_num_or_na(values$q3),
                        sd=miso_num_or_na(values$sd),
                        min=miso_num_or_na(values$min),
                        max=miso_num_or_na(values$max)))
            }
        },

        .populateDistanceDescription = function() {
            if (is.null(private$.state$distanceDiagnostic) ||
                    !isTRUE(self$options$showDistancePlot))
                return()
            self$results$plotDescription$setContent(miso_html_block(
                "Shows the distribution of distances to centre within each group.",
                ariaLabel="About distances to group centre",
                title="Distance-to-centre plot"))
        },

        .populateOrdination = function ()
        {
            ordination <- private$.state$ordination
            if (!isTRUE(self$options$showOrdinationPlot))
                return()
            if (!is.null(ordination) && isTRUE(ordination$tableAvailable)) {
                coordinates <- rbind(ordination$sites[, c("point", "pointType", "group", "plotKey", "axis1",
                    "axis2")], ordination$centres[, c("point", "pointType", "group", "plotKey", "axis1", "axis2")])
                for (i in seq_len(nrow(coordinates))) {
                    values <- coordinates[i, , drop = FALSE]
                    self$results$ordinationScores$addRow(rowKey = as.character(i), values = list(point = values$point,
                        pointType = values$pointType, group = values$group, plotKey = values$plotKey, axis1 = miso_num_or_na(values$axis1),
                        axis2 = miso_num_or_na(values$axis2)))
                }
            }
            if (is.null(ordination) || !isTRUE(ordination$available)) {
                reason <- if (!is.null(ordination$reason))
                    ordination$reason
                else "Two fitted ordination axes were unavailable."
                omitted <- if (!is.null(ordination) && !is.null(ordination$total) && ordination$total > 0L)
                    sprintf(paste("%d of %d fitted sites could not be plotted because", "finite coordinates were unavailable for a site or",
                        "its assigned group centre."), ordination$unplottable, ordination$total)
                else character()
                self$results$ordinationDescription$setContent(miso_html_block(
                    c(reason, omitted),
                    ariaLabel="About PERMDISP ordination",
                    title="PERMDISP ordination"))
                return()
            }
            mappingDisclosure <- if (length(ordination$groups) > 64L)
                paste("Neutral styling is used because more than 64 groups exceed", "the display-style limit; the coordinate table gives every",
                    "site and group centre identity.")
            else if (length(ordination$groups) > 12L)
                paste("Compact keys label group centres instead of using a wide", "legend; the coordinate table maps each key to its full group",
                    "name, and overlapping labels may be suppressed.")
            else character()
            finiteDisclosure <- if (ordination$unplottable > 0L)
                sprintf(paste("%d of %d fitted sites could not be plotted because", "finite coordinates were unavailable for a site or its",
                    "assigned group centre."), ordination$unplottable, ordination$total)
            else sprintf("All %d fitted sites had finite site and centre coordinates.", ordination$total)
            capDisclosure <- if (ordination$displayed == ordination$plotEligible)
                sprintf("All %d plot-eligible sites and connecting segments are shown.", ordination$plotEligible)
            else sprintf(paste("%d of %d plot-eligible sites and connecting segments are", "shown; %d are omitted from the image by the deterministic",
                "display cap but retained in the coordinate table."), ordination$displayed, ordination$plotEligible,
                ordination$plotEligible - ordination$displayed)
            self$results$ordinationDescription$setContent(miso_html_block(
                "Shows samples and the group centres used by PERMDISP.",
                ariaLabel="About PERMDISP ordination",
                title="PERMDISP ordination"))
        }
,

        .restrictionState = function() {
            current <- self$options$permRestriction
            legacy <- self$options$permScheme
            requested <- current
            effective <- current
            warning <- character()
            usedLegacy <- FALSE

            if (identical(current, "free") && ! identical(legacy, "free")) {
                usedLegacy <- TRUE
                requested <- legacy
                if (identical(legacy, "stratified")) {
                    effective <- "free"
                    warning <- paste(
                        "This saved analysis requested Stratified permutations",
                        "without a blocking variable. PERMDISP now uses Free",
                        "permutations, which is equivalent for that design.")
                }
                else if (identical(legacy, "series")) {
                    effective <- "series"
                    warning <- paste(
                        "This saved analysis uses Series permutations based on",
                        "the current data-row order. Reselect Series under",
                        "Reproducibility and computation to migrate the setting.")
                }
            }

            list(
                requested=private$.restrictionLabel(
                    requested,
                    legacy=usedLegacy),
                effective=private$.restrictionLabel(effective),
                effectiveCode=effective,
                warning=warning)
        },

        .setInterpretation = function(prep, pairwiseShown) {
            self$results$note$setContent(miso_html_block(paste(
                "PERMDISP tests multivariate spread, not group location.",
                "Compare the test with the distance plot."),
                title="How to read these results"))
        },

        .populateSettings = function(prep, pairwiseShown) {
            add <- function(setting, value) {
                key <- as.character(length(self$results$settings$rowKeys) + 1L)
                self$results$settings$addRow(
                    rowKey=key,
                    values=list(setting=setting, value=as.character(value)))
            }

            restriction <- private$.state$restriction
            add("Transformation", private$.transformLabel(self$options$transform))
            add("Dissimilarity", private$.distanceLabel(self$options$distance))
            add("Group centre", private$.centreLabel(self$options$dispType))
            add("Bias adjustment", private$.enabledLabel(self$options$dispBias))
            add("Binary dissimilarity", private$.enabledLabel(self$options$distBinary))
            add("Square-root distances", private$.enabledLabel(self$options$distSqrt))
            add("Additive constant", private$.additiveLabel(self$options$distAdd))
            add("Number of permutations", self$options$permN)
            add("Requested permutation restriction", restriction$requested)
            add("Effective permutation restriction", restriction$effective)
            if (identical(restriction$effectiveCode, "series"))
                add("Sequence order", "Current data-row order")
            add("Pairwise comparisons", private$.enabledLabel(self$options$dispPairwise))
            add(
                "P-value adjustment",
                if (pairwiseShown)
                    private$.adjustmentLabel(self$options$dispAdjust)
                else
                    "Not applied")
            add("Random seed", if (is.na(prep$seed)) "Random" else prep$seed)
            add(
                "Parallel processing requested",
                if (isTRUE(self$options$useParallel)) "Yes" else "No")
            add(
                "Effective execution",
                if (is.null(private$.state$cl)) "Serial" else "Parallel")
        },

        .hasValue = function(value) {
            ! is.null(value) && length(value) > 0L &&
                ! all(is.na(value)) && any(nzchar(as.character(value)))
        },

        .enabledLabel = function(value) {
            if (isTRUE(value)) "Enabled" else "Disabled"
        },

        .centreLabel = function(value) {
            switch(value,
                median="Median",
                centroid="Centroid",
                value)
        },

        .restrictionLabel = function(value, legacy=FALSE) {
            label <- switch(value,
                free="Free",
                stratified="Stratified",
                series="Series (rows in order)",
                value)
            if (isTRUE(legacy) && ! identical(value, "free"))
                paste(label, "(legacy)")
            else
                label
        },

        .adjustmentLabel = function(value) {
            switch(value,
                holm="Holm",
                bonferroni="Bonferroni",
                BH="Benjamini-Hochberg",
                BY="Benjamini-Yekutieli",
                none="None",
                value)
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

        .additiveLabel = function(value) {
            switch(value,
                none="None",
                cailliez="Cailliez",
                lingoes="Lingoes",
                value)
        },

        .plotDistances = function(image, ...) {
            plot <- .buildPermdispDistancePlot(image$state)
            if (is.null(plot))
                return()
            suppressWarnings(print(plot))
            invisible(TRUE)
        },

        .plotOrdination = function(image, ...) {
            plot <- .buildPermdispOrdinationPlot(image$state)
            if (is.null(plot))
                return()
            suppressWarnings(print(plot))
            invisible(TRUE)
        }
    )
)
