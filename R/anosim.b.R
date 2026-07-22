
# This file is a generated template, your changes will not be overwritten

anosimClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "anosimClass",
    inherit = anosimBase,
    private = list(
        .state = list(),

        .run = function() {
            private$.state <- list(
                warnings=character(),
                restriction=NULL,
                cl=NULL,
                globalR=NA_real_,
                globalP=NA_real_,
                effectivePermutations=NA_integer_,
                pairwisePermutations=integer(),
                rankPlotData=NULL)
            private$.resetResults()

            hasVars <- length(self$options$vars) > 0L
            hasFactor <- private$.hasValue(self$options$factor)
            if (! hasVars && ! hasFactor) {
                private$.showGuidance(
                    paste(
                        "ANOSIM compares ranked between-group and within-group dissimilarities.",
                        "1. Add one or more numeric Feature variables.",
                        "2. Add one categorical Grouping variable with at least two groups.",
                        "Results update automatically.",
                        sep="\n"),
                    title="Getting started")
                return()
            }
            if (! hasVars) {
                private$.showGuidance(paste(
                    "ANOSIM is waiting for Feature variables.",
                    "Add one or more numeric response columns, such as species abundances."))
                return()
            }
            if (! hasFactor) {
                private$.showGuidance(paste(
                    "ANOSIM is waiting for a Grouping variable.",
                    "Add one categorical variable containing at least two groups."))
                return()
            }

            prep <- tofu_prepare_resemblance(
                data=self$data,
                vars=self$options$vars,
                factor=self$options$factor,
                transform=self$options$transform,
                distance=self$options$distance,
                seed=self$options$seed,
                strata=self$options$strata,
                distBinary=self$options$distBinary)
            if (prep$error) {
                private$.showGuidance(prep$message)
                return()
            }

            private$.state$restriction <- private$.restrictionState(prep)
            if (! is.null(private$.state$restriction$error)) {
                private$.showGuidance(private$.state$restriction$error)
                return()
            }
            private$.state$warnings <- c(
                prep$warnings,
                private$.state$restriction$warnings)

            private$.state$cl <- tofu_parallel(self$options$useParallel)
            on.exit(tofu_parallel_stop(private$.state$cl), add=TRUE)
            if (isTRUE(self$options$useParallel) && is.null(private$.state$cl))
                private$.state$warnings <- c(
                    private$.state$warnings,
                    paste(
                        "Parallel processing was requested but unavailable;",
                        "the analysis ran serially."))

            tofu_populate_summary(
                self$results,
                prep,
                self$options$transform,
                self$options$distance)

            if (! private$.runGlobal(prep))
                return()

            pairwiseShown <- FALSE
            if (isTRUE(self$options$anosimPairwise) && nlevels(prep$group) >= 3L)
                pairwiseShown <- private$.runPairwise(prep)

            private$.setInterpretation(prep, pairwiseShown)
            private$.populateSettings(prep, pairwiseShown)
            private$.setWarnings(private$.state$warnings)
            private$.showSuccessfulResults(pairwiseShown)
        },

        .resetResults = function() {
            self$results$guidance$setContent("")
            self$results$warnings$setContent("")
            tofu_clear_table(self$results$summary)
            tofu_clear_table(self$results$global)
            tofu_clear_table(self$results$pairwise)
            tofu_clear_table(self$results$rankSummary)
            rankSummary <- self$results$rankSummary
            rankSummary$.__enclos_env__$private$.rowNames <- character()
            self$results$global$setNote(key="meaning", note="")
            self$results$pairwise$setNote(key="scope", note="")
            self$results$rankPlotDescription$setContent("")
            self$results$note$setContent("")
            tofu_clear_table(self$results$settings)

            for (name in c(
                    "guidance", "summary", "warnings", "global",
                    "pairwise", "rankPlot", "rankPlotDescription",
                    "rankSummary", "note", "settings"))
                self$results[[name]]$setVisible(FALSE)
        },

        .showGuidance = function(content, title="Action needed") {
            self$results$guidance$setTitle(title)
            self$results$guidance$setContent(tofu_html_block(content))
            self$results$guidance$setVisible(TRUE)
        },

        .showSuccessfulResults = function(pairwiseShown=FALSE) {
            for (name in c("summary", "global", "note", "settings"))
                self$results[[name]]$setVisible(TRUE)
            self$results$pairwise$setVisible(isTRUE(pairwiseShown))
            hasRank <- !is.null(private$.state$rankPlotData)
            showRank <- isTRUE(self$options$showRankPlot) && hasRank
            for (name in c("rankPlot", "rankPlotDescription"))
                self$results[[name]]$setVisible(showRank)
            self$results$rankSummary$setVisible(hasRank)
        },

        .setWarnings = function(warnings) {
            warnings <- unique(warnings[! is.na(warnings) & nzchar(warnings)])
            if (length(warnings) == 0L) {
                self$results$warnings$setContent("")
                self$results$warnings$setVisible(FALSE)
                return()
            }
            self$results$warnings$setContent(tofu_html_block(warnings))
            self$results$warnings$setVisible(TRUE)
        },

        .restrictionState = function(prep) {
            current <- self$options$permRestriction
            legacy <- self$options$permScheme
            hasBlock <- length(prep$strata) > 0L
            blockName <- if (hasBlock) prep$strata[[1L]] else NULL
            requestedCode <- current
            effectiveCode <- current
            usedLegacy <- FALSE
            warnings <- character()

            if (identical(current, "free") && ! identical(legacy, "free")) {
                usedLegacy <- TRUE
                requestedCode <- legacy
                if (identical(legacy, "stratified") && ! hasBlock) {
                    effectiveCode <- "free"
                    warnings <- c(
                        warnings,
                        paste(
                            "This saved analysis requested Stratified permutations",
                            "without a blocking variable. ANOSIM now uses Free",
                            "permutations, which is equivalent for that design."))
                }
                else {
                    effectiveCode <- legacy
                    warnings <- c(
                        warnings,
                        paste(
                            "This saved analysis uses a legacy permutation setting.",
                            "Reselect the effective restriction under Study design",
                            "and permutation restrictions to migrate it."))
                }
            }

            state <- list(
                error=NULL,
                warnings=warnings,
                requested=private$.restrictionLabel(
                    requestedCode,
                    legacy=usedLegacy),
                effective=private$.restrictionLabel(effectiveCode),
                effectiveCode=effectiveCode,
                block=if (hasBlock) blockName else "None",
                blockUsed=hasBlock && ! identical(effectiveCode, "free"),
                blockVector=NULL,
                legalPermutations=NA_real_,
                control=NULL)

            if (identical(effectiveCode, "stratified") && ! hasBlock) {
                state$error <- paste(
                    "Within-block permutations require a Blocking variable.",
                    "Add one, or select Free permutations.")
                return(state)
            }

            if (identical(effectiveCode, "free") && hasBlock)
                state$warnings <- c(
                    state$warnings,
                    sprintf(
                        "Blocking variable '%s' is assigned but not used with Free permutations.",
                        blockName))

            if (hasBlock)
                state$blockVector <- droplevels(
                    as.factor(prep$data[[blockName]]))

            if (state$blockUsed) {
                blockSizes <- table(state$blockVector)
                if (any(blockSizes == 1L))
                    state$warnings <- c(
                        state$warnings,
                        paste(
                            "One or more blocks contain a single sample and cannot",
                            "contribute restricted permutations."))

                groupLevelsByBlock <- tapply(
                    prep$group,
                    state$blockVector,
                    function(x) nlevels(droplevels(as.factor(x))))
                if (! any(groupLevelsByBlock >= 2L)) {
                    state$error <- paste(
                        "ANOSIM could not run: the Grouping variable does not vary within any block.",
                        "Use a different Blocking variable or select Free permutations.")
                    return(state)
                }
            }

            state$control <- tofu_permutation(
                self$options$anosimN,
                effectiveCode,
                if (state$blockUsed) state$blockVector else NULL)
            state$legalPermutations <- tryCatch(
                permute::numPerms(nrow(prep$data), control=state$control),
                error=function(e) NA_real_)
            if (is.finite(state$legalPermutations) &&
                    state$legalPermutations < 2) {
                state$error <- paste(
                    "ANOSIM could not run: fewer than two legal permutations are available.",
                    "Change the Blocking variable or permutation restriction.")
                return(state)
            }
            if (is.finite(state$legalPermutations) &&
                    state$legalPermutations < self$options$anosimN)
                state$warnings <- c(
                    state$warnings,
                    sprintf(
                        "Only %.0f unique permutations are available; %d were requested.",
                        state$legalPermutations,
                        self$options$anosimN))

            state
        },

        .runGlobal = function(prep) {
            tofu_set_seed(prep)
            fit <- tryCatch(
                withCallingHandlers(
                    vegan::anosim(
                        prep$dist,
                        prep$group,
                        permutations=private$.state$restriction$control,
                        parallel=private$.state$cl),
                    warning=function(w) {
                        private$.state$warnings <- c(
                            private$.state$warnings,
                            paste("ANOSIM warning:", conditionMessage(w)))
                        invokeRestart("muffleWarning")
                    }),
                error=function(e) e)
            if (inherits(fit, "error")) {
                private$.showGuidance(paste(
                    "ANOSIM could not calculate the global rank comparison.",
                    fit$message))
                return(FALSE)
            }

            r <- tofu_num_or_na(fit$statistic)
            p <- tofu_num_or_na(fit$signif)
            permutations <- suppressWarnings(as.integer(fit$permutations))
            if (! is.finite(r) || ! is.finite(p)) {
                private$.showGuidance(paste(
                    "ANOSIM could not produce a valid global result.",
                    "Check that each group contains enough non-identical samples."))
                return(FALSE)
            }

            private$.state$globalR <- r
            private$.state$globalP <- p
            private$.state$effectivePermutations <- if (
                length(permutations) > 0L && is.finite(permutations[[1L]]))
                    permutations[[1L]]
                else
                    as.integer(self$options$anosimN)

            private$.state$rankPlotData <- private$.prepareRankPlotData(fit)
            private$.populateRankDiagnostic()

            self$results$global$addRow(
                rowKey="r",
                values=list(
                    statistic="Global R",
                    value=r,
                    p=p,
                    permutations=private$.state$effectivePermutations))
            self$results$global$setNote(
                key="meaning",
                note=paste(
                    "R compares ranked between-group and within-group",
                    "dissimilarities. Permutation p uses the displayed",
                    "restriction in Analysis settings."))
            TRUE
        },

        .prepareRankPlotData = function(fit, maxRaw=600L) {
            ranks <- as.numeric(fit$dis.rank)
            classes <- as.character(fit$class.vec)
            classLevels <- levels(fit$class.vec)
            if (length(ranks) != length(classes) || length(ranks) == 0L)
                return(NULL)

            categoryLevels <- c(
                "Between",
                paste("Within", classLevels[classLevels != "Between"]))
            categories <- ifelse(
                classes == "Between", "Between", paste("Within", classes))
            all <- data.frame(
                rank=ranks,
                sourceClass=classes,
                category=factor(categories, levels=categoryLevels),
                stringsAsFactors=FALSE)

            finite <- which(is.finite(all$rank) & !is.na(all$category))
            counts <- vapply(categoryLevels, function(category) {
                sum(all$category[finite] == category)
            }, integer(1))
            targets <- pmin(counts, maxRaw %/% length(categoryLevels))
            while (sum(targets) < min(maxRaw, sum(counts))) {
                available <- which(targets < counts)
                add <- available[seq_len(min(
                    length(available),
                    min(maxRaw, sum(counts)) - sum(targets)))]
                targets[add] <- targets[add] + 1L
            }

            selected <- vector("list", length(categoryLevels))
            for (i in seq_along(categoryLevels)) {
                candidates <- finite[all$category[finite] == categoryLevels[[i]]]
                take <- targets[[i]]
                if (take > 0L) {
                    candidates <- candidates[order(
                        all$rank[candidates],
                        all$sourceClass[candidates],
                        method="radix")]
                    positions <- unique(as.integer(round(seq(
                        1, length(candidates), length.out=take))))
                    selected[[i]] <- candidates[positions]
                }
            }
            selected <- unlist(selected, use.names=FALSE)
            raw <- all[selected, , drop=FALSE]
            raw$categoryIndex <- match(
                as.character(raw$category), categoryLevels)
            raw$x <- raw$categoryIndex + .tofuJitter(
                nrow(raw), width=.16, seed=104729L)
            rownames(raw) <- NULL

            list(
                all=all,
                raw=raw,
                displayed=nrow(raw),
                total=length(finite))
        },

        .populateRankDiagnostic = function() {
            diagnostic <- private$.state$rankPlotData
            if (is.null(diagnostic))
                return()
            categories <- levels(diagnostic$all$category)
            for (i in seq_along(categories)) {
                category <- categories[[i]]
                values <- diagnostic$all$rank[
                    diagnostic$all$category == category &
                    is.finite(diagnostic$all$rank)]
                self$results$rankSummary$addRow(
                    rowKey=as.character(i),
                    values=list(
                        category=category,
                        pairs=length(values),
                        median=unname(stats::median(values)),
                        q1=unname(stats::quantile(
                            values, .25, names=FALSE)),
                        q3=unname(stats::quantile(
                            values, .75, names=FALSE))))
            }
            disclosure <- .tofuPlotDisclosure(
                diagnostic$displayed,
                diagnostic$total,
                noun="raw pair ranks")
            if (isTRUE(self$options$showRankPlot)) {
                groupCount <- length(categories) - 1L
                styleDisclosure <- if (groupCount > 64L)
                    sprintf(
                        paste(
                            "Group-specific styling was omitted because %d groups",
                            "exceed the 64-style display limit."),
                        groupCount)
                else
                    character()
                self$results$rankPlotDescription$setContent(tofu_html_block(c(
                    paste(
                        "Between ranks compare samples from different groups, while",
                        "each Within category compares samples from the named group."),
                    disclosure,
                    styleDisclosure,
                    paste(
                        "Pairwise dissimilarities are dependent, so this plot is a",
                        "diagnostic rather than additional inference; both location and",
                        "dispersion can affect ANOSIM R."))))
            }
        },

        .buildRankPlot = function() {
            diagnostic <- private$.state$rankPlotData
            if (is.null(diagnostic) || !isTRUE(self$options$showRankPlot))
                return(NULL)
            all <- diagnostic$all
            all <- all[is.finite(all$rank) & !is.na(all$category), , drop=FALSE]
            if (nrow(all) == 0L)
                return(NULL)
            categories <- levels(all$category)
            all$categoryIndex <- match(as.character(all$category), categories)
            labels <- .tofuUniqueShortLabels(categories, width=28L)
            withinCategories <- categories[categories != "Between"]
            if (length(withinCategories) <= 64L) {
                withinAesthetics <- .tofuGroupAesthetics(withinCategories)
                colours <- c(
                    Between="#222222",
                    withinAesthetics$colour[withinCategories])
                shapes <- c(
                    Between=1L,
                    withinAesthetics$shape[withinCategories])
            } else {
                colours <- stats::setNames(
                    c("#222222", rep("#777777", length(withinCategories))),
                    categories)
                shapes <- stats::setNames(
                    c(1L, rep(16L, length(withinCategories))),
                    categories)
            }

            ggplot2::ggplot(
                all,
                ggplot2::aes(
                    x=categoryIndex, y=rank, group=category,
                    fill=category)) +
                ggplot2::geom_boxplot(
                    width=.58, outlier.shape=NA, alpha=.28,
                    colour="#333333", linewidth=.55) +
                ggplot2::geom_point(
                    data=diagnostic$raw,
                    ggplot2::aes(
                        x=x, y=rank, colour=category, shape=category),
                    inherit.aes=FALSE, size=1.65, alpha=.62, stroke=.35) +
                ggplot2::scale_x_continuous(
                    breaks=seq_along(categories),
                    labels=unname(labels[categories]),
                    expand=ggplot2::expansion(mult=c(.06, .06)),
                    guide=ggplot2::guide_axis(n.dodge=2L)) +
                ggplot2::scale_fill_manual(
                    values=colours[categories], guide="none") +
                ggplot2::scale_colour_manual(
                    values=colours[categories], guide="none") +
                ggplot2::scale_shape_manual(
                    values=shapes[categories], guide="none") +
                ggplot2::labs(
                    x="Pair category",
                    y="Ranked dissimilarity") +
                .tofuPlotTheme(baseSize=10)
        },

        .plotRank = function(image, ...) {
            plot <- private$.buildRankPlot()
            if (is.null(plot))
                return()
            suppressWarnings(print(plot))
            invisible(TRUE)
        },

        .runPairwise = function(prep) {
            groups <- levels(prep$group)
            pairs <- utils::combn(groups, 2L, simplify=FALSE)
            distMat <- as.matrix(prep$dist)
            successful <- list()
            failed <- character()

            for (pair in pairs) {
                idx <- prep$group %in% pair
                subGroup <- droplevels(prep$group[idx])
                contrast <- paste(pair, collapse=" vs ")
                if (any(table(subGroup) < 2L)) {
                    failed <- c(failed, contrast)
                    next
                }

                subBlock <- NULL
                if (private$.state$restriction$blockUsed)
                    subBlock <- droplevels(
                        private$.state$restriction$blockVector[idx])
                control <- private$.pairwiseControl(
                    subGroup,
                    subBlock,
                    private$.state$restriction$effectiveCode)
                if (inherits(control, "error")) {
                    failed <- c(failed, contrast)
                    next
                }

                subDist <- stats::as.dist(distMat[idx, idx, drop=FALSE])
                tofu_set_seed(prep)
                fit <- tryCatch(
                    suppressWarnings(vegan::anosim(
                        subDist,
                        subGroup,
                        permutations=control,
                        parallel=private$.state$cl)),
                    error=function(e) e)
                if (inherits(fit, "error") ||
                        ! is.finite(tofu_num_or_na(fit$statistic)) ||
                        ! is.finite(tofu_num_or_na(fit$signif))) {
                    failed <- c(failed, contrast)
                    next
                }
                successful[[contrast]] <- list(
                    contrast=contrast,
                    r=tofu_num_or_na(fit$statistic),
                    p=tofu_num_or_na(fit$signif))
                private$.state$pairwisePermutations[[contrast]] <-
                    as.integer(fit$permutations)
            }

            if (length(failed) > 0L)
                private$.state$warnings <- c(
                    private$.state$warnings,
                    paste0(
                        "Pairwise ANOSIM was unavailable for: ",
                        paste(failed, collapse=", "),
                        ". Each group in a contrast needs at least two samples",
                        " and a usable permutation design."))
            if (length(successful) == 0L)
                return(FALSE)

            pvals <- vapply(successful, `[[`, numeric(1), "p")
            adjusted <- if (identical(self$options$anosimAdjust, "none"))
                pvals
            else
                stats::p.adjust(pvals, method=self$options$anosimAdjust)
            for (i in seq_along(successful)) {
                row <- successful[[i]]
                self$results$pairwise$addRow(
                    rowKey=as.character(i),
                    values=list(
                        contrast=row$contrast,
                        r=row$r,
                        p=row$p,
                        padj=adjusted[[i]]))
            }
            self$results$pairwise$setNote(
                key="scope",
                note=paste(
                    "Contrasts compare ranked group separation under the same",
                    "effective restriction as Global ANOSIM. Adjustment applies",
                    "only to the populated contrasts."))
            TRUE
        },

        .pairwiseControl = function(group, block, restriction) {
            if (! is.null(block)) {
                groupLevelsByBlock <- tapply(
                    group,
                    block,
                    function(x) nlevels(droplevels(as.factor(x))))
                if (! any(groupLevelsByBlock >= 2L))
                    return(simpleError(
                        "The contrast does not vary within any block."))
            }

            control <- tryCatch(
                tofu_permutation(
                    self$options$anosimN,
                    restriction,
                    block),
                error=function(e) e)
            if (inherits(control, "error"))
                return(control)
            legal <- tryCatch(
                permute::numPerms(length(group), control=control),
                error=function(e) NA_real_)
            if (is.finite(legal) && legal < 2L)
                return(simpleError(
                    "Fewer than two legal permutations are available."))
            control
        },

        .setInterpretation = function(prep, pairwiseShown) {
            r <- private$.state$globalR
            p <- private$.state$globalP
            observed <- if (r > .1)
                paste(
                    "The positive R indicates that observations from different",
                    "groups tend to be ranked as more dissimilar than observations",
                    "within groups.")
            else if (r < -.1)
                paste(
                    "The negative R indicates that within-group observations are,",
                    "on average, ranked as more dissimilar than observations from",
                    "different groups. The selected grouping may not describe the",
                    "dissimilarity pattern well, or unusual dispersion or structure",
                    "may need examination.")
            else
                paste(
                    "R is near zero, so between-group and within-group rank patterns",
                    "are similar.")
            evidence <- if (is.finite(p) && p < .05)
                paste(
                    "The permutation p value provides evidence against the null",
                    "pattern under the selected permutation design.")
            else
                paste(
                    "The permutation p value does not provide sufficient evidence",
                    "against the null pattern under the selected permutation design;",
                    "this is not proof that the groups are identical.")
            pairwiseText <- ""
            if (isTRUE(self$options$anosimPairwise) && nlevels(prep$group) == 2L)
                pairwiseText <- paste(
                    "With two groups, global ANOSIM is the only group contrast,",
                    "so a separate pairwise table is not shown.")
            else if (isTRUE(pairwiseShown))
                pairwiseText <- sprintf(
                    "Pairwise contrasts use %s adjustment and concern ranked group separation.",
                    private$.adjustmentLabel(self$options$anosimAdjust))

            paragraphs <- c(
                paste(
                    "ANOSIM compares ranked between-group and within-group",
                    "dissimilarities. R approaching 1 indicates stronger group",
                    "separation; R near 0 indicates similar rank patterns; negative R",
                    "means within-group observations are ranked as more dissimilar on",
                    "average than observations from different groups."),
                sprintf(
                    "Global R = %s; Permutation p = %s. %s %s",
                    format(r, digits=3),
                    format.pval(p, digits=3, eps=.001),
                    observed,
                    evidence),
                paste(
                    "The p value assesses the selected permutation design and does",
                    "not prove a biological mechanism. PERMANOVA is generally more",
                    "flexible for model-based testing, and PERMDISP can help examine",
                    "whether group dispersions differ."),
                pairwiseText)
            paragraphs <- paragraphs[nzchar(paragraphs)]
            self$results$note$setContent(tofu_html_block(paragraphs))
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
            add("Binary dissimilarity", private$.enabledLabel(self$options$distBinary))
            add("Grouping variable", prep$primary)
            add("Observed groups", nlevels(prep$group))
            add("Requested permutations", self$options$anosimN)
            add("Effective global permutations", private$.state$effectivePermutations)
            add("Requested permutation restriction", restriction$requested)
            add("Effective permutation restriction", restriction$effective)
            add("Blocking variable", restriction$block)
            add("Block used", if (restriction$blockUsed) "Yes" else "No")
            if (identical(restriction$effectiveCode, "series"))
                add(
                    "Sequence order",
                    if (restriction$blockUsed)
                        "Current data-row order within blocks"
                    else
                        "Current data-row order")
            add("Pairwise comparisons", private$.enabledLabel(self$options$anosimPairwise))
            add("Pairwise output", if (pairwiseShown) "Produced" else "Not produced")
            if (pairwiseShown)
                add(
                    "Effective pairwise permutations",
                    private$.pairwisePermutationLabel(
                        private$.state$pairwisePermutations))
            add(
                "P-value adjustment",
                if (pairwiseShown)
                    private$.adjustmentLabel(self$options$anosimAdjust)
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

        .htmlEscape = function(value) {
            value <- gsub("&", "&amp;", as.character(value), fixed=TRUE)
            value <- gsub("<", "&lt;", value, fixed=TRUE)
            value <- gsub(">", "&gt;", value, fixed=TRUE)
            value
        },

        .enabledLabel = function(value) {
            if (isTRUE(value)) "Enabled" else "Disabled"
        },

        .pairwisePermutationLabel = function(values) {
            values <- as.integer(values[is.finite(values)])
            if (length(values) == 0L)
                return("Unavailable")
            uniqueValues <- unique(values)
            if (length(uniqueValues) == 1L)
                return(sprintf(
                    "%d for each populated contrast",
                    uniqueValues[[1L]]))
            sprintf(
                "%d to %d across populated contrasts",
                min(values),
                max(values))
        },

        .restrictionLabel = function(value, legacy=FALSE) {
            label <- switch(value,
                free="Free",
                stratified="Within blocks",
                series="Series (rows in order)",
                value)
            if (isTRUE(legacy) && ! identical(value, "free")) {
                if (identical(value, "stratified"))
                    "Stratified (legacy)"
                else
                    paste(label, "(legacy)")
            }
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
        }
    )
)
