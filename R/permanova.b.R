# This file is a generated template, your changes will not be overwritten

permanovaClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "permanovaClass",
    inherit = permanovaBase,
    private = list(
        .state = list(),
        .lastStructuralKey = NULL,

        .run = function() {
            structuralKey <- miso_options_signature(
                self$options,
                excluded=c("showCompanionPcoa", "pcoaDisplayFactor",
                    "pcoaCentroids", "pcoaSpiders"))
            if (!is.null(private$.lastStructuralKey) &&
                    identical(private$.lastStructuralKey, structuralKey)) {
                private$.refreshDisplayOnly()
                return()
            }
            private$.lastStructuralKey <- structuralKey
            private$.state <- list(
                warnings=character(), cl=NULL, permutation=NULL,
                companion=list(
                    requested=isTRUE(self$options$showCompanionPcoa),
                    prep=NULL, model=NULL, fit=NULL,
                    plotData=NULL, displayFactor=NULL))
            private$.clearResults()

            if (length(self$options$vars) == 0) {
                private$.showGuidance(
                    paste(
                        "To run PERMANOVA:",
                        "1. Add one or more numeric Feature variables.",
                        "2. Add one categorical Grouping variable with at least two groups.",
                        "Results update automatically.",
                        sep="\n"),
                    title="Getting started")
                return()
            }

            if (miso_is_missing_var(self$options$factor)) {
                private$.showGuidance(
                    paste(
                        "PERMANOVA is waiting for a Grouping variable.",
                        "Add one categorical variable containing at least two groups."),
                    title="Action needed")
                return()
            }

            if (identical(self$options$permScheme, "stratified") &&
                    miso_is_missing_var(self$options$strata)) {
                private$.showGuidance(
                    paste(
                        "Within-block permutations require a Blocking variable.",
                        "Add one, or select Free permutations."),
                    title="Action needed")
                return()
            }

            prep <- miso_prepare_resemblance(
                data=self$data,
                vars=self$options$vars,
                factor=self$options$factor,
                transform=self$options$transform,
                distance=self$options$distance,
                seed=self$options$seed,
                extraVars=self$options$permFactors,
                strata=self$options$strata,
                covariates=self$options$covariates,
                distBinary=self$options$distBinary)
            if (isTRUE(prep$error)) {
                private$.showGuidance(
                    paste0("PERMANOVA could not run: ", prep$message),
                    title="Action needed")
                return()
            }

            permutation <- private$.permutationState(prep)
            private$.state$permutation <- permutation
            if (! is.null(permutation$error)) {
                private$.showGuidance(permutation$error, title="Action needed")
                return()
            }

            private$.state$warnings <- c(
                private$.state$warnings,
                prep$warnings,
                permutation$warnings)
            private$.state$cl <- miso_parallel(self$options$useParallel)
            on.exit(miso_parallel_stop(private$.state$cl), add=TRUE)

            main <- private$.runPermanova(prep)
            if (! main$success) {
                private$.clearResults()
                correction <- if (grepl("saturated", main$error, fixed=TRUE))
                    "The selected model has no residual degrees of freedom. Remove a model term or use more samples."
                else
                    "Check the selected variables and model settings."
                private$.showGuidance(
                    paste0(
                        "PERMANOVA could not run: ", correction,
                        "\nTechnical detail: ", main$error),
                    title="Action needed")
                return()
            }

            miso_populate_summary(
                self$results,
                prep,
                self$options$transform,
                self$options$distance)
            private$.populatePurposes()

            private$.runCompanionPcoa(prep, main$model)

            pairwiseShown <- FALSE
            if (isTRUE(self$options$permPairwise)) {
                if (isTRUE(self$options$permInteractions)) {
                    private$.state$warnings <- c(
                        private$.state$warnings,
                        paste(
                            "Pairwise comparisons are unavailable while interactions are included.",
                            "Clear Pairwise comparisons or clear Interactions."))
                }
                else if (identical(self$options$permBy, "omnibus")) {
                    private$.state$warnings <- c(
                        private$.state$warnings,
                        paste(
                            "Pairwise comparisons require Sequential terms or Marginal terms",
                            "so the Grouping variable can be tested separately.",
                            "Change Test type or clear Pairwise comparisons."))
                }
                else {
                    pairwise <- private$.runPairwisePermanova(prep)
                    pairwiseShown <- pairwise$rows > 0L
                    private$.state$warnings <- c(
                        private$.state$warnings,
                        pairwise$warnings)
                }
            }

            private$.setInterpretation(prep, main$model, pairwiseShown)
            private$.populateSettings(prep, permutation, pairwiseShown)
            private$.showSuccessfulResults(pairwiseShown)
            private$.setWarnings(private$.state$warnings)
        },

        .refreshDisplayOnly = function() {
            requested <- isTRUE(self$options$showCompanionPcoa)
            companion <- private$.state$companion
            if (requested && !is.null(companion$prep) &&
                    !is.null(companion$model)) {
                miso_clear_table(self$results$companionPcoaSites)
                miso_clear_table(self$results$companionPcoaCentroids)
                private$.runCompanionPcoa(companion$prep, companion$model)
                return()
            }
            miso_clear_table(self$results$companionPcoaSites)
            miso_clear_table(self$results$companionPcoaCentroids)
            self$results$companionPcoa$setVisible(FALSE)
            self$results$companionPcoaDescription$setVisible(FALSE)
            self$results$companionPcoaSites$setVisible(FALSE)
            self$results$companionPcoaSitesPurpose$setVisible(FALSE)
            self$results$companionPcoaCentroids$setVisible(FALSE)
            self$results$companionPcoaCentroidsPurpose$setVisible(FALSE)
        },

        .clearResults = function() {
            self$results$guidance$setContent("")
            self$results$warnings$setContent("")
            self$results$companionPcoaDescription$setContent("")
            for (name in c(
                    "summaryPurpose", "tablePurpose",
                    "companionPcoaSitesPurpose",
                    "companionPcoaCentroidsPurpose", "pairwisePurpose",
                    "settingsPurpose"))
                self$results[[name]]$setContent("")
            miso_clear_fixed_table(self$results$summary, 6L)
            miso_clear_table(self$results$table)
            miso_clear_table(self$results$companionPcoaSites)
            miso_clear_table(self$results$companionPcoaCentroids)
            miso_clear_table(self$results$pairwise)
            self$results$pairwise$setNote(
                key="scope",
                note="")
            self$results$note$setContent("")
            miso_clear_table(self$results$settings)

            for (name in c(
                    "guidance", "warnings", "summary", "summaryPurpose",
                    "table", "tablePurpose",
                    "companionPcoa", "companionPcoaDescription",
                    "companionPcoaSites", "companionPcoaSitesPurpose",
                    "companionPcoaCentroids",
                    "companionPcoaCentroidsPurpose",
                    "pairwise", "pairwisePurpose", "note", "settings",
                    "settingsPurpose"))
                self$results[[name]]$setVisible(FALSE)
            for (name in c("summaryPurpose", "summary", "tablePurpose", "table", "settingsPurpose", "settings"))
                self$results[[name]]$setVisible(TRUE)
        },

        .showGuidance = function(content, title="Action needed") {
            self$results$guidance$setTitle(title)
            self$results$guidance$setContent(miso_html_block(content))
            self$results$guidance$setVisible(TRUE)
        },

        .showSuccessfulResults = function(pairwiseShown=FALSE) {
            self$results$guidance$setVisible(FALSE)
            for (name in c(
                    "summary", "summaryPurpose", "table", "tablePurpose",
                    "note", "settings", "settingsPurpose"))
                self$results[[name]]$setVisible(TRUE)
            self$results$pairwise$setVisible(isTRUE(pairwiseShown))
            self$results$pairwisePurpose$setVisible(isTRUE(pairwiseShown))
        },

        .populatePurposes = function() {
            miso_populate_purposes(self$results, list(
                summaryPurpose=c(
                    "Data summary",
                    "Summarises included samples and features, including any exclusions."),
                tablePurpose=c(
                    "PERMANOVA table",
                    "Tests compositional associations and reports each term's explained variation (R\u00B2)."),
                companionPcoaSitesPurpose=c(
                    "Companion PCoA site coordinates",
                    "Lists plotted sample coordinates for identification or reuse."),
                companionPcoaCentroidsPurpose=c(
                    "Companion PCoA group centroids",
                    "Lists the plotted mean position of each group."),
                pairwisePurpose=c(
                    "Pairwise PERMANOVA",
                    "Compares pairs of groups and reports adjusted p-values when requested."),
                settingsPurpose=c(
                    "Analysis settings",
                    "Lists the options used for this analysis.")))
        },

        .setWarnings = function(warnings) {
            warnings <- unique(warnings[nzchar(warnings)])
            if (length(warnings) == 0L) {
                self$results$warnings$setContent("")
                self$results$warnings$setVisible(FALSE)
                return()
            }
            self$results$warnings$setContent(miso_warning_block(warnings))
            self$results$warnings$setVisible(TRUE)
        },

        .runCompanionPcoa = function(prep, model) {
            private$.state$companion$prep <- prep
            private$.state$companion$model <- model
            if (!isTRUE(self$options$showCompanionPcoa))
                return()

            selection <- private$.companionDisplayFactor(prep, model)
            if (!isTRUE(selection$valid)) {
                private$.showCompanionInstruction(selection$message)
                return()
            }

            fit <- tryCatch(
                .misoPcoa(
                    prep$dist,
                    sqrtDist=self$options$distSqrt,
                    correction=self$options$distAdd,
                    groups=selection$groups),
                error=function(e) list(
                    error=TRUE,
                    message=paste0("PCoA could not be fitted: ",
                        conditionMessage(e))))
            private$.state$companion$displayFactor <- selection$name
            private$.state$companion$fit <- fit
            if (isTRUE(fit$error)) {
                private$.showCompanionInstruction(paste(
                    fit$message,
                    "The PERMANOVA and pairwise results remain available."))
                return()
            }

            plotData <- .misoPcoaPlotData(
                fit,
                showCentroids=self$options$pcoaCentroids,
                showSpiders=self$options$pcoaSpiders)
            private$.state$companion$plotData <- plotData
            self$results$companionPcoa$setState(plotData)
            private$.populateCompanionSites(prep, fit)
            if (isTRUE(self$options$pcoaCentroids) ||
                    isTRUE(self$options$pcoaSpiders))
                private$.populateCompanionCentroids(fit)
            private$.populateCompanionDescription(
                prep, model, selection$name, fit, plotData,
                selection$notice)

            self$results$companionPcoaDescription$setVisible(TRUE)
            self$results$companionPcoaSites$setVisible(TRUE)
            self$results$companionPcoaSitesPurpose$setVisible(TRUE)
            self$results$companionPcoaCentroids$setVisible(
                (isTRUE(self$options$pcoaCentroids) ||
                    isTRUE(self$options$pcoaSpiders)) &&
                !is.null(fit$centroids))
            self$results$companionPcoaCentroidsPurpose$setVisible(
                self$results$companionPcoaCentroids$visible)
            self$results$companionPcoa$setVisible(
                !is.null(plotData) && isTRUE(plotData$available))
        },

        .companionDisplayFactor = function(prep, model) {
            eligible <- names(model$factorColumns)
            multifactor <- length(eligible) > 1L
            requested <- miso_clean_vars(self$options$pcoaDisplayFactor)
            notice <- character()
            if (!multifactor) {
                selected <- eligible[[1L]]
                if (length(requested) == 1L &&
                        !identical(requested[[1L]], selected)) {
                    stale <- requested[[1L]]
                    if (stale %in% model$droppedFactorNames) {
                        notice <- sprintf(
                            paste(
                                "Additional factor '%s' was not retained in",
                                "the fitted PERMANOVA model after data",
                                "filtering; the companion PCoA automatically",
                                "displays '%s'."),
                            stale, selected)
                    } else {
                        notice <- sprintf(
                            paste(
                                "The selected variable '%s' is not a",
                                "categorical factor retained in the fitted",
                                "PERMANOVA model; the companion PCoA",
                                "automatically displays '%s'."),
                            stale, selected)
                    }
                }
            } else {
                if (length(requested) != 1L ||
                        !requested[[1L]] %in% eligible) {
                    dropped <- if (length(requested) == 1L &&
                            requested[[1L]] %in% model$droppedFactorNames) {
                        sprintf(
                            paste(
                                "Additional factor '%s' was not retained after",
                                "data filtering."),
                            requested[[1L]])
                    } else {
                        character()
                    }
                    return(list(
                        valid=FALSE,
                        message=paste(
                            dropped,
                            "Choose one categorical model factor to display:",
                            paste(eligible, collapse=", "),
                            "Covariates, blocking variables, interactions, and",
                            "variables outside the fitted model cannot be used.")))
                }
                selected <- requested[[1L]]
            }

            modelColumn <- unname(model$factorColumns[[selected]])
            if (is.null(selected) || length(modelColumn) != 1L ||
                    !modelColumn %in% names(model$data)) {
                return(list(
                    valid=FALSE,
                    message="The model factor selected for the companion PCoA is unavailable."))
            }
            groups <- droplevels(as.factor(model$data[[modelColumn]]))
            if (length(groups) != attr(prep$dist, "Size") || anyNA(groups)) {
                return(list(
                    valid=FALSE,
                    message=paste(
                        "The selected display factor does not align with the",
                        "sites retained for PERMANOVA.")))
            }
            if (nlevels(groups) < 2L) {
                return(list(
                    valid=FALSE,
                    message=paste0(
                        "The selected display factor '", selected,
                        "' has fewer than two groups after data filtering.")))
            }
            list(valid=TRUE, name=selected, groups=groups,
                automatic=!multifactor, notice=notice)
        },

        .showCompanionInstruction = function(message) {
            self$results$companionPcoaDescription$setContent(
                miso_html_block(c(
                    paste(
                        "Shows a two-dimensional representation of the",
                        "dissimilarities used by PERMANOVA; nearby points",
                        "generally represent more similar samples."),
                    message),
                    ariaLabel="About PERMANOVA companion PCoA",
                    title="PERMANOVA companion PCoA"))
            self$results$companionPcoaDescription$setVisible(TRUE)
        },

        .addCompanionRow = function(table, values) {
            key <- as.character(length(table$rowKeys) + 1L)
            table$addRow(rowKey=key, values=values)
        },

        .populateCompanionSites = function(prep, fit) {
            axis1 <- if (ncol(fit$points) >= 1L) {
                fit$points[, 1L]
            } else {
                rep(NA_real_, nrow(fit$points))
            }
            axis2 <- if (ncol(fit$points) >= 2L) {
                fit$points[, 2L]
            } else {
                rep(NA_real_, nrow(fit$points))
            }
            groups <- as.character(fit$groups)
            for (i in seq_len(nrow(fit$points))) {
                private$.addCompanionRow(
                    self$results$companionPcoaSites,
                    list(
                        site=fit$siteNames[[i]],
                        sourceRow=as.integer(prep$rowIndex[[i]]),
                        group=groups[[i]],
                        PCoA1=miso_num_or_na(axis1[[i]]),
                        PCoA2=miso_num_or_na(axis2[[i]])))
            }
        },

        .populateCompanionCentroids = function(fit) {
            if (is.null(fit$centroids))
                return()
            axis2 <- if (ncol(fit$centroids) >= 2L) {
                fit$centroids[, 2L]
            } else {
                rep(NA_real_, nrow(fit$centroids))
            }
            for (i in seq_len(nrow(fit$centroids))) {
                group <- rownames(fit$centroids)[[i]]
                private$.addCompanionRow(
                    self$results$companionPcoaCentroids,
                    list(
                        group=group,
                        n=as.integer(fit$groupSizes[[group]]),
                        PCoA1=miso_num_or_na(fit$centroids[i, 1L]),
                        PCoA2=miso_num_or_na(axis2[[i]])))
            }
        },

        .populateCompanionDescription = function(
                prep, model, displayFactor, fit, plotData,
                selectionNotice=character()) {
            available <- !is.null(plotData) && isTRUE(plotData$available)
            self$results$companionPcoaDescription$setContent(
                miso_html_block(
                    if (available)
                        "Visualises sample resemblance and group positions alongside the test."
                    else
                        paste(
                            "A two-dimensional companion plot is unavailable.",
                            "Coordinate tables retain the fitted result."),
                    ariaLabel="About PERMANOVA companion PCoA",
                    title="PERMANOVA companion PCoA"))
            modelIsComplex <- length(model$extraNames) > 0L ||
                length(model$covariateNames) > 0L ||
                isTRUE(self$options$permInteractions)
            if (modelIsComplex)
                private$.state$warnings <- c(
                    private$.state$warnings,
                    paste(
                        "The companion PCoA displays one grouping variable and",
                        "does not represent adjusted effects from the complete model."))
            if (length(selectionNotice) > 0L)
                private$.state$warnings <- c(
                    private$.state$warnings, selectionNotice)
            if (available && isTRUE(plotData$neutral))
                private$.state$warnings <- c(
                    private$.state$warnings,
                    paste(
                        "Neutral site styling is used because more than 64 groups",
                        "are present; centroids and spiders are omitted from the image."))
        },

        .plotCompanionPcoa = function(image, ...) {
            plot <- .buildPcoaPlot(image$state)
            if (is.null(plot))
                return()
            suppressWarnings(print(plot))
            invisible(TRUE)
        },

        .permutationState = function(prep) {
            scheme <- self$options$permScheme
            hasBlock <- length(prep$strata) > 0L
            blockName <- if (hasBlock) prep$strata[[1L]] else NULL
            labels <- c(
                free="Free",
                stratified="Within blocks",
                series="Series")
            state <- list(
                error=NULL,
                warnings=character(),
                requested=unname(labels[[scheme]]),
                effective=unname(labels[[scheme]]),
                block=if (hasBlock) blockName else "None",
                blockUsed=hasBlock && ! identical(scheme, "free"),
                legalPermutations=NA_real_)

            if (identical(scheme, "stratified") && ! hasBlock) {
                state$error <- paste(
                    "Within-block permutations require a Blocking variable.",
                    "Add one, or select Free permutations.")
                return(state)
            }

            if (identical(scheme, "free") && hasBlock) {
                state$warnings <- c(
                    state$warnings,
                    sprintf(
                        "Blocking variable '%s' is assigned but not used with Free permutations.",
                        blockName))
                return(state)
            }

            block <- NULL
            if (hasBlock)
                block <- droplevels(as.factor(prep$data[[blockName]]))

            if (state$blockUsed) {
                blockSizes <- table(block)
                if (any(blockSizes == 1L))
                    state$warnings <- c(
                        state$warnings,
                        "One or more blocks contain a single sample and cannot contribute within-block permutations.")

                groupLevelsByBlock <- tapply(
                    prep$group,
                    block,
                    function(x) nlevels(droplevels(as.factor(x))))
                if (! any(groupLevelsByBlock >= 2L)) {
                    state$error <- paste(
                        "PERMANOVA could not run: the Grouping variable does not vary within any block.",
                        "Use a different Blocking variable or select Free permutations.")
                    return(state)
                }
            }

            control <- miso_permutation(
                self$options$permN,
                scheme,
                block)
            legal <- tryCatch(
                permute::numPerms(nrow(prep$data), control=control),
                error=function(e) NA_real_)
            state$legalPermutations <- legal
            if (is.finite(legal) && legal < 2) {
                state$error <- paste(
                    "PERMANOVA could not run: fewer than two legal permutations are available.",
                    "Change the Blocking variable or permutation restrictions.")
                return(state)
            }
            if (is.finite(legal) && legal < self$options$permN)
                state$warnings <- c(
                    state$warnings,
                    sprintf(
                        "Only %.0f unique permutations are available; %d were requested.",
                        legal,
                        self$options$permN))

            state
        },

        .runPermanova = function(prep) {
            miso_set_seed(prep)
            model <- private$.makeModelData(prep)
            result <- tryCatch(
                private$.adonisModel(prep, model),
                error=function(e) e)
            if (inherits(result, "error"))
                return(list(success=FALSE, error=result$message))

            tab <- as.data.frame(result)
            rn <- rownames(tab)
            for (i in seq_len(nrow(tab))) {
                notApplicable <- rn[i] %in% c("Residual", "Total")
                rowKey <- as.character(i)
                values <- list(
                    source=miso_display_term(rn[i], prep),
                    df=miso_num_or_na(tab[i, "Df"]),
                    sumsqs=miso_num_or_na(tab[i, "SumOfSqs"]),
                    r2=miso_num_or_na(tab[i, "R2"]),
                    f=if (notApplicable) "" else miso_num_or_na(tab[i, "F"]),
                    p=if (notApplicable) "" else miso_num_or_na(tab[i, "Pr(>F)"]))
                self$results$table$addRow(rowKey=rowKey, values=values)
            }

            list(
                success=TRUE,
                result=result,
                model=model)
        },

        .adonisModel = function(prep, model=NULL) {
            if (is.null(model))
                model <- private$.makeModelData(prep)
            by <- self$options$permBy
            if (identical(by, "omnibus"))
                by <- NULL

            formula <- stats::as.formula(paste(".dist ~", model$terms))
            mm <- stats::model.matrix(
                stats::delete.response(stats::terms(formula)),
                data=model$data)
            if (nrow(model$data) - qr(mm)$rank <= 0)
                stop("PERMANOVA model is saturated (no residual degrees of freedom).")

            .dist <- prep$dist
            vegan::adonis2(
                formula,
                data=model$data,
                permutations=miso_permutation(
                    self$options$permN,
                    self$options$permScheme,
                    model$strata),
                by=by,
                parallel=private$.state$cl,
                sqrt.dist=isTRUE(self$options$distSqrt),
                add=if (identical(self$options$distAdd, "none"))
                    FALSE
                else
                    self$options$distAdd)
        },

        .makeModelData = function(prep) {
            data <- data.frame(.f1=droplevels(prep$group), check.names=FALSE)
            terms <- ".f1"
            extraNames <- character()
            factorColumns <- stats::setNames(".f1", prep$primary)
            droppedFactorNames <- character()

            if (length(prep$extra) > 0) {
                for (i in seq_along(prep$extra)) {
                    f <- droplevels(as.factor(prep$data[[prep$extra[[i]]]]))
                    if (nlevels(f) < 2) {
                        droppedFactorNames <- c(
                            droppedFactorNames, prep$extra[[i]])
                        next
                    }
                    nm <- paste0(".f", i + 1)
                    data[[nm]] <- f
                    extraNames <- c(extraNames, nm)
                    factorColumns[[prep$extra[[i]]]] <- nm
                }
                if (length(extraNames) > 0) {
                    if (self$options$permInteractions)
                        terms <- paste(
                            c(".f1", paste0(".f1:", extraNames), extraNames),
                            collapse=" + ")
                    else
                        terms <- paste(c(".f1", extraNames), collapse=" + ")
                }
            }

            strata <- NULL
            if (length(prep$strata) > 0)
                strata <- droplevels(as.factor(prep$data[[prep$strata[[1]]]]))

            covNames <- character()
            if (! is.null(prep$covariates) && ncol(prep$covariates) > 0) {
                for (i in seq_len(ncol(prep$covariates))) {
                    nm <- paste0(".c", i)
                    data[[nm]] <- as.numeric(prep$covariates[[i]])
                    covNames <- c(covNames, nm)
                }
                terms <- paste(c(terms, covNames), collapse=" + ")
            }

            list(
                terms=terms,
                data=data,
                strata=strata,
                extraNames=extraNames,
                covariateNames=covNames,
                factorColumns=factorColumns,
                droppedFactorNames=droppedFactorNames)
        },

        .runPairwisePermanova = function(prep) {
            groups <- levels(prep$group)
            pairs <- utils::combn(groups, 2, simplify=FALSE)
            rows <- list()
            warnings <- character()
            distMat <- as.matrix(prep$dist)

            for (pair in pairs) {
                idx <- prep$group %in% pair
                subPrep <- prep
                subPrep$dist <- stats::as.dist(distMat[idx, idx, drop=FALSE])
                subPrep$group <- droplevels(prep$group[idx])
                subPrep$data <- prep$data[idx, , drop=FALSE]
                if (! is.null(prep$covariates))
                    subPrep$covariates <- prep$covariates[idx, , drop=FALSE]
                contrast <- paste(pair, collapse=" vs ")

                miso_set_seed(subPrep)
                result <- tryCatch(
                    private$.adonisModel(subPrep),
                    error=function(e) e)
                if (inherits(result, "error")) {
                    warnings <- c(
                        warnings,
                        sprintf(
                            "Pairwise comparison %s could not be calculated: %s",
                            contrast,
                            result$message))
                    next
                }

                tab <- as.data.frame(result)
                f <- miso_num_or_na(tab[1, "F"])
                p <- miso_num_or_na(tab[1, "Pr(>F)"])
                if (! is.finite(f) || ! is.finite(p)) {
                    warnings <- c(
                        warnings,
                        sprintf(
                            "Pairwise comparison %s did not produce a finite test statistic and p-value.",
                            contrast))
                    next
                }
                rows[[length(rows) + 1L]] <- list(
                    contrast=contrast,
                    f=f,
                    p=p)
            }

            if (length(rows) == 0L) {
                warnings <- c(
                    warnings,
                    "No pairwise comparison could be calculated for the selected model.")
                return(list(rows=0L, warnings=warnings))
            }

            pvals <- vapply(rows, `[[`, numeric(1), "p")
            method <- self$options$permAdjust
            adjusted <- if (identical(method, "none"))
                pvals
            else
                stats::p.adjust(pvals, method=method)
            for (i in seq_along(rows)) {
                rows[[i]]$padj <- adjusted[[i]]
                self$results$pairwise$addRow(
                    rowKey=as.character(i),
                    values=rows[[i]])
            }

            retained <- c(prep$extra, prep$covariateNames)
            retainedText <- if (length(retained) == 0L)
                "none"
            else
                paste(retained, collapse=", ")
            self$results$pairwise$setNote(
                key="scope",
                note=sprintf(
                    paste(
                        "Comparisons are between levels of Grouping variable '%s'.",
                        "Test type: %s. Retained adjustment terms: %s.",
                        "Permutation restrictions: %s. P-value adjustment: %s."),
                    prep$primary,
                    private$.testTypeLabel(self$options$permBy),
                    retainedText,
                    private$.state$permutation$effective,
                    private$.adjustmentLabel(self$options$permAdjust)))

            list(rows=length(rows), warnings=warnings)
        },

        .setInterpretation = function(prep, model, pairwiseShown) {
            self$results$note$setContent(miso_html_block(paste(
                "Pseudo-F compares among-group and within-group variation.",
                    "R\u00B2 shows explained variation, and permutation p tests the null model."),
                title="How to read these results"))
        },

        .populateSettings = function(prep, permutation, pairwiseShown) {
            add <- function(setting, value) {
                key <- as.character(length(self$results$settings$rowKeys) + 1L)
                self$results$settings$addRow(
                    rowKey=key,
                    values=list(setting=setting, value=as.character(value)))
            }

            add("Transformation", private$.transformLabel(self$options$transform))
            add("Dissimilarity", private$.distanceLabel(self$options$distance))
            add("Binary dissimilarity", private$.enabledLabel(self$options$distBinary))
            add("Square-root distances", private$.enabledLabel(self$options$distSqrt))
            add("Additive constant", private$.additiveLabel(self$options$distAdd))
            add("Number of permutations", self$options$permN)
            add("Requested permutation restrictions", permutation$requested)
            add("Effective permutation restrictions", permutation$effective)
            if (identical(self$options$permScheme, "series"))
                add("Sequence order", "Current data-row order")
            add("Blocking variable", permutation$block)
            add("Block used", if (permutation$blockUsed) "Yes" else "No")
            add("Test type", private$.testTypeLabel(self$options$permBy))
            add("Additional factors", private$.listLabel(prep$extra))
            add("Continuous covariates", private$.listLabel(prep$covariateNames))
            add("Interactions", private$.enabledLabel(self$options$permInteractions))
            add("Pairwise comparisons", private$.enabledLabel(self$options$permPairwise))
            add(
                "P-value adjustment",
                if (pairwiseShown)
                    private$.adjustmentLabel(self$options$permAdjust)
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

        .enabledLabel = function(value) {
            if (isTRUE(value)) "Enabled" else "Disabled"
        },

        .listLabel = function(value) {
            value <- miso_clean_vars(value)
            if (length(value) == 0L) "None" else paste(value, collapse=", ")
        },

        .testTypeLabel = function(value) {
            unname(c(
                omnibus="Omnibus",
                terms="Sequential terms",
                margin="Marginal terms")[[value]])
        },

        .adjustmentLabel = function(value) {
            unname(c(
                holm="Holm",
                bonferroni="Bonferroni",
                BH="Benjamini-Hochberg",
                BY="Benjamini-Yekutieli",
                none="None")[[value]])
        },

        .transformLabel = function(value) {
            labels <- c(
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
                rclr="Reverse CLR")
            unname(labels[[value]])
        },

        .distanceLabel = function(value) {
            labels <- c(
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
                mahalanobis="Mahalanobis")
            unname(labels[[value]])
        },

        .additiveLabel = function(value) {
            unname(c(
                none="None",
                cailliez="Cailliez",
                lingoes="Lingoes")[[value]])
        }
    )
)
