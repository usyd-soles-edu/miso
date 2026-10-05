# This file is a generated template, your changes will not be overwritten

permanovaClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "permanovaClass",
    inherit = permanovaBase,
    private = list(
        .state = list(),
        .lastStructuralKey = NULL,

        .init = function() {
            if (is.null(private$.lastStructuralKey))
                private$.showEmptyTables()
        },

        .showEmptyTables = function() {
            miso_show_empty_tables(self$results, c(
                    table=TRUE, pairwise=isTRUE(self$options$permPairwise),
                    companionPcoaSites=isTRUE(self$options$showCompanionPcoa),
                    companionPcoaCentroids=isTRUE(self$options$showCompanionPcoa) &&
                        (isTRUE(self$options$pcoaCentroids) || isTRUE(self$options$pcoaSpiders))))
        },

        .preparePermanova = function() {
            if (length(self$options$vars) == 0) {
                if (!miso_is_missing_var(self$options$factor))
                    private$.showGuidance("Add one or more numeric Feature variables.")
                return(NULL)
            }

            if (miso_is_missing_var(self$options$factor)) {
                private$.showGuidance(
                    paste(
                        "PERMANOVA is waiting for a Grouping variable.",
                        "Add one categorical variable containing at least two groups."),
                    title="Action needed")
                return(NULL)
            }

            prep <- miso_prepare_resemblance(
                data=self$data,
                vars=self$options$vars,
                factor=self$options$factor,
                transform=self$options$transform,
                distance=self$options$distance,
                seed=miso_effective_seed(
                    self$options$useFixedSeed, self$options$seed),
                extraVars=self$options$permFactors,
                strata=self$options$strata,
                strataActive=!identical(self$options$permScheme, "free"),
                covariates=self$options$covariates,
                distBinary=self$options$distBinary)
            if (isTRUE(prep$error)) {
                private$.showGuidance(
                    paste0("PERMANOVA could not run: ", prep$message),
                    title="Action needed")
                return(NULL)
            }

            permutation <- private$.permutationState(prep)
            private$.state$permutation <- permutation
            if (! is.null(permutation$error)) {
                private$.showGuidance(permutation$error, title="Action needed")
                return(NULL)
            }

            private$.state$warnings <- c(
                private$.state$warnings,
                prep$warnings,
                permutation$warnings)
            private$.state$cl <- miso_parallel(self$options$useParallel)
            prep
        },

        .fitPermanova = function(prep) {
            miso_set_seed(prep)
            model <- private$.makeModelData(prep)
            result <- tryCatch(
                private$.adonisModel(prep, model),
                error=function(e) e)
            if (inherits(result, "error"))
                return(list(success=FALSE, error=result$message))
            list(success=TRUE, result=result, model=model)
        },

        .assemblePermanovaResults = function(prep, main) {
            tab <- as.data.frame(main$result)
            rn <- rownames(tab)
            termRows <- lapply(seq_len(nrow(tab)), function(i) {
                notApplicable <- rn[[i]] %in% c("Residual", "Total")
                list(
                    key=as.character(i),
                    values=list(
                        source=miso_display_term(rn[[i]], prep),
                        df=miso_num_or_na(tab[i, "Df"]),
                        sumsqs=miso_num_or_na(tab[i, "SumOfSqs"]),
                        r2=miso_num_or_na(tab[i, "R2"]),
                        f=if (notApplicable) "" else miso_num_or_na(tab[i, "F"]),
                        p=if (notApplicable) "" else miso_num_or_na(tab[i, "Pr(>F)"])))
            })
            miso_reconcile_table_rows(self$results$table, termRows)

            private$.state$mainPermutationN <- private$.permutationCount(main$result)
            self$results$table$setNote(
                key="method",
                note=trimws(paste(
                    miso_method_note(
                        private$.transformLabel(self$options$transform),
                        private$.distanceLabel(self$options$distance),
                        self$options$distBinary, self$options$distSqrt,
                        private$.additiveLabel(self$options$distAdd)),
                    sprintf("%s with %d permutations.",
                        switch(self$options$permBy, omnibus="Omnibus test",
                            terms="Sequential tests", margin="Marginal tests"),
                        private$.state$mainPermutationN),
                    private$.permutationRestrictionNote())),
                init=FALSE)

            private$.runCompanionPcoa(prep, main$model)

            pairwiseShown <- FALSE
            if (isTRUE(self$options$permPairwise)) {
                if (isTRUE(self$options$permInteractions)) {
                    eligibility <- private$.conditionalPairwiseEligibility(
                        prep, main$model)
                    if (isTRUE(eligibility$eligible)) {
                        pairwise <- private$.runConditionalPairwisePermanova(
                            prep, main)
                        pairwiseShown <- pairwise$rows > 0L
                        private$.state$warnings <- c(
                            private$.state$warnings,
                            pairwise$warnings)
                    }
                    else {
                        private$.state$warnings <- c(
                            private$.state$warnings,
                            eligibility$message)
                    }
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
            if (! pairwiseShown)
                miso_clear_table(self$results$pairwise)
            private$.showSuccessfulResults(pairwiseShown)
            private$.setWarnings(c(
                private$.state$warnings, private$.state$companion$warnings))
        },

        .run = function() {
            on.exit(miso_finish_empty_tables(self$results), add=TRUE)
            effectiveSeed <- miso_effective_seed(
                self$options$useFixedSeed, self$options$seed)
            structuralKey <- miso_options_signature(
                self$options,
                excluded=c("showCompanionPcoa", "pcoaDisplayFactor",
                    "pcoaCentroids", "pcoaSpiders", "seed", "useFixedSeed"),
                data=self$data,
                extra=list(effectiveSeed=effectiveSeed))
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
                    prep=NULL, model=NULL, fit=NULL, warnings=character(),
                    plotData=NULL, displayFactor=NULL))
            private$.clearResults()

            prep <- tryCatch(private$.preparePermanova(), error=function(e) e)
            if (inherits(prep, "error")) {
                private$.discardKeyedRows()
                stop(prep)
            }
            if (is.null(prep)) {
                private$.discardKeyedRows()
                return()
            }
            on.exit(miso_parallel_stop(private$.state$cl), add=TRUE)

            prep <- miso_record_seed(prep, self$results$seedState)
            main <- private$.fitPermanova(prep)
            if (! main$success) {
                private$.discardKeyedRows()
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
            private$.assemblePermanovaResults(prep, main)
            self$results$table$setNote(key="seed",
                note=sprintf("%s seed: %d.",
                    if (identical(prep$seedSource, "fixed")) "Fixed" else "Random",
                    prep$actualSeed),
                init=FALSE)
        },

        .refreshDisplayOnly = function() {
            if (is.null(private$.state$companion$prep)) {
                private$.showEmptyTables()
                return()
            }
            requested <- isTRUE(self$options$showCompanionPcoa)
            companion <- private$.state$companion
            if (requested && !is.null(companion$prep) &&
                    !is.null(companion$model)) {
                miso_clear_table(self$results$companionPcoaSites)
                miso_clear_table(self$results$companionPcoaCentroids)
                private$.runCompanionPcoa(companion$prep, companion$model)
                # Display-only reruns bypass the normal .run() tail. Publish
                # selection notices here rather than leaving them in state.
                private$.setWarnings(c(
                    private$.state$warnings, private$.state$companion$warnings))
                return()
            }
            miso_clear_table(self$results$companionPcoaSites)
            miso_clear_table(self$results$companionPcoaCentroids)
            self$results$companionPcoa$setVisible(FALSE)
            self$results$companionPcoaDescription$setVisible(FALSE)
            self$results$companionPcoaSites$setVisible(FALSE)
            self$results$companionPcoaCentroids$setVisible(FALSE)
            private$.state$companion$warnings <- character()
            private$.setWarnings(private$.state$warnings)
        },

        .clearResults = function() {
            self$results$table$setNote(key="seed", note="", init=FALSE)
            # A new effective seed can refit without a raw-option clearWith hit.
            self$results$companionPcoa$.setPath(NULL)
            self$results$guidance$setContent("")
            self$results$warnings$setContent("")
            self$results$companionPcoaDescription$setContent("")
            miso_clear_table_values(self$results$table)
            miso_clear_table(self$results$companionPcoaSites)
            miso_clear_table(self$results$companionPcoaCentroids)
            miso_clear_table_values(self$results$pairwise)
            self$results$pairwise$getColumn("condition")$setVisible(FALSE)
            self$results$pairwise$setNote(
                key="scope",
                note="",
                init=FALSE)

            for (name in c(
                    "guidance", "warnings",
                    "table",
                    "companionPcoa", "companionPcoaDescription",
                    "companionPcoaSites",
                    "companionPcoaCentroids",
                    "pairwise"
                    ))
                self$results[[name]]$setVisible(FALSE)
            private$.showEmptyTables()
        },

        # Destructive row removal for keyed result tables on rerun paths that
        # do not repopulate them: guidance, rejections, and failed fits must
        # not leave stale or blank rows behind.
        .discardKeyedRows = function() {
            miso_clear_table(self$results$table)
            miso_clear_table(self$results$pairwise)
        },

        .showGuidance = function(content, title="Action needed") {
            self$results$guidance$setTitle(title)
            self$results$guidance$setContent(miso_html_block(content))
            self$results$guidance$setVisible(TRUE)
        },

        .showSuccessfulResults = function(pairwiseShown=FALSE) {
            self$results$guidance$setVisible(FALSE)
            for (name in c(
                    "table"
                    ))
                self$results[[name]]$setVisible(TRUE)
            self$results$pairwise$setVisible(isTRUE(pairwiseShown))
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
            private$.state$companion$warnings <- character()
            private$.state$companion$prep <- prep
            private$.state$companion$model <- model
            private$.state$companion$fit <- NULL
            private$.state$companion$plotData <- NULL
            private$.state$companion$displayFactor <- NULL
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

            self$results$companionPcoaDescription$setVisible(
                is.null(plotData) || !isTRUE(plotData$available))
            self$results$companionPcoaSites$setVisible(TRUE)
            self$results$companionPcoaCentroids$setVisible(
                (isTRUE(self$options$pcoaCentroids) ||
                    isTRUE(self$options$pcoaSpiders)) &&
                !is.null(fit$centroids))
            self$results$companionPcoa$setVisible(
                !is.null(plotData) && isTRUE(plotData$available))
        },

        .companionDisplayFactor = function(prep, model) {
            eligible <- names(model$factorColumns)
            eligibleText <- if (length(eligible) == 0L)
                "none"
            else
                paste(eligible, collapse=", ")
            multifactor <- length(eligible) > 1L
            requested <- miso_clean_vars(self$options$pcoaDisplayFactor)
            notice <- character()

            unavailableMessage <- function(name, reason) {
                paste0(
                    "The selected display factor '", name, "' ", reason,
                    " Choose one eligible retained model factor: ",
                    eligibleText, ". ",
                    "The PERMANOVA and pairwise results remain available.")
            }

            if (!multifactor) {
                selected <- eligible[[1L]]
                if (length(requested) == 1L &&
                        !identical(requested[[1L]], selected)) {
                    stale <- requested[[1L]]
                    reason <- if (stale %in% model$droppedFactorNames)
                        paste0(
                            "was not retained in the fitted PERMANOVA model after data filtering; the companion PCoA automatically displays '",
                            selected, "'.")
                    else if (!stale %in% names(self$data))
                        paste0(
                            "is unavailable in the data; the companion PCoA automatically displays '",
                            selected, "'.")
                    else
                        paste0(
                            "is not a categorical factor retained in the fitted PERMANOVA model; the companion PCoA automatically displays '",
                            selected, "'.")
                    notice <- unavailableMessage(stale, reason)
                }
            } else {
                if (length(requested) != 1L ||
                        !requested[[1L]] %in% eligible) {
                    name <- if (length(requested) == 1L)
                        requested[[1L]]
                    else
                        "(none or multiple variables)"
                    reason <- if (length(requested) != 1L)
                        "is not a single retained model factor."
                    else if (name %in% model$droppedFactorNames)
                        "was not retained in the fitted PERMANOVA model after data filtering."
                    else if (!name %in% names(self$data))
                        "is unavailable in the data."
                    else
                        "is not a categorical factor retained in the fitted PERMANOVA model."
                    return(list(
                        valid=FALSE,
                        message=unavailableMessage(name, reason)))
                }
                selected <- requested[[1L]]
            }

            modelColumn <- unname(model$factorColumns[[selected]])
            if (is.null(selected) || length(modelColumn) != 1L ||
                    !modelColumn %in% names(model$data)) {
                return(list(
                    valid=FALSE,
                    message=unavailableMessage(
                        selected,
                        "is unavailable from the fitted model data.")))
            }
            groups <- droplevels(as.factor(model$data[[modelColumn]]))
            if (length(groups) != attr(prep$dist, "Size") || anyNA(groups)) {
                return(list(
                    valid=FALSE,
                    message=unavailableMessage(
                        selected,
                        "does not align with the sites retained for PERMANOVA.")))
            }
            if (nlevels(groups) < 2L) {
                return(list(
                    valid=FALSE,
                    message=unavailableMessage(
                        selected,
                        "has fewer than two groups after data filtering.")))
            }
            list(valid=TRUE, name=selected, groups=groups,
                automatic=!multifactor, notice=notice)
        },

        .showCompanionInstruction = function(message) {
            self$results$companionPcoa$setVisible(FALSE)
            self$results$companionPcoaSites$setVisible(FALSE)
            self$results$companionPcoaCentroids$setVisible(FALSE)
            self$results$companionPcoaDescription$setContent(
                miso_html_block(message,
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
                        character(0)
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
                private$.state$companion$warnings <- c(
                    private$.state$companion$warnings,
                    paste(
                        "The companion PCoA displays one grouping variable and",
                        "does not represent adjusted effects from the complete model."))
            if (length(selectionNotice) > 0L)
                private$.state$companion$warnings <- c(
                    private$.state$companion$warnings, selectionNotice)
            if (available && isTRUE(plotData$neutral))
                private$.state$companion$warnings <- c(
                    private$.state$companion$warnings,
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

        .permutationNotice = function(scheme, blockName) {
            hasBlock <- !is.null(blockName) && nzchar(blockName)
            if (identical(scheme, "stratified") && !hasBlock)
                return(list(
                    kind="error",
                    text=paste(
                        "Within-block permutations require a Blocking variable.",
                        "Add one, or select Free permutations.")))
            if (identical(scheme, "free") && hasBlock)
                return(list(
                    kind="warning",
                    text=sprintf(
                        "Blocking variable '%s' is assigned but not used with Free permutations.",
                        blockName)))
            if (identical(scheme, "stratified") && hasBlock)
                return(list(
                    kind="table",
                    text=sprintf(
                        "Block used: Yes (Blocking variable '%s' is used for Within blocks permutations).",
                        blockName)))
            list(kind="none", text="")
        },

        .permutationState = function(prep) {
            scheme <- self$options$permScheme
            hasBlock <- length(prep$assignedStrata) > 0L
            blockName <- if (hasBlock) prep$assignedStrata[[1L]] else NULL
            labels <- c(
                free="Free",
                stratified="Within blocks",
                series="Series")
            notice <- private$.permutationNotice(scheme, blockName)
            state <- list(
                error=NULL,
                warnings=character(),
                notice=notice,
                requested=unname(labels[[scheme]]),
                effective=unname(labels[[scheme]]),
                block=if (hasBlock) blockName else "None",
                blockUsed=hasBlock && ! identical(scheme, "free"),
                legalPermutations=NA_real_)

            if (identical(notice$kind, "error")) {
                state$error <- notice$text
                return(state)
            }

            if (identical(notice$kind, "warning")) {
                state$warnings <- c(state$warnings, notice$text)
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
            permutationCounts <- integer()
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
                permutationCounts <- c(permutationCounts,
                    private$.permutationCount(result))
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
            pairwiseRows <- lapply(seq_along(rows), function(i) {
                values <- rows[[i]]
                values$padj <- adjusted[[i]]
                list(key=as.character(i), values=values)
            })
            miso_reconcile_table_rows(self$results$pairwise, pairwiseRows)

            comparisons <- if (identical(self$options$permBy, "terms"))
                sprintf("Sequential comparisons of %s, tested first in each pairwise model.",
                    prep$primary)
            else sprintf("Marginal comparisons of %s in each pairwise model.",
                prep$primary)
            adjustment <- if (identical(method, "none")) "Unadjusted p-values."
            else sprintf("%s correction across %d contrasts.",
                private$.adjustmentLabel(method), length(pvals))
            self$results$pairwise$setNote(
                key="scope",
                note=paste0(comparisons, " ", adjustment,
                    private$.pairwisePermutationNote(permutationCounts)),
                init=FALSE)

            list(rows=length(rows), warnings=warnings)
        },



        # Conditional simple-effect eligibility for pairwise comparisons with
        # interactions. The fitted model is authoritative about which additional
        # factor survived filtering; raw requested selections are rejected even
        # when common preprocessing cleaned them away.
        .conditionalPairwiseEligibility = function(prep, model) {
            unsupported <- character()

            if (! identical(self$options$permBy, "margin"))
                unsupported <- c(unsupported, sprintf(
                    "Test type is %s",
                    private$.testTypeLabel(self$options$permBy)))
            selected <- unique(miso_clean_vars(self$options$permFactors))
            if (length(selected) != 1L)
                unsupported <- c(unsupported, sprintf(
                    "%d Additional factors are selected", length(selected)))
            else if (length(model$extraNames) != 1L)
                unsupported <- c(unsupported, sprintf(
                    "the Additional factor '%s' was not retained in the fitted model",
                    selected[[1L]]))
            if (length(miso_clean_vars(self$options$strata)) > 0L)
                unsupported <- c(unsupported,
                    "a Blocking variable is assigned")
            if (length(miso_clean_vars(self$options$covariates)) > 0L)
                unsupported <- c(unsupported,
                    "Continuous covariates are assigned")
            if (! identical(self$options$permScheme, "free"))
                unsupported <- c(unsupported,
                    "Permutation restrictions are not Free")
            if (! identical(self$options$permAdjust, "holm"))
                unsupported <- c(unsupported, sprintf(
                    "P-value adjustment is %s",
                    private$.adjustmentLabel(self$options$permAdjust)))
            if (isTRUE(self$options$distBinary))
                unsupported <- c(unsupported,
                    "binary presence/absence distances are enabled")
            if (isTRUE(self$options$distSqrt))
                unsupported <- c(unsupported,
                    "square-root distances are enabled")
            if (! identical(self$options$distAdd, "none"))
                unsupported <- c(unsupported,
                    "an additive constant is enabled")
            distanceSupported <-
                (identical(self$options$distance, "euclidean") &&
                        identical(self$options$transform, "none")) ||
                (identical(self$options$distance, "bray") &&
                        identical(self$options$transform, "fourthroot"))
            if (! distanceSupported)
                unsupported <- c(unsupported,
                    "the distance settings are not untransformed Euclidean or fourth-root Bray-Curtis")

            if (length(unsupported) == 0L)
                return(list(eligible=TRUE))

            list(
                eligible=FALSE,
                message=paste(
                    "Pairwise comparisons with Model interactions are available only as",
                    "conditional simple-effect tests within each level of one Additional factor.",
                    "They require Test type Marginal terms, exactly one Additional factor retained",
                    "in the fitted model, Holm P-value adjustment, Free permutations, no Blocking",
                    "variable, no Continuous covariates, and untransformed Euclidean or fourth-root",
                    "transformed Bray-Curtis distances without binary form, square-root distances,",
                    "or an additive constant.",
                    paste0(
                        "Unsupported here: ",
                        paste(unsupported, collapse="; "), "."),
                    "The main PERMANOVA results are unchanged."))
        },

        # Planned-family conditional (simple-effect) pairwise fits. The family
        # is registered before any fit: every unordered Grouping pair within
        # every retained level of the single Additional factor. Planned rows
        # keep their position when a subset cannot be estimated, and Holm
        # adjustment always spans the complete planned family.
        .runConditionalPairwisePermanova = function(prep, main) {
            aVar <- prep$primary
            bVar <- prep$extra[[1L]]
            aLevels <- levels(prep$group)
            bValues <- as.character(prep$data[[bVar]])
            bLevels <- levels(droplevels(as.factor(prep$data[[bVar]])))
            pairs <- utils::combn(aLevels, 2, simplify=FALSE)
            planned <- unlist(
                lapply(bLevels, function(bLevel)
                    lapply(pairs, function(pair)
                        list(a1=pair[[1L]], a2=pair[[2L]], b=bLevel))),
                recursive=FALSE)
            familyN <- length(planned)
            warnings <- character()
            rows <- vector("list", familyN)
            pvals <- rep(NA_real_, familyN)
            permutationCounts <- integer()
            distMat <- as.matrix(prep$dist)

            for (i in seq_len(familyN)) {
                plan <- planned[[i]]
                contrast <- sprintf("%s vs %s", plan$a1, plan$a2)
                warningContrast <- sprintf("%s (%s: %s)", contrast, bVar, plan$b)
                notEstimated <- function(reason) {
                    warnings <<- c(warnings, sprintf(
                        "Conditional comparison %s was not estimated: %s.",
                        warningContrast, reason))
                    rows[[i]] <<- list(
                        contrast=paste0(contrast, " (not estimated)"),
                        f=NA,
                        p=NA)
                }
                idx <- which(bValues == plan$b &
                    prep$group %in% c(plan$a1, plan$a2))
                subsetGroup <- droplevels(prep$group[idx])
                sizes <- table(subsetGroup)
                if (length(sizes) != 2L || any(sizes < 2L)) {
                    notEstimated(paste(
                        "each Grouping level needs at least two samples within",
                        sprintf("'%s: %s'", bVar, plan$b)))
                    next
                }
                subDist <- distMat[idx, idx, drop=FALSE]
                subsetDistances <- subDist[lower.tri(subDist)]
                if (any(! is.finite(subsetDistances)) ||
                        all(subsetDistances == 0)) {
                    notEstimated(
                        "the subset distances are all zero or not finite")
                    next
                }
                subPrep <- prep
                subPrep$dist <- stats::as.dist(subDist)
                subPrep$group <- subsetGroup
                subPrep$data <- prep$data[idx, , drop=FALSE]
                subPrep$extra <- character(0)
                subPrep$strata <- character(0)
                subPrep$covariates <- NULL
                miso_set_seed(subPrep)
                result <- tryCatch(
                    private$.adonisModel(subPrep),
                    error=function(e) e)
                if (inherits(result, "error")) {
                    notEstimated(result$message)
                    next
                }
                tab <- as.data.frame(result)
                residual <- match("Residual", rownames(tab))
                # Named .f1 row extraction, not the first table row.
                f <- suppressWarnings(as.numeric(tab[".f1", "F"]))
                p <- suppressWarnings(as.numeric(tab[".f1", "Pr(>F)"]))
                if (is.na(residual) ||
                        ! is.finite(tab$SumOfSqs[[residual]]) ||
                        tab$SumOfSqs[[residual]] <= 0 ||
                        ! is.finite(f) || ! is.finite(p)) {
                    notEstimated(paste(
                        "the subset fit did not produce a positive residual sum of",
                        "squares with finite test statistic and p-value"))
                    next
                }
                permutationCounts <- c(permutationCounts,
                    private$.permutationCount(result))
                rows[[i]] <- list(contrast=contrast, f=f, p=p)
                pvals[[i]] <- p
            }

            successful <- is.finite(pvals)
            adjusted <- rep(NA_real_, familyN)
            if (any(successful))
                adjusted[successful] <- stats::p.adjust(
                    pvals[successful], method="holm", n=familyN)
            pairwiseRows <- lapply(seq_len(familyN), function(i)
                list(
                    key=as.character(i),
                    values=list(
                        condition=planned[[i]]$b,
                        contrast=rows[[i]]$contrast,
                        f=rows[[i]]$f,
                        p=rows[[i]]$p,
                        padj=if (is.na(adjusted[[i]])) NA else adjusted[[i]])))
            self$results$pairwise$getColumn("condition")$setTitle(bVar)
            self$results$pairwise$getColumn("condition")$setVisible(TRUE)
            miso_reconcile_table_rows(self$results$pairwise, pairwiseRows)

            self$results$pairwise$setNote(
                key="scope",
                note=paste0(sprintf(
                    "%s comparisons within each %s level. Holm correction across %d planned contrasts.",
                    aVar, bVar, familyN),
                    private$.pairwisePermutationNote(permutationCounts)),
                init=FALSE)

            list(rows=familyN, warnings=warnings)
        },

        .permutationCount = function(result) {
            nrow(attr(result, "F.perm"))
        },

        .permutationRestrictionNote = function() {
            permutation <- private$.state$permutation
            if (identical(self$options$permScheme, "series")) {
                block <- if (isTRUE(permutation$blockUsed))
                    sprintf(" within blocks defined by %s", permutation$block) else ""
                return(sprintf("Series permutations%s in data-row order.", block))
            }
            if (isTRUE(permutation$blockUsed))
                return(sprintf("Permutations within blocks defined by %s.", permutation$block))
            ""
        },

        .pairwisePermutationNote = function(counts) {
            # Subsets may exhaust their legal permutations before the main fit.
            if (length(counts) == 0L || all(counts == private$.state$mainPermutationN))
                return("")
            limits <- range(counts)
            count <- if (limits[[1L]] == limits[[2L]]) as.character(limits[[1L]])
                else sprintf("%d–%d", limits[[1L]], limits[[2L]])
            sprintf(" %s permutations per contrast.", count)
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
                fourthroot="Fourth-root",
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
