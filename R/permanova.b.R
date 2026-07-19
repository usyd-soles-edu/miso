# This file is a generated template, your changes will not be overwritten

permanovaClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "permanovaClass",
    inherit = permanovaBase,
    private = list(
        .state = list(),

        .run = function() {
            private$.state <- list(warnings=character(), cl=NULL, permutation=NULL)
            private$.resetResults()

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

            if (tofu_is_missing_var(self$options$factor)) {
                private$.showGuidance(
                    paste(
                        "PERMANOVA is waiting for a Grouping variable.",
                        "Add one categorical variable containing at least two groups."),
                    title="Action needed")
                return()
            }

            if (identical(self$options$permScheme, "stratified") &&
                    tofu_is_missing_var(self$options$strata)) {
                private$.showGuidance(
                    paste(
                        "Within-block permutations require a Blocking variable.",
                        "Add one, or select Free permutations."),
                    title="Action needed")
                return()
            }

            prep <- tofu_prepare_resemblance(
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
            if (prep$error) {
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
            private$.state$cl <- tofu_parallel(self$options$useParallel)
            on.exit(tofu_parallel_stop(private$.state$cl), add=TRUE)

            main <- private$.runPermanova(prep)
            if (! main$success) {
                private$.resetResults()
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

            tofu_populate_summary(
                self$results,
                prep,
                self$options$transform,
                self$options$distance)

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

        .resetResults = function() {
            self$results$guidance$setContent("")
            self$results$warnings$setContent("")
            tofu_clear_table(self$results$summary)
            tofu_clear_table(self$results$table)
            tofu_clear_table(self$results$pairwise)
            self$results$table$setNote(
                key="structuralCells",
                note="")
            self$results$pairwise$setNote(
                key="scope",
                note="")
            self$results$note$setContent("")
            tofu_clear_table(self$results$settings)

            for (name in c(
                    "guidance", "warnings", "summary", "table",
                    "pairwise", "note", "settings"))
                self$results[[name]]$setVisible(FALSE)
        },

        .showGuidance = function(content, title="Action needed") {
            self$results$guidance$setTitle(title)
            self$results$guidance$setContent(tofu_html_block(content))
            self$results$guidance$setVisible(TRUE)
        },

        .showSuccessfulResults = function(pairwiseShown=FALSE) {
            for (name in c("summary", "table", "note", "settings"))
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
            self$results$warnings$setContent(tofu_html_block(warnings))
            self$results$warnings$setVisible(TRUE)
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

            control <- tofu_permutation(
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
            tofu_set_seed(prep)
            result <- tryCatch(private$.adonisModel(prep), error=function(e) e)
            if (inherits(result, "error"))
                return(list(success=FALSE, error=result$message))

            tab <- as.data.frame(result)
            rn <- rownames(tab)
            for (i in seq_len(nrow(tab))) {
                notApplicable <- rn[i] %in% c("Residual", "Total")
                rowKey <- as.character(i)
                values <- list(
                    source=tofu_display_term(rn[i], prep),
                    df=tofu_num_or_na(tab[i, "Df"]),
                    sumsqs=tofu_num_or_na(tab[i, "SumOfSqs"]),
                    r2=tofu_num_or_na(tab[i, "R2"]),
                    f=if (notApplicable) "" else tofu_num_or_na(tab[i, "F"]),
                    p=if (notApplicable) "" else tofu_num_or_na(tab[i, "Pr(>F)"]))
                self$results$table$addRow(rowKey=rowKey, values=values)
                if (notApplicable) {
                    self$results$table$getCell(
                        rowKey=rowKey,
                        col="f")$addFootnote(
                            "Not applicable for Residual and Total rows.")
                    self$results$table$getCell(
                        rowKey=rowKey,
                        col="p")$addFootnote(
                            "Not applicable for Residual and Total rows.")
                }
            }
            self$results$table$setNote(
                key="structuralCells",
                note=paste(
                    "Blank Pseudo-F and Permutation p cells for Residual and Total",
                    "are structurally not applicable."))

            list(
                success=TRUE,
                result=result,
                model=private$.makeModelData(prep))
        },

        .adonisModel = function(prep) {
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
                permutations=tofu_permutation(
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

            if (length(prep$extra) > 0) {
                for (i in seq_along(prep$extra)) {
                    f <- droplevels(as.factor(prep$data[[prep$extra[[i]]]]))
                    if (nlevels(f) < 2)
                        next
                    nm <- paste0(".f", i + 1)
                    data[[nm]] <- f
                    extraNames <- c(extraNames, nm)
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
                covariateNames=covNames)
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

                tofu_set_seed(subPrep)
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
                f <- tofu_num_or_na(tab[1, "F"])
                p <- tofu_num_or_na(tab[1, "Pr(>F)"])
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
            text <- paste(
                "R\u00B2 is the proportion of multivariate variation associated with each model term.",
                paste(
                    "A permutation p-value assesses evidence that the corresponding term",
                    "explains differences in multivariate composition under the selected permutation model."))

            multiTerm <- length(model$extraNames) > 0L ||
                length(model$covariateNames) > 0L
            if (multiTerm) {
                if (identical(self$options$permBy, "terms"))
                    text <- paste(
                        text,
                        "Sequential tests depend on model-term order.")
                else if (identical(self$options$permBy, "margin"))
                    text <- paste(
                        text,
                        "Marginal tests assess each term while accounting for the others.")
                else
                    text <- paste(text, "The Omnibus test assesses the model as a whole.")
            }

            text <- paste(
                text,
                paste(
                    "Unequal dispersion can contribute to a PERMANOVA result.",
                    "Examine PERMDISP alongside PERMANOVA, but do not treat PERMDISP alone",
                    "as proof or disproof of differences in group centres."))
            if (pairwiseShown)
                text <- paste(
                    text,
                    "Pairwise comparisons concern levels of the primary Grouping variable.",
                    if (identical(self$options$permAdjust, "none"))
                        "Unadjusted p-values are shown for the pairwise comparisons."
                    else
                        sprintf(
                            "%s-adjusted p-values address multiple pairwise comparisons.",
                            private$.adjustmentLabel(self$options$permAdjust)))

            self$results$note$setContent(paste0(
                '<h2>Interpretation</h2>',
                tofu_html_block(text)))
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
            value <- tofu_clean_vars(value)
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
