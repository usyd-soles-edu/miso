
# This file is a generated template, your changes will not be overwritten

permanovaClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "permanovaClass",
    inherit = permanovaBase,
    private = list(
        .state = list(),

        .run = function() {
            private$.state <- list(warnings=character())
            private$.state$cl <- tofu_parallel(self$options$useParallel)
            on.exit(tofu_parallel_stop(private$.state$cl), add=TRUE)
            private$.resetResults()

            prep <- tofu_prepare_resemblance(
                data=self$data,
                vars=self$options$vars,
                factor=self$options$factor,
                transform=self$options$transform,
                distance=self$options$distance,
                seed=self$options$seed,
                extraVars=self$options$permFactors,
                strata=self$options$strata, covariates=self$options$covariates, distBinary=self$options$distBinary)
            if (prep$error) {
                self$results$warnings$setContent(prep$message)
                return()
            }

            tofu_populate_summary(self$results, prep, self$options$transform, self$options$distance)
            private$.state$warnings <- c(private$.state$warnings, prep$warnings)
            private$.runPermanova(prep)

            if (length(private$.state$warnings) > 0)
                self$results$warnings$setContent(paste(private$.state$warnings, collapse="\n"))
        },

        .resetResults = function() {
            self$results$warnings$setContent("")
            tofu_clear_table(self$results$summary)
            tofu_clear_table(self$results$table)
            tofu_clear_table(self$results$pairwise)
            self$results$note$setContent("")
        },

        .runPermanova = function(prep) {
            tofu_set_seed(prep)
            result <- tryCatch(private$.adonisModel(prep), error=function(e) e)
            if (inherits(result, "error")) {
                self$results$note$setContent(paste0("PERMANOVA failed: ", result$message))
                return()
            }

            tab <- as.data.frame(result)
            rn <- rownames(tab)
            for (i in seq_len(nrow(tab))) {
                self$results$table$addRow(rowKey=as.character(i), values=list(
                    source=tofu_display_term(rn[i], prep),
                    df=tofu_num_or_na(tab[i, "Df"]),
                    sumsqs=tofu_num_or_na(tab[i, "SumOfSqs"]),
                    r2=tofu_num_or_na(tab[i, "R2"]),
                    f=tofu_num_or_na(tab[i, "F"]),
                    p=tofu_num_or_na(tab[i, "Pr(>F)"])))
            }

            self$results$note$setContent(sprintf("Transform: %s\nDissimilarity: %s\nPermutations: %s\nTest type: %s\nSeed: %s",
                self$options$transform,
                self$options$distance,
                self$options$permN,
                self$options$permBy,
                ifelse(is.na(prep$seed), "random", prep$seed)))

            if (self$options$permPairwise)
                private$.runPairwisePermanova(prep)
        },

        .adonisModel = function(prep) {
            model <- private$.makeModelData(prep)
            by <- self$options$permBy
            if (identical(by, "omnibus"))
                by <- NULL

            formula <- stats::as.formula(paste(".dist ~", model$terms))
            mm <- stats::model.matrix(stats::delete.response(stats::terms(formula)), data=model$data)
            if (nrow(model$data) - qr(mm)$rank <= 0)
                stop("PERMANOVA model is saturated (no residual degrees of freedom).")

            .dist <- prep$dist
            vegan::adonis2(
                formula,
                data=model$data,
                permutations=tofu_permutation(self$options$permN, self$options$permScheme, model$strata),
                by=by, parallel=private$.state$cl, sqrt.dist=isTRUE(self$options$distSqrt), add=if (identical(self$options$distAdd, "none")) FALSE else self$options$distAdd)
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
                        terms <- paste(c(".f1", paste0(".f1:", extraNames), extraNames), collapse=" + ")
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

            list(terms=terms, data=data, strata=strata)
        },

        .runPairwisePermanova = function(prep) {
            if (self$options$permInteractions) {
                self$results$note$setContent(paste(self$results$note$asString(), "Pairwise PERMANOVA disabled when interactions are enabled.", sep="\n"))
                return()
            }

            groups <- levels(prep$group)
            pairs <- utils::combn(groups, 2, simplify=FALSE)
            pvals <- rep(NA_real_, length(pairs))
            rows <- vector("list", length(pairs))
            distMat <- as.matrix(prep$dist)

            for (i in seq_along(pairs)) {
                pair <- pairs[[i]]
                idx <- prep$group %in% pair
                subPrep <- prep
                subPrep$dist <- stats::as.dist(distMat[idx, idx, drop=FALSE])
                subPrep$group <- droplevels(prep$group[idx])
                subPrep$data <- prep$data[idx, , drop=FALSE]
                contrast <- paste(pair, collapse=" vs ")

                tofu_set_seed(prep)
                res <- tryCatch(private$.adonisModel(subPrep), error=function(e) e)
                if (inherits(res, "error")) {
                    rows[[i]] <- list(contrast=contrast, f=NA_real_, p=NA_real_)
                }
                else {
                    tab <- as.data.frame(res)
                    rows[[i]] <- list(
                        contrast=contrast,
                        f=tofu_num_or_na(tab[1, "F"]),
                        p=tofu_num_or_na(tab[1, "Pr(>F)"]))
                    pvals[i] <- rows[[i]]$p
                }
            }

            method <- self$options$permAdjust
            adj <- if (identical(method, "none")) pvals else stats::p.adjust(pvals, method=method)
            for (i in seq_along(rows)) {
                rows[[i]]$padj <- adj[i]
                self$results$pairwise$addRow(rowKey=as.character(i), values=rows[[i]])
            }
        }
    )
)
