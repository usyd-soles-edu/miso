
# This file is a generated template, your changes will not be overwritten

anosimClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "anosimClass",
    inherit = anosimBase,
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
                strata=self$options$strata, distBinary=self$options$distBinary)
            if (prep$error) {
                self$results$warnings$setContent(prep$message)
                return()
            }

            tofu_populate_summary(self$results, prep, self$options$transform, self$options$distance)
            private$.state$warnings <- c(private$.state$warnings, prep$warnings)
            private$.runAnosim(prep)

            if (length(private$.state$warnings) > 0)
                self$results$warnings$setContent(paste(private$.state$warnings, collapse="\n"))
        },

        .resetResults = function() {
            self$results$warnings$setContent("")
            tofu_clear_table(self$results$summary)
            tofu_clear_table(self$results$global)
            tofu_clear_table(self$results$pairwise)
            self$results$note$setContent("")
        },

        .runAnosim = function(prep) {
            tofu_set_seed(prep)
            strata <- NULL
            if (length(prep$strata) > 0)
                strata <- droplevels(as.factor(prep$data[[prep$strata[[1]]]]))

            fit <- tryCatch(
                vegan::anosim(prep$dist, prep$group, permutations=tofu_permutation(self$options$anosimN, self$options$permScheme, strata), parallel=private$.state$cl),
                error=function(e) e)
            if (inherits(fit, "error")) {
                self$results$note$setContent(paste0("ANOSIM failed: ", fit$message))
                return()
            }

            self$results$global$addRow(rowKey="r", values=list(
                statistic="Global R",
                value=as.numeric(fit$statistic),
                p=as.numeric(fit$signif),
                permutations=as.integer(self$options$anosimN)))

            private$.runPairwiseAnosim(prep)
            self$results$note$setContent(sprintf("ANOSIM is a rank-based group comparison. PERMANOVA is usually preferred for model-based group testing.\nR close to 1 indicates strong group separation; R close to 0 indicates little separation.\nPermutations: %s\nSeed: %s",
                self$options$anosimN,
                ifelse(is.na(prep$seed), "random", prep$seed)))
        },

        .runPairwiseAnosim = function(prep) {
            groups <- levels(prep$group)
            if (length(groups) < 3)
                return()

            distMat <- as.matrix(prep$dist)
            pairs <- utils::combn(groups, 2, simplify=FALSE)
            rows <- vector("list", length(pairs))
            for (i in seq_along(pairs)) {
                pair <- pairs[[i]]
                idx <- prep$group %in% pair
                subGroup <- droplevels(prep$group[idx])
                contrast <- paste(pair, collapse=" vs ")
                r <- NA_real_; p <- NA_real_
                if (! any(table(subGroup) < 2)) {
                    subDist <- stats::as.dist(distMat[idx, idx, drop=FALSE])
                    tofu_set_seed(prep)
                    fit <- tryCatch(
                        vegan::anosim(subDist, subGroup, permutations=as.integer(self$options$anosimN)),
                        error=function(e) e)
                    if (! inherits(fit, "error")) {
                        r <- as.numeric(fit$statistic)
                        p <- as.numeric(fit$signif)
                    }
                }
                rows[[i]] <- list(contrast=contrast, r=r, p=p)
            }

            pvals <- vapply(rows, function(x) x$p, NA_real_)
            method <- self$options$anosimAdjust
            padj <- if (identical(method, "none")) pvals else stats::p.adjust(pvals, method=method)
            for (i in seq_along(rows)) {
                rows[[i]]$padj <- padj[i]
                self$results$pairwise$addRow(rowKey=as.character(i), values=rows[[i]])
            }
        }
    )
)
