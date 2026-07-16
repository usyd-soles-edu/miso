
# This file is a generated template, your changes will not be overwritten

permdispClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "permdispClass",
    inherit = permdispBase,
    private = list(
        .state = list(),

        .run = function() {
            private$.state <- list(warnings=character(), distances=NULL)
            private$.state$cl <- tofu_parallel(self$options$useParallel)
            on.exit(tofu_parallel_stop(private$.state$cl), add=TRUE)
            private$.resetResults()

            prep <- tofu_prepare_resemblance(
                data=self$data,
                vars=self$options$vars,
                factor=self$options$factor,
                transform=self$options$transform,
                distance=self$options$distance,
                seed=self$options$seed, distBinary=self$options$distBinary)
            if (prep$error) {
                self$results$warnings$setContent(prep$message)
                return()
            }

            tofu_populate_summary(self$results, prep, self$options$transform, self$options$distance)
            private$.state$warnings <- c(private$.state$warnings, prep$warnings)
            private$.runDispersion(prep)

            if (length(private$.state$warnings) > 0)
                self$results$warnings$setContent(paste(private$.state$warnings, collapse="\n"))
        },

        .resetResults = function() {
            self$results$warnings$setContent("")
            tofu_clear_table(self$results$summary)
            tofu_clear_table(self$results$distances)
            tofu_clear_table(self$results$anova)
            tofu_clear_table(self$results$pairwise)
            self$results$note$setContent("")
        },

        .runDispersion = function(prep) {
            tofu_set_seed(prep)
            fit <- tryCatch(
                vegan::betadisper(prep$dist, prep$group, type=self$options$dispType, bias.adjust=self$options$dispBias, sqrt.dist=isTRUE(self$options$distSqrt), add=if (identical(self$options$distAdd, "none")) FALSE else self$options$distAdd),
                error=function(e) e)
            if (inherits(fit, "error")) {
                self$results$note$setContent(paste0("PERMDISP failed: ", fit$message))
                return()
            }

            private$.state$distances <- data.frame(group=prep$group, distance=fit$distances, check.names=FALSE)
            means <- tapply(fit$distances, prep$group, mean, na.rm=TRUE)
            for (i in seq_along(means))
                self$results$distances$addRow(rowKey=names(means)[i], values=list(group=names(means)[i], distance=means[[i]]))

            perm <- tryCatch(vegan::permutest(fit, permutations=tofu_permutation(self$options$permN, self$options$permScheme, NULL), parallel=private$.state$cl), error=function(e) e)
            if (inherits(perm, "error")) {
                self$results$note$setContent(paste0("PERMDISP permutation test failed: ", perm$message))
                return()
            }

            atab <- as.data.frame(perm$tab)
            rn <- rownames(atab)
            for (i in seq_len(nrow(atab))) {
                notApplicable <- rn[i] %in% c("Residual", "Residuals")
                values <- list(
                    source=rn[i],
                    df=tofu_num_or_na(atab[i, "Df"]),
                    sumsqs=tofu_num_or_na(atab[i, "Sum Sq"]),
                    meansq=tofu_num_or_na(atab[i, "Mean Sq"]),
                    f=if (notApplicable) "" else tofu_num_or_na(atab[i, "F"]),
                    p=if (notApplicable) "" else tofu_num_or_na(atab[i, "Pr(>F)"]))
                self$results$anova$addRow(rowKey=as.character(i), values=values)
            }

            if (isTRUE(self$options$dispPairwise) && nlevels(prep$group) >= 3) {
                pt <- tryCatch(
                    vegan::permutest(fit, permutations=tofu_permutation(self$options$permN, self$options$permScheme, NULL), pairwise=TRUE, parallel=private$.state$cl),
                    error=function(e) e)
                if (! inherits(pt, "error") && ! is.null(pt$pairwise) && length(pt$pairwise$permuted) > 0) {
                    labels <- names(pt$pairwise$permuted)
                    tstat <- pt$statistic[-1]
                    pperm <- as.numeric(pt$pairwise$permuted)
                    padj <- if (identical(self$options$dispAdjust, "none")) pperm else stats::p.adjust(pperm, method=self$options$dispAdjust)
                    for (i in seq_along(labels))
                        self$results$pairwise$addRow(rowKey=as.character(i), values=list(
                            contrast=gsub("-", " vs ", labels[i], fixed=TRUE),
                            statistic=tofu_num_or_na(tstat[i]),
                            p=tofu_num_or_na(pperm[i]),
                            padj=tofu_num_or_na(padj[i])))
                }
            }

            self$results$note$setContent(sprintf("PERMDISP tests whether groups differ in multivariate dispersion. Use it to check whether a group-comparison result may be driven by spread rather than location.\nCentre: %s\nBias adjustment: %s\nPermutations: %s\nSeed: %s",
                self$options$dispType,
                self$options$dispBias,
                self$options$permN,
                ifelse(is.na(prep$seed), "random", prep$seed)))
        },

        .plotDistances = function(image, ...) {
            dat <- private$.state$distances
            if (is.null(dat) || nrow(dat) == 0)
                return()

            graphics::boxplot(distance ~ group, data=dat, xlab="Group", ylab="Distance to centre", main="Distances to Centre", col="grey90", border="grey35")
            graphics::stripchart(distance ~ group, data=dat, vertical=TRUE, method="jitter", pch=19, col="#277da1", add=TRUE)
        }
    )
)
