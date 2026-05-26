
# This file is a generated template, your changes will not be overwritten

permdispClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "permdispClass",
    inherit = permdispBase,
    private = list(
        .state = list(),

        .run = function() {
            private$.state <- list(warnings=character(), distances=NULL)
            private$.resetResults()

            prep <- tofu_prepare_resemblance(
                data=self$data,
                vars=self$options$vars,
                factor=self$options$factor,
                transform=self$options$transform,
                distance=self$options$distance,
                seed=self$options$seed)
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
            self$results$note$setContent("")
        },

        .runDispersion = function(prep) {
            tofu_set_seed(prep)
            fit <- tryCatch(
                vegan::betadisper(prep$dist, prep$group, type=self$options$dispType, bias.adjust=self$options$dispBias),
                error=function(e) e)
            if (inherits(fit, "error")) {
                self$results$note$setContent(paste0("PERMDISP failed: ", fit$message))
                return()
            }

            private$.state$distances <- data.frame(group=prep$group, distance=fit$distances, check.names=FALSE)
            means <- tapply(fit$distances, prep$group, mean, na.rm=TRUE)
            for (i in seq_along(means))
                self$results$distances$addRow(rowKey=names(means)[i], values=list(group=names(means)[i], distance=means[[i]]))

            perm <- tryCatch(vegan::permutest(fit, permutations=as.integer(self$options$permN)), error=function(e) e)
            if (inherits(perm, "error")) {
                self$results$note$setContent(paste0("PERMDISP permutation test failed: ", perm$message))
                return()
            }

            atab <- as.data.frame(perm$tab)
            rn <- rownames(atab)
            for (i in seq_len(nrow(atab))) {
                self$results$anova$addRow(rowKey=as.character(i), values=list(
                    source=rn[i],
                    df=tofu_num_or_na(atab[i, "Df"]),
                    sumsqs=tofu_num_or_na(atab[i, "Sum Sq"]),
                    meansq=tofu_num_or_na(atab[i, "Mean Sq"]),
                    f=tofu_num_or_na(atab[i, "F"]),
                    p=tofu_num_or_na(atab[i, "Pr(>F)"])))
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
