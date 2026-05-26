
# This file is a generated template, your changes will not be overwritten

nmdsClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "nmdsClass",
    inherit = nmdsBase,
    private = list(
        .state = list(),

        .run = function() {
            private$.state <- list(warnings=character(), nmds=NULL, group=NULL)
            private$.resetResults()

            prep <- tofu_prepare_resemblance(
                data=self$data,
                vars=self$options$vars,
                factor=self$options$factor,
                transform=self$options$transform,
                distance=self$options$distance,
                seed=self$options$seed,
                requireFactor=FALSE)
            if (prep$error) {
                self$results$warnings$setContent(prep$message)
                return()
            }

            tofu_populate_summary(self$results, prep, self$options$transform, self$options$distance)
            private$.state$warnings <- c(private$.state$warnings, prep$warnings)
            private$.runNmds(prep)

            if (length(private$.state$warnings) > 0)
                self$results$warnings$setContent(paste(private$.state$warnings, collapse="\n"))
        },

        .resetResults = function() {
            self$results$warnings$setContent("")
            tofu_clear_table(self$results$summary)
            tofu_clear_table(self$results$stress)
            self$results$note$setContent("")
        },

        .runNmds = function(prep) {
            tofu_set_seed(prep)
            k <- as.integer(self$options$nmdsK)
            note <- character()
            if (prep$rowsUsed <= 2 * k + 1)
                note <- c(note, sprintf("nMDS with %d samples in %d dimensions may be unreliable (n <= 2k + 1).", prep$rowsUsed, k))

            fit <- tryCatch(
                withCallingHandlers(
                    vegan::metaMDS(
                        prep$transformed,
                        distance=self$options$distance,
                        k=k,
                        trymax=as.integer(self$options$nmdsTrymax),
                        maxit=as.integer(self$options$nmdsMaxit),
                        autotransform=FALSE,
                        trace=FALSE),
                    warning=function(w) {
                        note <<- c(note, conditionMessage(w))
                        invokeRestart("muffleWarning")
                    }),
                error=function(e) e)
            if (inherits(fit, "error")) {
                self$results$note$setContent(paste0("nMDS failed: ", fit$message))
                return()
            }

            private$.state$nmds <- fit
            private$.state$group <- prep$group

            self$results$stress$addRow(rowKey="stress", values=list(item="Stress", value=sprintf("%.4f", fit$stress)))
            self$results$stress$addRow(rowKey="dimensions", values=list(item="Dimensions", value=as.character(k)))
            self$results$stress$addRow(rowKey="trymax", values=list(item="Random starts", value=as.character(self$options$nmdsTrymax)))
            self$results$stress$addRow(rowKey="maxit", values=list(item="Max iterations per run", value=as.character(self$options$nmdsMaxit)))
            self$results$stress$addRow(rowKey="autotransform", values=list(item="autotransform", value="FALSE"))

            if (fit$stress > 0.2)
                note <- c(note, sprintf("High stress (%.3f): the ordination may be unreliable. Consider increasing dimensions.", fit$stress))

            interpretation <- if (fit$stress < 0.05) "excellent" else if (fit$stress < 0.1) "good" else if (fit$stress < 0.2) "usable with care" else "unreliable"
            note <- c(note, sprintf("Stress interpretation: %s.", interpretation))
            note <- c(note, sprintf("nMDS uses %s dissimilarity on transformed data with autotransform=FALSE.", self$options$distance))
            note <- c(note, sprintf("Seed: %s", ifelse(is.na(prep$seed), "random", prep$seed)))
            self$results$note$setContent(paste(note, collapse="\n"))
        },

        .plotNmds = function(image, ...) {
            fit <- private$.state$nmds
            if (is.null(fit))
                return()
            group <- private$.state$group
            scores <- vegan::scores(fit, display="sites")
            graphics::plot(scores[, 1], scores[, 2], type="n", xlab="nMDS1", ylab="nMDS2", main="nMDS Ordination")
            if (self$options$nmdsOverlay && ! is.null(group)) {
                cols <- as.integer(group)
                graphics::points(scores[, 1], scores[, 2], pch=19, col=cols)
                graphics::legend("topright", legend=levels(group), col=seq_along(levels(group)), pch=19, bty="n")
            }
            else {
                graphics::points(scores[, 1], scores[, 2], pch=19)
            }
            graphics::mtext(sprintf("Stress = %.3f", fit$stress), side=3, adj=1, line=0.2)
        },

        .plotShepard = function(image, ...) {
            fit <- private$.state$nmds
            if (is.null(fit) || ! self$options$nmdsShepard)
                return()
            vegan::stressplot(fit, main="Shepard Diagram")
        }
    )
)
