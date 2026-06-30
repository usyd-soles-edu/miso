
# This file is a generated template, your changes will not be overwritten

nmdsClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "nmdsClass",
    inherit = nmdsBase,
    private = list(
        .state = list(),

        .run = function() {
            private$.state <- list(warnings=character(), nmds=NULL, group=NULL, envfit=NULL)
            private$.resetResults()

            prep <- tofu_prepare_resemblance(
                data=self$data,
                vars=self$options$vars,
                factor=self$options$factor,
                transform=self$options$transform,
                distance=self$options$distance,
                seed=self$options$seed,
                requireFactor=FALSE, covariates=self$options$nmdsEnv, distBinary=self$options$distBinary)
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
            tofu_clear_table(self$results$envfit)
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
                        wascores=isTRUE(self$options$nmdsSpecies),
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

            if (! is.null(prep$covariates) && ncol(prep$covariates) > 0) {
                ef <- tryCatch(
                    vegan::envfit(fit, prep$covariates, permutations=99L),
                    error=function(e) e)
                if (! inherits(ef, "error") && ! is.null(ef$vectors) && length(ef$vectors$r) > 0) {
                    private$.state$envfit <- ef
                    vsc <- vegan::scores(ef, display="vectors")
                    r2 <- ef$vectors$r
                    pv <- ef$vectors$pvals
                    for (nm in rownames(vsc))
                        self$results$envfit$addRow(rowKey=nm, values=list(
                            variable=nm,
                            r2=tofu_num_or_na(r2[nm]),
                            p=tofu_num_or_na(pv[nm]),
                            NMDS1=tofu_num_or_na(vsc[nm, 1]),
                            NMDS2=tofu_num_or_na(vsc[nm, 2])))
                } else if (inherits(ef, "error")) {
                    note <- c(note, paste0("envfit failed: ", ef$message))
                }
            }

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
            ef <- private$.state$envfit
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
            if (isTRUE(self$options$nmdsSpecies)) {
                sp <- tryCatch(vegan::scores(fit, display="species"), error=function(e) NULL)
                if (! is.null(sp) && nrow(sp) > 0)
                    graphics::text(sp[, 1], sp[, 2], labels=rownames(sp), cex=0.7, col="grey40")
            }
            if (! is.null(ef) && ! is.null(ef$vectors)) {
                vsc <- vegan::scores(ef, display="vectors")
                graphics::arrows(0, 0, vsc[, 1], vsc[, 2], length=0.05, col="darkred")
                graphics::text(vsc[, 1] * 1.12, vsc[, 2] * 1.12, labels=rownames(vsc), col="darkred", cex=0.75)
            }
            if (! is.null(group)) {
                gp <- seq_along(levels(group))
                if (isTRUE(self$options$nmdsHull))    try(vegan::ordihull(fit, group, col=gp, label=FALSE), silent=TRUE)
                if (isTRUE(self$options$nmdsEllipse)) try(vegan::ordiellipse(fit, group, col=gp, kind="se", label=FALSE), silent=TRUE)
                if (isTRUE(self$options$nmdsSpider))  try(vegan::ordispider(fit, group, col=gp, label=FALSE), silent=TRUE)
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
