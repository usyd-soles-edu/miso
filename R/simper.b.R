
# This file is a generated template, your changes will not be overwritten

simperClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "simperClass",
    inherit = simperBase,
    private = list(
        .state = list(),

        .run = function() {
            private$.state <- list(warnings=character(), plotData=NULL)
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
            private$.runSimper(prep)

            if (length(private$.state$warnings) > 0)
                self$results$warnings$setContent(paste(private$.state$warnings, collapse="\n"))
        },

        .resetResults = function() {
            self$results$warnings$setContent("")
            tofu_clear_table(self$results$summary)
            tofu_clear_table(self$results$table)
            self$results$note$setContent("")
        },

        .runSimper = function(prep) {
            tofu_set_seed(prep)
            fit <- tryCatch(
                vegan::simper(prep$transformed, prep$group, permutations=as.integer(self$options$simperN)),
                error=function(e) e)
            if (inherits(fit, "error")) {
                self$results$note$setContent(paste0("SIMPER failed: ", fit$message))
                return()
            }

            summaries <- summary(fit)
            row <- 1L
            topN <- as.integer(self$options$simperTop)
            threshold <- as.numeric(self$options$simperCum) / 100
            plotRows <- list()

            for (contrast in names(summaries)) {
                tab <- as.data.frame(summaries[[contrast]])
                if (nrow(tab) == 0)
                    next
                tab$feature <- rownames(tab)
                if (! "average" %in% names(tab))
                    next
                tab$contribution <- tab$average / sum(tab$average, na.rm=TRUE)
                tab$cumulative <- cumsum(tab$contribution)
                keep <- seq_len(nrow(tab)) <= topN & tab$cumulative <= threshold
                if (! any(keep))
                    keep[seq_len(min(topN, nrow(tab)))] <- TRUE
                tab <- tab[keep, , drop=FALSE]

                for (i in seq_len(nrow(tab))) {
                    values <- list(
                        contrast=contrast,
                        feature=tab$feature[i],
                        average=tofu_num_or_na(tab$average[i]),
                        sd=tofu_num_or_na(tab$sd[i]),
                        ratio=tofu_num_or_na(tab$ratio[i]),
                        contribution=100 * tofu_num_or_na(tab$contribution[i]),
                        cumulative=100 * tofu_num_or_na(tab$cumulative[i]))
                    self$results$table$addRow(rowKey=as.character(row), values=values)
                    plotRows[[length(plotRows) + 1L]] <- values
                    row <- row + 1L
                }
            }

            if (length(plotRows) > 0)
                private$.state$plotData <- do.call(rbind, lapply(plotRows, as.data.frame, stringsAsFactors=FALSE))

            note <- "SIMPER decomposes Bray-Curtis dissimilarity on the transformed feature data. Treat it as exploratory follow-up rather than evidence of group differences."
            if (! identical(self$options$distance, "bray"))
                note <- paste(note, "The selected dissimilarity index is not used by SIMPER; SIMPER always uses Bray-Curtis.")
            note <- paste(note, sprintf("\nPermutations: %s\nSeed: %s", self$options$simperN, ifelse(is.na(prep$seed), "random", prep$seed)))
            self$results$note$setContent(note)
        },

        .plotContributions = function(image, ...) {
            dat <- private$.state$plotData
            if (is.null(dat) || nrow(dat) == 0)
                return()

            firstContrast <- dat$contrast[[1]]
            dat <- dat[dat$contrast == firstContrast, , drop=FALSE]
            dat$feature <- stats::reorder(dat$feature, dat$contribution)
            op <- graphics::par(mar=c(5, 8, 4, 2))
            on.exit(graphics::par(op), add=TRUE)
            graphics::barplot(
                height=dat$contribution,
                names.arg=dat$feature,
                horiz=TRUE,
                las=1,
                col="#277da1",
                border=NA,
                xlab="Contribution (%)",
                main=paste("SIMPER Contributions:", firstContrast))
        }
    )
)
