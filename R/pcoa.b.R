
# This file is a generated template, your changes will not be overwritten

pcoaClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "pcoaClass",
    inherit = pcoaBase,
    private = list(
        .state = list(),
        .lastStructuralKey = NULL,
        .structuralChanged = TRUE,
        .rowCursors = list(),

        .run = function() {
            structuralKey <- private$.structuralKey()
            private$.structuralChanged <- !identical(
                private$.lastStructuralKey, structuralKey)
            if (!private$.structuralChanged) {
                private$.refreshDisplayOnly()
                return()
            }
            private$.lastStructuralKey <- structuralKey
            private$.rowCursors <- list()
            private$.state <- list(prep=NULL, pcoa=NULL, plotData=NULL)
            private$.clearResults()

            requestedVars <- miso_clean_vars(self$options$vars)
            if (length(requestedVars) == 0L) {
                private$.showGuidance(
                    paste(
                        "PCoA displays sites from their pairwise dissimilarities.",
                        "Add two or more numeric Feature variables.",
                        "Results update automatically.",
                        sep="\n"),
                    title="Getting started")
                return()
            }

            analysisData <- self$data
            # Unsupported feature assignments are rejected by the shared
            # preparation seam after this analysis has cleared stale outputs.
            prep <- miso_prepare_resemblance(
                data=analysisData,
                vars=requestedVars,
                factor=self$options$factor,
                transform=self$options$transform,
                distance=self$options$distance,
                seed=0,
                requireFactor=FALSE,
                distBinary=self$options$distBinary)
            if (isTRUE(prep$error)) {
                private$.showGuidance(prep$message)
                return()
            }

            fit <- .misoPcoa(
                prep$dist,
                correction=self$options$correction,
                sqrtDist=self$options$sqrtDist,
                groups=prep$group)
            if (isTRUE(fit$error)) {
                private$.showGuidance(fit$message)
                return()
            }

            private$.state$prep <- prep
            private$.state$pcoa <- fit
            private$.state$plotData <- .misoPreparePcoaPlot(
                fit,
                showCentroids=self$options$showCentroids,
                showSpiders=self$options$showSpiders)
            self$results$ordination$setState(private$.state$plotData)

            private$.populateSites()
            private$.populateCentroids()
            private$.populateEigenvalues()
            private$.populateDescription()
            private$.setWarnings(c(prep$warnings, fit$warnings,
                private$.styleWarning()))
            private$.showSuccessfulResults()
        },

        .clearDisplayResults = function() {
            miso_clear_table(self$results$centroids)
            self$results$centroids$setVisible(FALSE)
        },

        .refreshDisplayOnly = function() {
            if (is.null(private$.state$pcoa)) {
                private$.clearDisplayResults()
                return()
            }
            plotData <- .misoPreparePcoaPlot(
                private$.state$pcoa,
                showCentroids=self$options$showCentroids,
                showSpiders=self$options$showSpiders)
            private$.state$plotData <- plotData
            self$results$ordination$setState(plotData)
            private$.clearDisplayResults()
            private$.populateCentroids()
            private$.showSuccessfulResults()
        },

        .clearResults = function() {
            self$results$guidance$setContent("")
            self$results$warnings$setContent("")
            self$results$ordinationDescription$setContent("")
            for (name in c("sites", "centroids", "eigenvalues"))
                miso_clear_table(self$results[[name]])
            for (name in c(
                    "guidance", "warnings",
                    "ordination", "ordinationDescription", "sites",
                    "centroids", "eigenvalues"))
                self$results[[name]]$setVisible(FALSE)
            for (name in c("sites", "eigenvalues"))
                self$results[[name]]$setVisible(TRUE)
        },

        .showGuidance = function(content, title="Action needed") {
            self$results$guidance$setTitle(title)
            self$results$guidance$setContent(miso_html_block(content))
            self$results$guidance$setVisible(TRUE)
        },

        .showSuccessfulResults = function() {
            self$results$guidance$setVisible(FALSE)
            fit <- private$.state$pcoa
            plotAvailable <- !is.null(private$.state$plotData) &&
                isTRUE(private$.state$plotData$available)
            for (name in c(
                    "sites",
                    "eigenvalues"
                    ))
                self$results[[name]]$setVisible(TRUE)
            showCentroids <-
                !is.null(fit$groups) &&
                (isTRUE(self$options$showCentroids) ||
                    isTRUE(self$options$showSpiders))
            self$results$centroids$setVisible(showCentroids)
            self$results$ordination$setVisible(plotAvailable)
            self$results$ordinationDescription$setVisible(TRUE)
        },


        .setWarnings = function(warnings) {
            warnings <- unique(warnings[!is.na(warnings) & nzchar(warnings)])
            if (length(warnings) == 0L)
                return()
            self$results$warnings$setContent(miso_warning_block(warnings))
            self$results$warnings$setVisible(TRUE)
        },

        .styleWarning = function() {
            fit <- private$.state$pcoa
            if (!is.null(fit$groups) && nlevels(fit$groups) > 64L)
                paste(
                    "Neutral point styling is used because more than 64 groups",
                    "are present; full group identities remain in the tables.")
            else
                character()
        },

        .addRow = function(table, values) {
            name <- table$name
            current <- private$.rowCursors[[name]]
            if (is.null(current)) current <- 0L
            current <- current + 1L
            private$.rowCursors[[name]] <- current
            miso_add_or_set_row(table, as.character(current), values)
        },

        .structuralKey = function() {
            serialize(list(
                vars=self$options$vars,
                factor=self$options$factor,
                transform=self$options$transform,
                distance=self$options$distance,
                distBinary=self$options$distBinary,
                sqrtDist=self$options$sqrtDist,
                correction=self$options$correction,
                dataSignature=miso_data_signature(self$data)), NULL)
        },


        .populateSites = function() {
            prep <- private$.state$prep
            fit <- private$.state$pcoa
            groups <- if (is.null(fit$groups)) {
                rep("", nrow(fit$points))
            } else {
                as.character(fit$groups)
            }
            axis1 <- if (ncol(fit$points) >= 1L) fit$points[, 1L] else
                rep(NA_real_, nrow(fit$points))
            axis2 <- if (ncol(fit$points) >= 2L) fit$points[, 2L] else
                rep(NA_real_, nrow(fit$points))
            for (i in seq_len(nrow(fit$points))) {
                private$.addRow(self$results$sites, list(
                    site=fit$siteNames[[i]],
                    sourceRow=as.integer(prep$rowIndex[[i]]),
                    group=groups[[i]],
                    PCoA1=miso_num_or_na(axis1[[i]]),
                    PCoA2=miso_num_or_na(axis2[[i]])))
            }
            self$results$sites$setNote(
                key="method",
                note=sprintf(
                    "Transformation: %s. Dissimilarity index: %s. Square-root dissimilarities: %s. Additive correction: %s. Axis signs may reverse without changing the ordination.",
                    private$.transformLabel(self$options$transform),
                    private$.distanceLabel(self$options$distance),
                    if (isTRUE(self$options$sqrtDist)) "Yes" else "No",
                    private$.correctionLabel(self$options$correction)),
                init=FALSE)
        },

        .populateCentroids = function() {
            fit <- private$.state$pcoa
            if (is.null(fit$centroids))
                return()
            axis2 <- if (ncol(fit$centroids) >= 2L) {
                fit$centroids[, 2L]
            } else {
                rep(NA_real_, nrow(fit$centroids))
            }
            for (i in seq_len(nrow(fit$centroids))) {
                group <- rownames(fit$centroids)[[i]]
                private$.addRow(self$results$centroids, list(
                    group=group,
                    n=fit$groupSizes[[group]],
                    PCoA1=miso_num_or_na(fit$centroids[i, 1L]),
                    PCoA2=miso_num_or_na(axis2[[i]])))
            }
        },

        .populateEigenvalues = function() {
            fit <- private$.state$pcoa
            for (i in seq_along(fit$eigenvalues)) {
                sign <- if (fit$positive[[i]]) "Positive" else if (
                    fit$negative[[i]]) "Negative" else "Zero"
                private$.addRow(self$results$eigenvalues, list(
                    axis=paste0("PCoA", i),
                    eigenvalue=miso_num_or_na(fit$eigenvalues[[i]]),
                    sign=sign,
                    explained=if (fit$positive[[i]])
                        miso_num_or_na(fit$explained[[i]])
                    else
                        ""))
            }
            self$results$eigenvalues$setNote(
                key="denominator",
                note=sprintf(
                    "Explained percentages are calculated only for positive axes using the sum of positive eigenvalues. Square-root dissimilarities: %s. Additive correction: %s.",
                    if (isTRUE(self$options$sqrtDist)) "Yes" else "No",
                    private$.correctionLabel(self$options$correction)),
                init=FALSE)
        },

        .populateDescription = function() {
            plotData <- private$.state$plotData
            if (!isTRUE(plotData$available)) {
                self$results$ordinationDescription$setContent(miso_html_block(
                    paste(
                        "A two-dimensional plot is unavailable.",
                        "Coordinate and eigenvalue tables retain the fitted result."),
                    ariaLabel="About PCoA ordination",
                    title="Principal coordinates ordination"))
                return()
            }
            self$results$ordinationDescription$setContent(miso_html_block(
                character(0),
                ariaLabel="About PCoA ordination",
                title="Principal coordinates ordination"))
        },



        .overlayStatus = function(requested, rendered, implied=FALSE) {
            if (!isTRUE(requested))
                return("Not requested")

            fit <- private$.state$pcoa
            plotData <- private$.state$plotData
            if (is.null(fit$groups))
                return("Requested; unavailable without a grouping variable")
            if (is.null(plotData) || !isTRUE(plotData$available))
                return(paste(
                    "Requested; omitted because a two-dimensional plot",
                    "is unavailable"))
            if (isTRUE(plotData$neutral))
                return("Requested; omitted because more than 64 groups")
            if (isTRUE(rendered)) {
                if (isTRUE(implied))
                    return("Shown (required for spiders)")
                return("Shown")
            }
            "Requested; omitted from the plot"
        },

        .transformLabel = function(value) {
            switch(value,
                none="None", sqrt="Square root", fourthroot="Fourth root",
                log="Log(x + 1)", pa="Presence/absence",
                wisconsin="Wisconsin", hellinger="Hellinger", total="Total",
                max="Max", frequency="Frequency", normalize="Normalize",
                range="Range", standardize="Standardize",
                chi.square="Chi-square", rclr="Reverse CLR", value)
        },

        .distanceLabel = function(value) {
            switch(value,
                bray="Bray-Curtis", jaccard="Jaccard",
                euclidean="Euclidean", manhattan="Manhattan",
                canberra="Canberra", kulczynski="Kulczynski",
                gower="Gower", morisita="Morisita",
                horn="Horn-Morisita", mountford="Mountford",
                raup="Raup-Crick", binomial="Binomial", chao="Chao",
                cao="Cao", clark="Clark", altGower="Alternative Gower",
                mahalanobis="Mahalanobis", value)
        },

        .correctionLabel = function(value) {
            switch(value, none="None", lingoes="Lingoes",
                cailliez="Cailliez", value)
        },

        .plotPcoa = function(image, ...) {
            plot <- .misoBuildPcoaPlot(image$state)
            if (is.null(plot))
                return()
            suppressWarnings(print(plot))
            invisible(TRUE)
        }
    )
)
