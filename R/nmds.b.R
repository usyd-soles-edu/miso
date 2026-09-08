
# This file is a generated template, your changes will not be overwritten

nmdsClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "nmdsClass",
    inherit = nmdsBase,
    private = list(
        .state = list(),

        .run = function() {
            private$.resetResults()

            requestedVars <- miso_clean_vars(self$options$vars)
            if (length(requestedVars) < 2L) {
                if (length(requestedVars) == 0L) {
                    private$.showGuidance(
                        "Getting started",
                        "Add at least two numeric Feature variables.")
                } else {
                    private$.showGuidance(
                        "Action needed",
                        "Add at least two usable numeric Feature variables.")
                }
                return()
            }

            requestedK <- suppressWarnings(as.integer(self$options$nmdsK))
            if (! requestedK %in% c(2L, 3L)) {
                private$.showGuidance("Action needed", sprintf(
                    "Dimensions must be 2 or the retained legacy value 3; received %s.",
                    as.character(self$options$nmdsK)))
                return()
            }

            preparationDistance <- if (
                    self$options$transform %in% c("standardize", "rclr") &&
                    ! self$options$distance %in% private$.signedDistanceMethods()) {
                "euclidean"
            } else {
                self$options$distance
            }

            prep <- miso_prepare_resemblance(
                data=self$data,
                vars=requestedVars,
                transform=self$options$transform,
                distance=preparationDistance,
                seed=self$options$seed,
                requireFactor=FALSE,
                distBinary=FALSE)
            if (prep$error) {
                if (grepl("Too few samples", prep$message, fixed=TRUE)) {
                    private$.showGuidance("Action needed", sprintf(
                        "A %d-dimensional nMDS needs at least %d usable sites. Check missing feature values and Feature variable assignments.",
                        requestedK, requestedK + 1L))
                } else if (grepl("Transformation", prep$message, ignore.case=TRUE)) {
                    private$.showGuidance("Action needed", sprintf(
                        "The %s transformation did not produce usable finite values. Check empty samples and feature variation. Technical detail: %s",
                        private$.transformLabel(self$options$transform), prep$message))
                } else {
                    private$.showGuidance("Action needed", prep$message)
                }
                return()
            }

            if (prep$varsUsed < 2L) {
                private$.showGuidance(
                    "Action needed",
                    "Add at least two usable numeric Feature variables.")
                return()
            }

            minimumSites <- requestedK + 1L
            if (prep$rowsUsed < minimumSites) {
                private$.showGuidance("Action needed", sprintf(
                    "A %d-dimensional nMDS needs at least %d usable sites. Check missing feature values and Feature variable assignments.",
                    requestedK, minimumSites))
                return()
            }

            checkedDistance <- private$.validateDistance(
                prep$transformed, self$options$distance)
            if (checkedDistance$error) {
                private$.showGuidance("Action needed", checkedDistance$message)
                return()
            }
            prep$dist <- checkedDistance$distance

            private$.state$prep <- prep
            private$.state$effectiveK <- requestedK
            private$.state$warnings <- unique(c(
                prep$warnings, checkedDistance$warnings))
            if (isTRUE(self$options$distBinary))
                private$.state$warnings <- c(
                    private$.state$warnings,
                    paste(
                        "The legacy Binary option was requested but is ignored;",
                        "the ordination uses the selected dissimilarity without",
                        "automatic presence/absence conversion. Choose the",
                        "Presence/absence transformation when that preprocessing is intended."))
            if (prep$rowsUsed <= 2L * requestedK + 1L)
                private$.state$warnings <- c(private$.state$warnings, sprintf(
                    "Only %d sites were retained for %d dimensions; stress may be artificially low or uninformative and is not rated as good or excellent.",
                    prep$rowsUsed, requestedK))

            private$.state$fitArguments <- list(
                distance=self$options$distance,
                k=requestedK,
                trymax=as.integer(self$options$nmdsTrymax),
                maxit=as.integer(self$options$nmdsMaxit),
                wascores=isTRUE(self$options$nmdsSpecies),
                autotransform=FALSE,
                trace=FALSE)
            miso_set_seed(prep)
            fit <- tryCatch(withCallingHandlers(
                do.call(
                    vegan::metaMDS,
                    c(list(comm=prep$transformed), private$.state$fitArguments)),
                warning=function(w) {
                    private$.state$warnings <- c(
                        private$.state$warnings, conditionMessage(w))
                    invokeRestart("muffleWarning")
                }), error=function(e) e)
            private$.state$postFitRng <- if (
                exists(".Random.seed", envir=.GlobalEnv, inherits=FALSE)) {
                get(".Random.seed", envir=.GlobalEnv, inherits=FALSE)
            } else {
                NULL
            }
            if (inherits(fit, "error")) {
                private$.showGuidance("nMDS could not find a solution", sprintf(
                    "nMDS could not find a solution. Check feature variation and the selected settings. Technical detail: %s",
                    conditionMessage(fit)))
                return()
            }

            sites <- tryCatch(
                vegan::scores(fit, display="sites", choices=seq_len(requestedK)),
                error=function(e) e)
            if (inherits(sites, "error") || is.null(sites)) {
                detail <- if (inherits(sites, "error"))
                    conditionMessage(sites)
                else
                    "site scores were unavailable"
                private$.showGuidance("nMDS could not find a solution", sprintf(
                    "nMDS could not find a solution. Check feature variation and the selected settings. Technical detail: %s",
                    detail))
                return()
            }

            features <- if (isTRUE(self$options$nmdsSpecies)) {
                tryCatch(
                    vegan::scores(fit, display="species", choices=seq_len(requestedK)),
                    error=function(e) NULL)
            } else {
                NULL
            }
            private$.state$fit <- fit
            private$.state$sites <- as.matrix(sites)
            private$.state$features <- if (is.null(features)) NULL else as.matrix(features)
            repeats <- private$.finiteInteger(fit$converged)
            if (!is.na(repeats) && repeats < 1L)
                private$.state$warnings <- c(
                    private$.state$warnings,
                    paste(
                        "No similar best solution was repeated; the configuration",
                        "may be locally unstable. Consider increasing Maximum random",
                        "starts and comparing solutions."))
            if (identical(fit$engine, "monoMDS") &&
                    identical(private$.finiteInteger(fit$icause), 1L))
                private$.state$warnings <- c(
                    private$.state$warnings,
                    paste(
                        "The retained optimization reached the maximum iterations;",
                        "consider increasing Maximum iterations per start."))
            private$.prepareGroup(prep)
            private$.prepareOverlays()
            private$.prepareEnvironmental(prep)

            private$.state$shepardData <- private$.shepardData()
            private$.state$shepardValid <-
                ! is.null(private$.state$shepardData)
            if (private$.state$shepardValid) {
                private$.state$shepardDisplayData <-
                    private$.capShepardData(private$.state$shepardData)
                private$.state$shepardTotal <-
                    nrow(private$.state$shepardData)
                private$.state$shepardDisplayed <-
                    nrow(private$.state$shepardDisplayData)
            }
            if (isTRUE(self$options$nmdsShepard) &&
                    ! private$.state$shepardValid)
                private$.state$warnings <- c(
                    private$.state$warnings,
                    paste(
                        "The Shepard diagram was requested but was not shown",
                        "because finite dissimilarity and ordination-distance",
                        "values were unavailable."))
            private$.populateSummary()
            private$.populatePurposes()
            private$.populateCoreResults()
            self$results$ordination$setState(private$.nmdsPlotData())
            self$results$shepard$setState(private$.shepardPlotData())
            showShepard <- isTRUE(self$options$nmdsShepard) &&
                private$.state$shepardValid
            private$.showSuccessfulResults(
                showShepard=showShepard,
                showEnv=length(private$.state$envRows) > 0L,
                showFeatures=! is.null(private$.state$features))
        },

        .htmlEscape = function(value) {
            miso_html_escape(value)
        },

        .htmlBlock = function(paragraphs, ariaLabel=NULL, title=NULL) {
            miso_html_block(
                paragraphs,
                ariaLabel=ariaLabel,
                title=title)
        },

        .distanceLabel = function(method) {
            labels <- c(
                bray="Bray-Curtis", jaccard="Jaccard", euclidean="Euclidean",
                manhattan="Manhattan", canberra="Canberra", kulczynski="Kulczynski",
                gower="Gower", morisita="Morisita", horn="Horn-Morisita",
                mountford="Mountford", raup="Raup-Crick", binomial="Binomial",
                chao="Chao", cao="Cao", clark="Clark",
                altGower="Alternative Gower", mahalanobis="Mahalanobis")
            if (method %in% names(labels))
                unname(labels[[method]])
            else
                as.character(method)
        },

        .transformLabel = function(method) {
            labels <- c(
                none="None", sqrt="Square root", fourthroot="Fourth root",
                log="Log(x + 1)", pa="Presence/absence", wisconsin="Wisconsin",
                hellinger="Hellinger", total="Total", max="Max",
                frequency="Frequency", normalize="Normalize", range="Range",
                standardize="Standardize", chi.square="Chi-square", rclr="Reverse CLR")
            if (method %in% names(labels))
                unname(labels[[method]])
            else
                as.character(method)
        },

        .signedDistanceMethods = function() {
            c("euclidean", "manhattan", "canberra", "gower", "mahalanobis")
        },

        .finiteInteger = function(value) {
            if (is.null(value) || length(value) == 0L)
                return(NA_integer_)
            value <- suppressWarnings(as.integer(value[[1L]]))
            if (length(value) == 0L || is.na(value))
                NA_integer_
            else
                value
        },

        .finiteNumber = function(value) {
            if (is.null(value) || length(value) == 0L)
                return(NA_real_)
            value <- suppressWarnings(as.numeric(value[[1L]]))
            if (length(value) == 0L || ! is.finite(value))
                NA_real_
            else
                value
        },

        .fitValue = function(fit, name, fallback="Unavailable") {
            value <- fit[[name]]
            if (is.null(value) || length(value) == 0L)
                return(fallback)
            value <- value[[1L]]
            if (length(value) == 0L || is.na(value) ||
                    (is.numeric(value) && ! is.finite(value)))
                return(fallback)
            as.character(value)
        },

        .stoppingReason = function(fit) {
            if (! identical(fit$engine, "monoMDS"))
                return("Not reported by this engine")
            switch(as.character(private$.finiteInteger(fit$icause)),
                `1`="Maximum iterations reached",
                `2`="Stress nearly zero",
                `3`="Stress nearly unchanged",
                `4`="Gradient scale factor nearly zero",
                "Unavailable")
        },

        .searchStatus = function(fit) {
            repeats <- private$.finiteInteger(fit$converged)
            if (is.na(repeats))
                "Unavailable"
            else if (repeats < 1L)
                "No similar best solution was repeated; consider more starts"
            else
                sprintf("A similar best solution was repeated %d time(s)", repeats)
        },

        .bestStart = function(fit) {
            best <- private$.finiteInteger(fit$bestry)
            if (is.na(best))
                "Unavailable"
            else if (best == 0L)
                "Initial configuration"
            else
                sprintf("Start %d", best)
        },

        .stressGuide = function(fit, prep, k) {
            if (prep$rowsUsed <= 2L * k + 1L)
                return(paste(
                    "Not rated because the retained sample is too small for a",
                    "reliable stress label"))
            stress <- private$.finiteNumber(fit$stress)
            if (is.na(stress))
                return("Unavailable because fitted stress was not finite")
            if (stress < 0.1)
                return(paste(
                    "Often considered a good representation, but stress is only",
                    "one diagnostic and thresholds are context dependent"))
            if (stress < 0.2)
                return(paste(
                    "Requires care; stress is only one diagnostic and thresholds",
                    "are context dependent"))
            paste(
                "Can indicate a poor representation; stress is only one diagnostic",
                "and thresholds are context dependent")
        },

        .yesNo = function(value) {
            if (isTRUE(value)) "Yes" else "No"
        },

        .seedLabel = function(prep) {
            if (is.na(prep$seed))
                "Random (0)"
            else
                sprintf("Fixed (%d)", prep$seed)
        },

        .groupAssignmentSummary = function() {
            requested <- miso_clean_vars(self$options$factor)
            if (length(requested) == 0L)
                return("None")
            name <- requested[[1L]]
            if (is.null(private$.state$groupLabels))
                return(sprintf("%s (unavailable)", name))
            unassigned <- sum(private$.state$groupMissing)
            assigned <- length(private$.state$groupLabels) - unassigned
            sprintf(
                "%s (%d assigned; %d unassigned)",
                name, assigned, unassigned)
        },

        .environmentRequestSummary = function() {
            requested <- unique(miso_clean_vars(self$options$nmdsEnv))
            if (length(requested) == 0L)
                "None"
            else
                sprintf(
                    "%s (%d requested)",
                    paste(requested, collapse=", "),
                    length(requested))
        },

    .clearTable = function(table) {
        miso_clear_table(table)
        # jmvcore::Table$deleteRows() does not clear its cached row names.
        table$.__enclos_env__$private$.rowNames <- character()
    },

        .addSummary = function(label, value) {
            key <- as.character(length(self$results$summary$rowKeys) + 1L)
            self$results$summary$addRow(
                rowKey=key,
                values=list(item=label, value=as.character(value)))
        },

        .addSetting = function(label, value) {
            key <- as.character(length(self$results$settings$rowKeys) + 1L)
            self$results$settings$addRow(
                rowKey=key,
                values=list(setting=label, value=as.character(value)))
        },

        .populateSummary = function() {
            prep <- private$.state$prep
            private$.clearTable(self$results$summary)
            private$.addSummary("Core samples used", prep$rowsUsed)
            private$.addSummary("Feature variables used", prep$varsUsed)
            private$.addSummary(
                "Rows excluded: missing feature values",
                prep$rowsMissingExcluded)
            private$.addSummary(
                "Rows excluded: all-zero feature values",
                prep$rowsZeroExcluded)
            private$.addSummary(
                "All-zero feature variables excluded",
                prep$featuresZeroExcluded)
            private$.addSummary(
                "Transformation",
                private$.transformLabel(self$options$transform))
            private$.addSummary(
                "Dissimilarity index",
                private$.distanceLabel(self$options$distance))
            private$.addSummary(
                "Effective dimensions",
                private$.state$effectiveK)
            private$.addSummary(
                "Grouping assignment",
                private$.groupAssignmentSummary())
            private$.addSummary(
                "Environmental variables requested",
                private$.environmentRequestSummary())
            private$.addSummary("Seed", private$.seedLabel(prep))
        },

        .selectPlotLabels = function(points, labels, maxLabels=12L) {
            points <- as.matrix(points)
            labels <- as.character(labels)
            if (nrow(points) != length(labels) || ncol(points) < 2L)
                stop("Label points and labels are not aligned")
            valid <- is.finite(points[, 1L]) &
                is.finite(points[, 2L]) & nzchar(labels)
            candidates <- which(valid)
            if (length(candidates) == 0L)
                return(list(
                    shown=integer(), omitted=seq_along(labels)))
            squaredDistance <- rowSums(
                points[candidates, 1:2, drop=FALSE]^2)
            candidates <- candidates[order(-squaredDistance, candidates)]
            xRange <- diff(range(points[valid, 1L], finite=TRUE))
            yRange <- diff(range(points[valid, 2L], finite=TRUE))
            if (!is.finite(xRange) || xRange <= 0)
                xRange <- 1
            if (!is.finite(yRange) || yRange <= 0)
                yRange <- 1
            chosen <- integer()
            boxes <- list()
            for (index in candidates) {
                halfWidth <- xRange * max(
                    0.025,
                    0.006 * nchar(labels[[index]], type="width"))
                halfHeight <- yRange * 0.025
                box <- c(
                    left=unname(points[index, 1L] - halfWidth),
                    right=unname(points[index, 1L] + halfWidth),
                    bottom=unname(points[index, 2L] - halfHeight),
                    top=unname(points[index, 2L] + halfHeight))
                overlaps <- vapply(
                    boxes,
                    function(existing)
                        box[["left"]] < existing[["right"]] &&
                        box[["right"]] > existing[["left"]] &&
                        box[["bottom"]] < existing[["top"]] &&
                        box[["top"]] > existing[["bottom"]],
                    logical(1))
                if (!any(overlaps)) {
                    chosen <- c(chosen, index)
                    boxes[[length(boxes) + 1L]] <- box
                }
                if (length(chosen) >= as.integer(maxLabels))
                    break
            }
            list(
                shown=chosen,
                omitted=setdiff(seq_along(labels), chosen))
        },

        .shepardData = function() {
            fit <- private$.state$fit
            prep <- private$.state$prep
            sites <- private$.state$sites
            k <- private$.state$effectiveK
            stress <- private$.finiteNumber(fit$stress)
            if (is.na(stress) || is.null(prep$dist) || is.null(sites) ||
                    is.null(k) || ncol(sites) < k)
                return(NULL)
            dissimilarities <- as.numeric(prep$dist)
            ordinationDistances <- as.numeric(stats::dist(
                sites[, seq_len(k), drop=FALSE]))
            validDistances <- length(dissimilarities) > 0L &&
                length(dissimilarities) == length(ordinationDistances) &&
                all(is.finite(dissimilarities)) &&
                all(is.finite(ordinationDistances))
            if (! validDistances)
                return(NULL)

            observed <- as.numeric(fit$diss)
            fittedDistances <- as.numeric(fit$dist)
            monotonicFit <- as.numeric(fit$dhat)
            lengths <- c(
                length(observed), length(fittedDistances),
                length(monotonicFit))
            expectedLength <- choose(nrow(sites), 2L)
            if (expectedLength <= 0L || any(lengths != expectedLength) ||
                    any(! is.finite(observed)) ||
                    any(! is.finite(fittedDistances)) ||
                    any(! is.finite(monotonicFit)))
                return(NULL)
            data.frame(
                dissimilarity=observed,
                ordinationDistance=fittedDistances,
                monotonicFit=monotonicFit)
        },

        .validateShepard = function() {
            ! is.null(private$.shepardData())
        },

        .capShepardData = function(data, maximum=1000L) {
            if (is.null(data) || nrow(data) <= maximum)
                return(data)
            orderIndex <- order(
                data$dissimilarity,
                data$ordinationDistance,
                data$monotonicFit,
                method="radix")
            positions <- unique(as.integer(round(seq(
                1L, length(orderIndex), length.out=maximum))))
            data[orderIndex[positions], , drop=FALSE]
        },

        .validateDistance = function(transformed, method) {
            if (any(transformed < 0, na.rm=TRUE) &&
                    ! method %in% private$.signedDistanceMethods())
                return(list(error=TRUE, message=sprintf(
                    "%s cannot be used with negative values produced by %s. Choose Euclidean, Manhattan, Canberra, Gower, or Mahalanobis.",
                    private$.distanceLabel(method),
                    private$.transformLabel(self$options$transform))))

            warnings <- character()
            distance <- tryCatch(withCallingHandlers(
                vegan::vegdist(transformed, method=method, binary=FALSE),
                warning=function(w) {
                    warnings <<- c(warnings, conditionMessage(w))
                    invokeRestart("muffleWarning")
                }), error=function(e) e)
            if (inherits(distance, "error"))
                return(list(
                    error=TRUE,
                    message=paste0("Could not compute dissimilarities: ",
                                   conditionMessage(distance))))
            values <- as.numeric(distance)
            if (length(values) == 0L || any(! is.finite(values)) ||
                    any(values < 0) ||
                    length(unique(signif(values, 14))) < 2L)
                return(list(
                    error=TRUE,
                    message=paste(
                        "The selected settings produced non-finite, negative, or constant dissimilarities.",
                        "Check empty samples, the transformation, and feature variation.")))
            list(
                error=FALSE,
                distance=distance,
                warnings=unique(warnings))
        },

        .resetResults = function() {
            private$.state <- list(
                warnings=character(), fit=NULL, prep=NULL, sites=NULL,
                features=NULL, group=NULL, groupLabels=NULL,
                groupMissing=NULL, groupVariable=NULL,
                assignedGroupLevels=character(), envRows=list(),
                vectorEndpoints=NULL, overlays=list(), shepardData=NULL,
                shepardDisplayData=NULL, shepardTotal=0L,
                shepardDisplayed=0L, shepardValid=FALSE,
                effectiveK=NULL, featureLabelsShown=character(),
                featureLabelsOmitted=character(),
                featureLabelSelection=list(shown=integer(), omitted=integer()),
                vectorLabelsShown=character(),
                vectorLabelsOmitted=character(),
                vectorLabelSelection=list(shown=integer(), omitted=integer()),
                postFitRng=NULL,
                fitArguments=NULL)
            self$results$guidance$setContent("")
            self$results$warnings$setContent("")
            self$results$ordinationDescription$setContent("")
            self$results$shepardDescription$setContent("")
            self$results$note$setContent("")
            for (name in c(
                    "summaryPurpose", "sitesPurpose", "stressPurpose",
                    "shepardPairsPurpose", "envfitPurpose", "featuresPurpose",
                    "settingsPurpose"))
                self$results[[name]]$setContent("")
            private$.clearTable(self$results$summary)
            private$.clearTable(self$results$stress)
            private$.clearTable(self$results$shepardPairs)
            private$.clearTable(self$results$envfit)
            self$results$envfit$setNote(
                key="interpretation",
                note="")
            private$.clearTable(self$results$sites)
            private$.clearTable(self$results$features)
            private$.clearTable(self$results$settings)
            for (name in c(
                    "guidance", "summary", "summaryPurpose", "warnings",
                    "ordination", "ordinationDescription", "stress",
                    "stressPurpose", "shepard", "shepardDescription",
                    "shepardPairs", "shepardPairsPurpose", "envfit",
                    "envfitPurpose", "note", "sites", "sitesPurpose",
                    "features", "featuresPurpose", "settings",
                    "settingsPurpose"))
                self$results[[name]]$setVisible(FALSE)
        },

        .showGuidance = function(title, paragraphs) {
            self$results$guidance$setTitle(title)
            self$results$guidance$setContent(
                private$.htmlBlock(paragraphs))
            self$results$guidance$setVisible(TRUE)
        },

        .showSuccessfulResults = function(showShepard, showEnv, showFeatures) {
            hasWarnings <- length(private$.state$warnings) > 0L
            if (hasWarnings)
                self$results$warnings$setContent(miso_warning_block(
                    unique(private$.state$warnings)))
            for (name in c(
                    "summary", "summaryPurpose", "ordination",
                    "ordinationDescription", "stress", "stressPurpose", "note",
                    "sites", "sitesPurpose", "settings", "settingsPurpose"))
                self$results[[name]]$setVisible(TRUE)
            self$results$warnings$setVisible(hasWarnings)
            self$results$shepard$setVisible(showShepard)
            self$results$shepardDescription$setVisible(showShepard)
            self$results$shepardPairs$setVisible(showShepard)
            self$results$shepardPairsPurpose$setVisible(showShepard)
            self$results$envfit$setVisible(showEnv)
            self$results$envfitPurpose$setVisible(showEnv)
            self$results$features$setVisible(showFeatures)
            self$results$featuresPurpose$setVisible(showFeatures)
        },

        .populatePurposes = function() {
            miso_populate_purposes(self$results, list(
                summaryPurpose=c(
                    "Data summary",
                    "Summarises included samples and features, including any exclusions."),
                sitesPurpose=c(
                    "Site scores",
                    "Lists plotted sample coordinates for identification or reuse."),
                stressPurpose=c(
                    "Stress and convergence diagnostics",
                    "Reports ordination stress and convergence."),
                shepardPairsPurpose=c(
                    "Shepard diagram values",
                    "Lists the values represented in the Shepard diagram."),
                envfitPurpose=c(
                    "Environmental fit",
                    "Shows associations between environmental variables and the ordination."),
                featuresPurpose=c(
                    "Feature scores",
                    "Shows each feature's weighted-average position in the ordination."),
                settingsPurpose=c(
                    "Analysis settings",
                    "Lists the options used for this analysis.")))
        },

        .prepareGroup = function(prep) {
            groupName <- miso_clean_vars(self$options$factor)
            if (length(groupName) == 0L)
                return()
            groupName <- groupName[[1L]]
            if (! groupName %in% names(self$data)) {
                private$.state$warnings <- c(
                    private$.state$warnings,
                    sprintf(
                        "Grouping variable '%s' was not found; the base ordination is still shown.",
                        groupName))
                return()
            }

            raw <- self$data[[groupName]][prep$rowIndex]
            values <- as.character(raw)
            missing <- is.na(values) | trimws(values) == ""
            missingLabel <- "Unassigned (missing)"
            observedCollision <- !missing & values == missingLabel
            if (any(observedCollision)) {
                observedLabel <- paste0(missingLabel, " (observed level)")
                suffix <- 2L
                while (observedLabel %in% values[!missing & !observedCollision]) {
                    observedLabel <- sprintf(
                        "%s (observed level %d)", missingLabel, suffix)
                    suffix <- suffix + 1L
                }
                values[observedCollision] <- observedLabel
            }
            values[missing] <- missingLabel
            private$.state$groupLabels <- values
            private$.state$groupMissing <- missing
            private$.state$groupVariable <- groupName

            assignedGroups <- unique(values[!missing])
            private$.state$assignedGroupLevels <- assignedGroups
            if (length(assignedGroups) < 2L) {
                private$.state$warnings <- c(
                    private$.state$warnings,
                    sprintf(
                        "Grouping variable '%s' has fewer than two assigned groups; group styling and overlays were not shown.",
                        groupName))
                return()
            }
            groupLevels <- c(
                assignedGroups,
                if (any(missing)) missingLabel else character())
            private$.state$group <- factor(values, levels=groupLevels)
        },

        .overlayRequests = function() {
            c(
                points=isTRUE(self$options$nmdsOverlay),
                hull=isTRUE(self$options$nmdsHull),
                ellipse=isTRUE(self$options$nmdsEllipse),
                spider=isTRUE(self$options$nmdsSpider))
        },

        .groupStyles = function(levels, includeMissing=FALSE) {
            aesthetics <- .misoGroupAesthetics(levels)
            styles <- data.frame(
                group=levels,
                colour=unname(aesthetics$colour[levels]),
                shape=as.integer(unname(aesthetics$shape[levels])),
                lineType=unname(aesthetics$linetype[levels]),
                assigned=TRUE,
                stringsAsFactors=FALSE)
            if (isTRUE(includeMissing))
                styles <- rbind(
                    styles,
                    data.frame(
                        group="Unassigned (missing)",
                        colour="#7F7F7F",
                        shape=1L,
                        lineType="solid",
                        assigned=FALSE,
                        stringsAsFactors=FALSE))
            rownames(styles) <- NULL
            styles
        },

        .ellipsePoints = function(item, n=101L) {
            if (is.null(item$cov) || is.null(item$center) ||
                    is.null(item$scale))
                return(NULL)
            covariance <- as.matrix(item$cov)
            center <- as.numeric(item$center)
            scale <- as.numeric(item$scale[[1L]])
            if (! identical(dim(covariance), c(2L, 2L)) ||
                    length(center) != 2L || any(! is.finite(covariance)) ||
                    any(! is.finite(center)) || length(scale) != 1L ||
                    ! is.finite(scale) || scale < 0)
                return(NULL)
            decomposition <- tryCatch(
                eigen(covariance, symmetric=TRUE),
                error=function(e) NULL)
            if (is.null(decomposition))
                return(NULL)
            radii <- sqrt(pmax(decomposition$values, 0)) * scale
            if (all(radii == 0))
                return(NULL)
            theta <- seq(0, 2 * pi, length.out=as.integer(n))
            unitCircle <- rbind(cos(theta), sin(theta))
            points <- t(decomposition$vectors %*%
                diag(radii, nrow=2L, ncol=2L) %*% unitCircle)
            points <- sweep(points, 2L, center, "+")
            if (any(! is.finite(points)))
                return(NULL)
            points
        },

        .overlayWarning = function(layer, group=NULL, detail=NULL) {
            label <- switch(layer,
                points="point styling",
                hull="hull layer",
                ellipse="ellipse layer",
                spider="spider layer")
            subject <- if (is.null(group))
                label
            else
                sprintf("%s for group '%s'", label, group)
            message <- if (is.null(detail))
                sprintf("The requested %s was not shown.", subject)
            else
                sprintf("The requested %s was not shown because %s.", subject, detail)
            private$.state$warnings <- c(private$.state$warnings, message)
        },

        .prepareOverlays = function() {
            requested <- private$.overlayRequests()
            overlays <- list(
                requested=requested,
                effective=setNames(rep(FALSE, length(requested)), names(requested)),
                styles=data.frame(
                    group=character(), colour=character(), shape=integer(),
                    lineType=character(), assigned=logical(),
                    stringsAsFactors=FALSE),
                hull=list(), ellipse=list(), spider=list(),
                lineTypes=c(hull=1L, ellipse=2L, spider=3L),
                lineWidths=if (requested[["points"]])
                    c(hull=1.5, ellipse=1.5, spider=1)
                else
                    c(hull=2.5, ellipse=1.5, spider=0.75))
            private$.state$overlays <- overlays

            groupLabels <- private$.state$groupLabels
            if (is.null(groupLabels))
                return()

            assignedLevels <- private$.state$assignedGroupLevels
            if (length(assignedLevels) < 2L || is.null(private$.state$group)) {
                for (layer in names(requested)[requested])
                    private$.overlayWarning(
                        layer,
                        detail="the grouping variable has fewer than two assigned groups")
                return()
            }

            if (length(assignedLevels) > 64L) {
                private$.state$warnings <- c(
                    private$.state$warnings,
                    sprintf(
                        paste(
                            "Grouping variable '%s' has %d assigned groups;",
                            "group styling and overlays were not shown because",
                            "at most 64 distinguishable group styles are available."),
                        private$.state$groupVariable,
                        length(assignedLevels)))
                return()
            }

            missing <- private$.state$groupMissing
            overlays$styles <- private$.groupStyles(
                assignedLevels,
                includeMissing=any(missing))
            overlays$effective[["points"]] <- requested[["points"]]
            sites <- private$.state$sites[, 1:2, drop=FALSE]

            if (requested[["hull"]]) {
                for (level in assignedLevels) {
                    coordinates <- sites[!missing & groupLabels == level, , drop=FALSE]
                    distinct <- unique(as.data.frame(coordinates))
                    hasArea <- nrow(distinct) >= 3L &&
                        qr(sweep(as.matrix(distinct), 2L, as.matrix(distinct)[1L, ], "-"))$rank >= 2L
                    if (! hasArea) {
                        private$.overlayWarning(
                            "hull", level,
                            "fewer than three non-collinear sites were available")
                        next
                    }
                    indices <- grDevices::chull(coordinates[, 1L], coordinates[, 2L])
                    geometry <- coordinates[c(indices, indices[[1L]]), , drop=FALSE]
                    if (all(is.finite(geometry)))
                        overlays$hull[[level]] <- geometry
                    else
                        private$.overlayWarning(
                            "hull", level, "its boundary coordinates were not finite")
                }
                overlays$effective[["hull"]] <- length(overlays$hull) > 0L
            }

            if (requested[["spider"]]) {
                for (level in assignedLevels) {
                    coordinates <- sites[!missing & groupLabels == level, , drop=FALSE]
                    if (nrow(coordinates) < 1L || any(! is.finite(coordinates))) {
                        private$.overlayWarning(
                            "spider", level, "no finite assigned sites were available")
                        next
                    }
                    centre <- colMeans(coordinates)
                    overlays$spider[[level]] <- list(
                        centre=unname(centre),
                        segments=cbind(
                            x0=rep(centre[[1L]], nrow(coordinates)),
                            y0=rep(centre[[2L]], nrow(coordinates)),
                            x1=coordinates[, 1L],
                            y1=coordinates[, 2L]))
                }
                overlays$effective[["spider"]] <- length(overlays$spider) > 0L
            }

            if (requested[["ellipse"]]) {
                ellipseGroup <- factor(
                    ifelse(missing, NA_character_, groupLabels),
                    levels=assignedLevels)
                ellipseWarnings <- character()
                ellipseItems <- tryCatch(withCallingHandlers(
                    vegan::ordiellipse(
                        private$.state$fit,
                        ellipseGroup,
                        kind="sd",
                        draw="none"),
                    warning=function(w) {
                        ellipseWarnings <<- c(
                            ellipseWarnings, conditionMessage(w))
                        invokeRestart("muffleWarning")
                    }), error=function(e) e)
                if (inherits(ellipseItems, "error")) {
                    private$.overlayWarning(
                        "ellipse",
                        detail=paste0(
                            "the geometry could not be estimated: ",
                            conditionMessage(ellipseItems)))
                } else {
                    for (level in assignedLevels) {
                        geometry <- private$.ellipsePoints(
                            ellipseItems[[level]])
                        if (is.null(geometry)) {
                            private$.overlayWarning(
                                "ellipse", level,
                                "a finite one-standard-deviation ellipse could not be estimated")
                            next
                        }
                        overlays$ellipse[[level]] <- geometry
                    }
                    for (detail in unique(ellipseWarnings))
                        private$.state$warnings <- c(
                            private$.state$warnings,
                            sprintf("Ellipse layer: %s", detail))
                }
                overlays$effective[["ellipse"]] <- length(overlays$ellipse) > 0L
            }

            private$.state$overlays <- overlays
        },

        .fitEnvironmentalVariable = function(name, sites, prep, rngState) {
            if (! name %in% names(self$data))
                return(list(error=sprintf(
                    "'%s' is not present in the data", name)))

            values <- self$data[[name]][prep$rowIndex]
            if (! is.numeric(values))
                return(list(error=sprintf(
                    "'%s' must be numeric", name)))

            keep <- is.finite(values)
            k <- private$.state$effectiveK
            minimumSites <- k + 2L
            if (sum(keep) < minimumSites)
                return(list(error=sprintf(
                    "'%s' has fewer than %d usable sites",
                    name, minimumSites)))

            variation <- stats::var(values[keep])
            if (! is.finite(variation) || variation <= 0)
                return(list(error=sprintf(
                    "'%s' has no usable variation", name)))

            if (! is.null(rngState))
                assign(".Random.seed", rngState, envir=.GlobalEnv)

            fitWarnings <- character()
            fit <- tryCatch(withCallingHandlers(
                vegan::envfit(
                    sites[keep, seq_len(k), drop=FALSE],
                    data.frame(
                        value=values[keep],
                        row.names=prep$rowIndex[keep]),
                    permutations=as.integer(self$options$nmdsEnvPerm),
                    choices=seq_len(k)),
                warning=function(w) {
                    fitWarnings <<- c(fitWarnings, conditionMessage(w))
                    invokeRestart("muffleWarning")
                }), error=function(e) e)
            if (inherits(fit, "error"))
                return(list(error=sprintf(
                    "'%s' could not be fitted: %s",
                    name, conditionMessage(fit))))

            endpoint <- tryCatch(
                vegan::scores(
                    fit, display="vectors", choices=seq_len(k)),
                error=function(e) e)
            if (inherits(endpoint, "error"))
                return(list(error=sprintf(
                    "'%s' vector coordinates were unavailable: %s",
                    name, conditionMessage(endpoint))))
            endpoint <- as.numeric(endpoint[1L, seq_len(k), drop=TRUE])
            if (length(endpoint) != k || any(! is.finite(endpoint)))
                return(list(error=sprintf(
                    "'%s' did not produce finite vector coordinates", name)))

            r2 <- private$.finiteNumber(fit$vectors$r[[1L]])
            p <- private$.finiteNumber(fit$vectors$pvals[[1L]])
            permutations <- private$.finiteInteger(
                fit$vectors$permutations)
            unavailable <- character()
            if (is.na(r2))
                unavailable <- c(unavailable, "r-squared")
            if (is.na(p))
                unavailable <- c(unavailable, "permutation p")
            if (is.na(permutations))
                unavailable <- c(unavailable, "evaluated permutations")

            list(
                error=NULL,
                name=name,
                r2=r2,
                p=p,
                samples=as.integer(sum(keep)),
                permutations=permutations,
                endpoint=endpoint,
                warnings=unique(c(
                    fitWarnings,
                    if (length(unavailable) > 0L) sprintf(
                        "'%s' returned unavailable %s; affected cells are blank",
                        name, paste(unavailable, collapse=", "))
                    else
                        character())))
        },

        .prepareEnvironmental = function(prep) {
            requested <- unique(miso_clean_vars(self$options$nmdsEnv))
            if (length(requested) == 0L)
                return()

            sites <- private$.state$sites
            rngState <- private$.state$postFitRng
            rows <- list()
            for (name in requested) {
                fitted <- private$.fitEnvironmentalVariable(
                    name, sites, prep, rngState)
                if (! is.null(fitted$error)) {
                    private$.state$warnings <- c(
                        private$.state$warnings,
                        paste0(
                            "Environmental fit: ", fitted$error,
                            ". The base ordination is unchanged."))
                    next
                }
                if (length(fitted$warnings) > 0L)
                    private$.state$warnings <- c(
                        private$.state$warnings,
                        paste0("Environmental fit: ", fitted$warnings))
                rows[[length(rows) + 1L]] <- fitted
            }

            if (length(rows) == 0L)
                return()

            endpoints <- do.call(rbind, lapply(rows, `[[`, "endpoint"))
            rownames(endpoints) <- vapply(rows, `[[`, character(1), "name")
            colnames(endpoints) <- paste0(
                "NMDS", seq_len(private$.state$effectiveK))
            private$.state$envRows <- rows
            private$.state$vectorEndpoints <- endpoints

            for (i in seq_along(rows)) {
                row <- rows[[i]]
                self$results$envfit$addRow(
                    rowKey=row$name,
                    values=list(
                        variable=row$name,
                        r2=miso_num_or_na(row$r2),
                        p=miso_num_or_na(row$p),
                        samples=row$samples,
                        permutations=row$permutations,
                        NMDS1=miso_num_or_na(endpoints[i, 1L]),
                        NMDS2=miso_num_or_na(endpoints[i, 2L]),
                        NMDS3=if (identical(
                            private$.state$effectiveK, 3L)) {
                            miso_num_or_na(endpoints[i, 3L])
                        } else {
                            NA_real_
                        }))
            }
            self$results$envfit$setNote(
                key="interpretation",
                note=paste(
                    "Unadjusted p-values test association with the ordination,",
                    "not causation or group differences. Axes may rotate or reflect."))
        },

        .featureNames = function() {
            features <- private$.state$features
            if (is.null(features))
                return(character())
            labels <- rownames(features)
            if (is.null(labels))
                labels <- private$.state$prep$varsNames
            as.character(labels)
        },

        .vectorNames = function() {
            vectors <- private$.state$vectorEndpoints
            if (is.null(vectors))
                return(character())
            labels <- rownames(vectors)
            if (is.null(labels))
                labels <- vapply(
                    private$.state$envRows,
                    `[[`, character(1), "name")
            as.character(labels)
        },

        .prepareLabelState = function() {
            features <- private$.state$features
            if (!is.null(features) && nrow(features) > 0L &&
                    ncol(features) >= 2L) {
                labels <- private$.featureNames()
                selection <- private$.selectPlotLabels(
                    features[, 1:2, drop=FALSE], labels)
                private$.state$featureLabelSelection <- selection
                private$.state$featureLabelsShown <- labels[selection$shown]
                private$.state$featureLabelsOmitted <- labels[selection$omitted]
            }

            vectors <- private$.state$vectorEndpoints
            if (!is.null(vectors)) {
                vectors <- as.matrix(vectors)
                if (nrow(vectors) > 0L && ncol(vectors) >= 2L) {
                    labels <- private$.vectorNames()
                    selection <- private$.selectPlotLabels(
                        vectors[, 1:2, drop=FALSE], labels)
                    private$.state$vectorLabelSelection <- selection
                    private$.state$vectorLabelsShown <- labels[selection$shown]
                    private$.state$vectorLabelsOmitted <- labels[selection$omitted]
                }
            }
        },

        .effectiveOverlaySummary = function() {
            overlays <- private$.state$overlays
            if (length(overlays) == 0L || is.null(overlays$effective))
                return("None")
            labels <- c(
                points="group colour and shape",
                hull="group hull",
                ellipse="1-SD dispersion ellipse",
                spider="group spider")
            active <- names(overlays$effective)[overlays$effective]
            if (length(active) == 0L)
                "None"
            else
                paste(unname(labels[active]), collapse=", ")
        },

        .labelDisclosure = function(kind, shown, omitted, total, table) {
            if (length(omitted) == 0L)
                return(character())
            sprintf(
                "%s labels: %d of %d shown; all values are in %s.",
                kind, length(shown), total, table)
        },

        .populateDescriptions = function() {
            self$results$ordinationDescription$setContent(
                private$.htmlBlock(
                    "Maps sample resemblance; closer points have more similar composition.",
                    ariaLabel="About nMDS ordination",
                    title="nMDS ordination"))
            if (isTRUE(self$options$nmdsShepard) &&
                    private$.state$shepardValid)
                self$results$shepardDescription$setContent(
                    private$.htmlBlock(
                        "Shows how well ordination distances preserve ranked dissimilarities.",
                        ariaLabel="About the nMDS Shepard diagram",
                        title="Shepard diagram"))
        },

        .populateInterpretation = function() {
            self$results$note$setContent(miso_html_block(paste(
                "Closer points are more similar; axis direction is arbitrary, so check stress.",
                "Feature scores are descriptive."),
                title="How to read this ordination"))
        },

        .populateSettings = function() {
            prep <- private$.state$prep
            fit <- private$.state$fit
            k <- private$.state$effectiveK
            requestedEnv <- unique(miso_clean_vars(self$options$nmdsEnv))
            envRows <- private$.state$envRows
            if (is.null(private$.state$groupLabels)) {
                grouping <- "None"
            } else {
                grouping <- sprintf(
                    "%s (%d assigned; %d unassigned)",
                    private$.state$groupVariable,
                    sum(!private$.state$groupMissing),
                    sum(private$.state$groupMissing))
            }

            private$.clearTable(self$results$settings)
            private$.addSetting(
                "Transformation",
                private$.transformLabel(self$options$transform))
            private$.addSetting(
                "Dissimilarity",
                private$.distanceLabel(self$options$distance))
            if (isTRUE(self$options$distBinary))
                private$.addSetting("Legacy Binary request", "Ignored")
            private$.addSetting(
                "Dimensions",
                if (identical(k, 3L))
                    "3 (legacy; plot shows NMDS1-NMDS2)"
                else
                    "2")
            private$.addSetting("Samples used", prep$rowsUsed)
            private$.addSetting("Features used", prep$varsUsed)
            if (prep$rowsMissingExcluded > 0L)
                private$.addSetting(
                    "Missing rows excluded",
                    prep$rowsMissingExcluded)
            if (prep$rowsZeroExcluded > 0L)
                private$.addSetting(
                    "All-zero rows excluded",
                    prep$rowsZeroExcluded)
            if (prep$featuresZeroExcluded > 0L)
                private$.addSetting(
                    "All-zero features excluded",
                    prep$featuresZeroExcluded)
            private$.addSetting("Grouping", grouping)
            private$.addSetting(
                "Group display",
                private$.effectiveOverlaySummary())
            private$.addSetting(
                "Environmental variables",
                if (length(requestedEnv) == 0L)
                    "None"
                else
                    sprintf(
                        "%d fitted of %d requested",
                        length(envRows),
                        length(requestedEnv)))
            if (length(requestedEnv) > 0L)
                private$.addSetting(
                    "Environmental permutations",
                    as.character(self$options$nmdsEnvPerm))
            private$.addSetting(
                "Feature Scores",
                if (!is.null(private$.state$features))
                    "Shown"
                else if (isTRUE(self$options$nmdsSpecies))
                    "Unavailable"
                else
                    "Hidden")
            private$.addSetting(
                "Shepard diagram",
                if (isTRUE(self$options$nmdsShepard) &&
                        private$.state$shepardValid)
                    "Shown"
                else if (isTRUE(self$options$nmdsShepard))
                    "Unavailable"
                else
                    "Hidden")
            private$.addSetting("Seed", private$.seedLabel(prep))
            private$.addSetting(
                "Random starts",
                sprintf(
                    "%s tried; %d maximum",
                    private$.fitValue(fit, "tries"),
                    as.integer(self$options$nmdsTrymax)))
            private$.addSetting(
                "Maximum iterations",
                as.character(self$options$nmdsMaxit))
        },

        .populateCoreResults = function() {
            fit <- private$.state$fit
            prep <- private$.state$prep
            sites <- private$.state$sites
            k <- private$.state$effectiveK

            for (table in c(
                    "stress", "shepardPairs", "sites", "features", "settings"))
                private$.clearTable(self$results[[table]])

            stress <- private$.finiteNumber(fit$stress)
            diagnosticRows <- list(
                c(
                    "Stress",
                    if (is.na(stress)) "Unavailable" else sprintf("%.4f", stress)),
                c("Effective dimensions", as.character(k)),
                c(
                    "Similar best solution repeats",
                    private$.fitValue(fit, "converged")),
                c(
                    "Retained optimization stopping reason",
                    private$.stoppingReason(fit)),
                c("Random starts tried", private$.fitValue(fit, "tries")),
                c("Best solution first found", private$.bestStart(fit)),
                c(
                    "Iterations in retained solution",
                    private$.fitValue(fit, "iters")),
                c("Engine", private$.fitValue(fit, "engine")))
            for (i in seq_along(diagnosticRows))
                self$results$stress$addRow(
                    rowKey=as.character(i),
                    values=list(
                        item=diagnosticRows[[i]][[1L]],
                        value=diagnosticRows[[i]][[2L]]))

            shepardPairs <- private$.state$shepardDisplayData
            if (isTRUE(self$options$nmdsShepard) && !is.null(shepardPairs))
                for (i in seq_len(nrow(shepardPairs)))
                    self$results$shepardPairs$addRow(
                        rowKey=as.character(i),
                        values=list(
                            dissimilarity=miso_num_or_na(
                                shepardPairs$dissimilarity[[i]]),
                            ordinationDistance=miso_num_or_na(
                                shepardPairs$ordinationDistance[[i]]),
                            monotonicFit=miso_num_or_na(
                                shepardPairs$monotonicFit[[i]])))

            isThreeDimensional <- identical(k, 3L)
            self$results$sites$getColumn("NMDS3")$setVisible(
                isThreeDimensional)
            self$results$features$getColumn("NMDS3")$setVisible(
                isThreeDimensional)
            self$results$envfit$getColumn("NMDS3")$setVisible(
                isThreeDimensional)
            self$results$sites$getColumn("group")$setVisible(
                !is.null(private$.state$groupLabels))
            self$results$ordination$setTitle(if (isThreeDimensional) {
                "NMDS1-NMDS2 view of a three-dimensional nMDS solution"
            } else {
                "Two-dimensional nMDS ordination"
            })

            groups <- private$.state$groupLabels
            for (i in seq_len(nrow(sites))) {
                groupValue <- if (is.null(groups)) "" else groups[[i]]
                values <- list(
                    row=prep$rowIndex[[i]],
                    NMDS1=miso_num_or_na(sites[i, 1L]),
                    NMDS2=miso_num_or_na(sites[i, 2L]),
                    NMDS3=if (isThreeDimensional)
                        miso_num_or_na(sites[i, 3L])
                    else
                        NA_real_,
                    group=groupValue)
                self$results$sites$addRow(
                    rowKey=as.character(prep$rowIndex[[i]]), values=values)
            }

            features <- private$.state$features
            if (! is.null(features)) {
                featureNames <- private$.featureNames()
                for (i in seq_len(nrow(features)))
                    self$results$features$addRow(
                        rowKey=as.character(i),
                        values=list(
                            feature=featureNames[[i]],
                            NMDS1=miso_num_or_na(features[i, 1L]),
                            NMDS2=miso_num_or_na(features[i, 2L]),
                            NMDS3=if (isThreeDimensional)
                                miso_num_or_na(features[i, 3L])
                            else
                                NA_real_))
            }
            private$.prepareLabelState()
            private$.populateDescriptions()
            private$.populateInterpretation()
            private$.populateSettings()
        },

        .plotLimits = function(x, y, expansion=0.08) {
            paddedRange <- function(values) {
                values <- values[is.finite(values)]
                if (length(values) == 0L)
                    return(c(-1, 1))
                limits <- range(values)
                span <- diff(limits)
                if (! is.finite(span) || span == 0)
                    span <- max(abs(limits), 1)
                limits + c(-1, 1) * span * expansion
            }
            list(x=paddedRange(x), y=paddedRange(y))
        },

        .overlayPathData = function(items, layer) {
            if (length(items) == 0L)
                return(data.frame())
            do.call(rbind, lapply(names(items), function(group) {
                geometry <- as.matrix(items[[group]])
                data.frame(
                    x=geometry[, 1L], y=geometry[, 2L],
                    group=group, layer=layer,
                    pathGroup=paste(layer, group, sep="::"),
                    stringsAsFactors=FALSE)
            }))
        },

        .overlaySegmentData = function(items, layer) {
            if (length(items) == 0L)
                return(data.frame())
            do.call(rbind, lapply(names(items), function(group) {
                geometry <- as.data.frame(items[[group]]$segments)
                data.frame(
                    x=geometry$x0, y=geometry$y0,
                    xend=geometry$x1, yend=geometry$y1,
                    group=group, layer=layer,
                    stringsAsFactors=FALSE)
            }))
        },

        .nmdsPlotData = function() {
            list(
                sites=private$.state$sites,
                overlays=private$.state$overlays,
                features=private$.state$features,
                groupLabels=private$.state$groupLabels,
                stress=private$.finiteNumber(private$.state$fit$stress),
                featureNames=private$.featureNames(),
                featureLabelSelection=private$.state$featureLabelSelection,
                vectorEndpoints=private$.state$vectorEndpoints,
                vectorLabelSelection=private$.state$vectorLabelSelection)
        },

        .shepardPlotData = function() {
            list(
                display=private$.state$shepardDisplayData,
                all=private$.state$shepardData)
        },

        .buildNmdsPlot = function (plotData = private$.nmdsPlotData())
        {
            sites <- plotData$sites
            if (is.null(sites) || ncol(sites) < 2L)
                return(NULL)
            overlays <- plotData$overlays
            features <- plotData$features
            xValues <- c(0, sites[is.finite(sites[, 1L]), 1L])
            yValues <- c(0, sites[is.finite(sites[, 2L]), 2L])
            styles <- overlays$styles
            pointsStyled <- isTRUE(overlays$effective[["points"]])
            plot <- ggplot2::ggplot() + ggplot2::labs(x = "NMDS1", y = "NMDS2", caption = {
                stress <- plotData$stress
                if (is.na(stress))
                    "Stress unavailable"
                else sprintf("Stress = %.3f", stress)
            }) + .misoPlotTheme()
            spiderData <- if (isTRUE(overlays$effective[["spider"]]))
                private$.overlaySegmentData(overlays$spider, "Group spider")
            else data.frame()
            hullData <- if (isTRUE(overlays$effective[["hull"]]))
                private$.overlayPathData(overlays$hull, "Group hull")
            else data.frame()
            ellipseData <- if (isTRUE(overlays$effective[["ellipse"]]))
                private$.overlayPathData(overlays$ellipse, "Dispersion ellipse (1 SD)")
            else data.frame()
            overlayData <- list(spiderData, hullData, ellipseData)
            for (geometry in overlayData) if (nrow(geometry) > 0L) {
                xValues <- c(xValues, geometry$x)
                yValues <- c(yValues, geometry$y)
                if ("xend" %in% names(geometry)) {
                    xValues <- c(xValues, geometry$xend)
                    yValues <- c(yValues, geometry$yend)
                }
            }
            lineMapping <- if (pointsStyled) {
                ggplot2::aes(colour = group, linetype = layer, linewidth = layer)
            }
            else {
                ggplot2::aes(colour = group, linetype = group, linewidth = layer)
            }
            if (nrow(spiderData) > 0L)
                plot <- plot + ggplot2::geom_segment(data = spiderData, mapping = utils::modifyList(lineMapping,
                    ggplot2::aes(x = x, y = y, xend = xend, yend = yend)), alpha = 0.72, show.legend = TRUE)
            if (nrow(hullData) > 0L)
                plot <- plot + ggplot2::geom_path(data = hullData, mapping = utils::modifyList(lineMapping, ggplot2::aes(x = x,
                    y = y, group = pathGroup)), show.legend = TRUE)
            if (nrow(ellipseData) > 0L)
                plot <- plot + ggplot2::geom_path(data = ellipseData, mapping = utils::modifyList(lineMapping,
                    ggplot2::aes(x = x, y = y, group = pathGroup)), show.legend = TRUE)
            siteData <- data.frame(x = sites[, 1L], y = sites[, 2L], stringsAsFactors = FALSE)
            if (pointsStyled) {
                siteData$group <- factor(plotData$groupLabels, levels = styles$group)
                plot <- plot + ggplot2::geom_point(data = siteData, ggplot2::aes(x = x, y = y, colour = group,
                    shape = group), size = 2.5, stroke = 0.8)
            }
            else {
                plot <- plot + ggplot2::geom_point(data = siteData, ggplot2::aes(x = x, y = y), shape = 21L,
                    size = 2.5, stroke = 0.7, colour = "#1A1A1A", fill = "#005A9C")
            }
            if (!is.null(features) && nrow(features) > 0L && ncol(features) >= 2L) {
                finiteFeatures <- is.finite(features[, 1L]) & is.finite(features[, 2L])
                featureData <- data.frame(x = 0, y = 0, xend = features[finiteFeatures, 1L], yend = features[finiteFeatures,
                    2L], label = plotData$featureNames[finiteFeatures], group = "Feature scores", layer = "Feature score",
                    original = which(finiteFeatures), stringsAsFactors = FALSE)
                xValues <- c(xValues, featureData$xend)
                yValues <- c(yValues, featureData$yend)
                plot <- plot + ggplot2::geom_segment(data = featureData, ggplot2::aes(x = x, y = y, xend = xend,
                    yend = yend), arrow = grid::arrow(length = grid::unit(0.12, "cm"), type = "closed"), colour = "#4D4D4D",
                    linewidth = 0.45, alpha = 0.72, show.legend = FALSE)
                labels <- plotData$featureNames
                shown <- plotData$featureLabelSelection$shown
                shown <- shown[shown %in% featureData$original]
                if (length(shown) > 0L) {
                    labelData <- data.frame(x = features[shown, 1L], y = features[shown, 2L], label = labels[shown],
                        stringsAsFactors = FALSE)
                    xValues <- c(xValues, labelData$x)
                    yValues <- c(yValues, labelData$y)
                    plot <- plot + ggplot2::geom_text(data = labelData, ggplot2::aes(x = x, y = y, label = label),
                        colour = "#3D3D3D", size = 3.5, vjust = -0.45, check_overlap = FALSE, show.legend = FALSE)
                }
            }
            vectors <- plotData$vectorEndpoints
            if (!is.null(vectors)) {
                vectors <- as.matrix(vectors)
                if (nrow(vectors) > 0L && ncol(vectors) >= 2L) {
                    labels <- rownames(vectors)
                    if (is.null(labels))
                        labels <- seq_len(nrow(vectors))
                    finiteVectors <- is.finite(vectors[, 1L]) & is.finite(vectors[, 2L])
                    if (any(finiteVectors)) {
                        labelExpansion <- 1.12
                        arrowMultiplier <- suppressWarnings(tryCatch(vegan::ordiArrowMul(vectors[finiteVectors,
                          1:2, drop = FALSE], fill = 0.75/labelExpansion), error = function(e) NA_real_))
                        if (length(arrowMultiplier) != 1L || !is.finite(arrowMultiplier) || arrowMultiplier <=
                          0)
                          arrowMultiplier <- 1
                        displayVectors <- matrix(NA_real_, nrow = nrow(vectors), ncol = 2L)
                        displayVectors[finiteVectors, ] <- vectors[finiteVectors, 1:2, drop = FALSE] * arrowMultiplier
                        vectorData <- data.frame(x = 0, y = 0, xend = displayVectors[finiteVectors, 1L], yend = displayVectors[finiteVectors,
                          2L], label = labels[finiteVectors], group = "Environmental vectors", layer = "Environmental vector",
                          original = which(finiteVectors), stringsAsFactors = FALSE)
                        xValues <- c(xValues, vectorData$xend)
                        yValues <- c(yValues, vectorData$yend)
                        plot <- plot + ggplot2::geom_segment(data = vectorData, ggplot2::aes(x = x, y = y, xend = xend,
                          yend = yend), arrow = grid::arrow(length = grid::unit(0.14, "cm"), type = "closed"),
                          colour = "#8B1A1A", linewidth = 0.7, show.legend = FALSE)
                        shown <- intersect(plotData$vectorLabelSelection$shown, which(finiteVectors))
                        if (length(shown) > 0L) {
                          labelData <- data.frame(x = displayVectors[shown, 1L] * labelExpansion, y = displayVectors[shown,
                            2L] * labelExpansion, label = labels[shown], stringsAsFactors = FALSE)
                          xValues <- c(xValues, labelData$x)
                          yValues <- c(yValues, labelData$y)
                          plot <- plot + ggplot2::geom_text(data = labelData, ggplot2::aes(x = x, y = y, label = label),
                            colour = "#8B1A1A", fontface = "bold", size = 3.5, vjust = -0.45, show.legend = FALSE)
                        }
                    }
                }
            }
            hasGroupLines <- any(vapply(list(spiderData, hullData, ellipseData), nrow, integer(1)) > 0L)
            if ((pointsStyled || hasGroupLines) && nrow(styles) > 0L) {
                legendStyles <- if (pointsStyled)
                    styles
                else styles[styles$assigned, , drop = FALSE]
                plot <- plot + ggplot2::scale_colour_manual(name = "Group", values = stats::setNames(legendStyles$colour,
                    legendStyles$group), drop = FALSE)
                if (pointsStyled)
                    plot <- plot + ggplot2::scale_shape_manual(name = "Group", values = stats::setNames(legendStyles$shape,
                        legendStyles$group), drop = FALSE)
                plot <- plot + ggplot2::guides(colour = ggplot2::guide_legend(nrow = 2, byrow = TRUE))
                if (pointsStyled)
                    plot <- plot + ggplot2::guides(shape = ggplot2::guide_legend(nrow = 2, byrow = TRUE))
            }
            effectiveLineLayers <- c("hull", "ellipse", "spider")
            effectiveLineLayers <- effectiveLineLayers[overlays$effective[effectiveLineLayers]]
            layerLabels <- c(hull = "Group hull", ellipse = "Dispersion ellipse (1 SD)", spider = "Group spider")
            if (length(effectiveLineLayers) > 0L) {
                lineLabels <- unname(layerLabels[effectiveLineLayers])
                plot <- plot + ggplot2::scale_linewidth_manual(name = "Layer", values = stats::setNames(unname(overlays$lineWidths[effectiveLineLayers]),
                    lineLabels))
                if (pointsStyled) {
                    plot <- plot + ggplot2::scale_linetype_manual(name = "Layer", values = stats::setNames(unname(overlays$lineTypes[effectiveLineLayers]),
                        lineLabels))
                }
                else {
                    assigned <- styles[styles$assigned, , drop = FALSE]
                    plot <- plot + ggplot2::scale_linetype_manual(name = "Group", values = stats::setNames(assigned$lineType,
                        assigned$group), drop = FALSE)
                }
                plot <- plot + ggplot2::guides(linetype = ggplot2::guide_legend(nrow = 2, byrow = TRUE))
            }
            limits <- private$.plotLimits(xValues, yValues)
            plot + ggplot2::coord_equal(xlim = limits$x, ylim = limits$y, expand = FALSE, clip = "off")
        }
,

        .plotNmds = function(image, ...) {
            plot <- private$.buildNmdsPlot(image$state)
            if (is.null(plot))
                return()
            suppressWarnings(print(plot))
            invisible(TRUE)
        },

        .buildShepardPlot = function(plotData = private$.shepardPlotData()) {
            if (is.null(plotData) || is.null(plotData$display) ||
                    is.null(plotData$all) || nrow(plotData$display) == 0L)
                return(NULL)
            data <- plotData$display
            allData <- plotData$all
            if (nrow(data) == 0L)
                return(NULL)
            fitData <- allData[order(
                allData$dissimilarity, allData$monotonicFit,
                method="radix"), , drop=FALSE]
            ggplot2::ggplot(
                data,
                ggplot2::aes(
                    x=dissimilarity, y=ordinationDistance)) +
                ggplot2::geom_point(
                    shape=21L, size=2.1, stroke=0.55,
                    colour="#1A1A1A", fill="#005A9C", alpha=0.72) +
                ggplot2::geom_line(
                    data=fitData,
                    ggplot2::aes(y=monotonicFit),
                    colour="#D55E00", linewidth=0.85) +
                ggplot2::labs(
                    x="Observed dissimilarity",
                    y="Ordination distance") +
                .misoPlotTheme()
        },

        .plotShepard = function(image, ...) {
            plot <- private$.buildShepardPlot(image$state)
            if (is.null(plot))
                return()
            suppressWarnings(print(plot))
            invisible(TRUE)
        }
    )
)
