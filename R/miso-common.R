miso_options_signature <- function(options, excluded=character()) {
    names <- setdiff(options$names, excluded)
    values <- lapply(names, function(name) options$option(name)$value)
    names(values) <- names
    serialize(values, NULL)
}

miso_clean_vars <- function(x) {
    if (is.null(x) || length(x) == 0)
        character()
    else
        x[! is.na(x) & x != ""]
}

miso_is_missing_var <- function(x) {
    is.null(x) || length(x) == 0 || all(is.na(x) | x == "")
}

miso_transform_community <- function(x, transform) {
    switch(transform,
        none = x,
        sqrt = sqrt(x),
        fourthroot = x ^ 0.25,
        log = log(x + 1),
        pa = vegan::decostand(x, method="pa"),
        wisconsin = vegan::wisconsin(x),
        hellinger = vegan::decostand(x, method="hellinger"),
        total = vegan::decostand(x, method="total"),
        max = vegan::decostand(x, method="max"),
        frequency = vegan::decostand(x, method="frequency"),
        normalize = vegan::decostand(x, method="normalize"),
        range = vegan::decostand(x, method="range"),
        standardize = vegan::decostand(x, method="standardize"),
        chi.square = vegan::decostand(x, method="chi.square"),
        rclr = vegan::decostand(x, method="rclr"),
        x)
}

miso_normalize_feature_columns <- function(comm) {
    normalize <- function(column) {
        if (is.numeric(column))
            return(as.numeric(column))
        if (! is.factor(column))
            return(NULL)
        levels <- levels(column)
        codes <- suppressWarnings(as.numeric(levels))
        if (anyNA(codes) || any(codes != round(codes)))
            return(NULL)
        codes[as.integer(column)]
    }

    normalized <- lapply(comm, normalize)
    unsupported <- names(comm)[vapply(normalized, is.null, logical(1))]
    if (length(unsupported) > 0L)
        jmvcore::reject(
            c("Feature variables must be numeric; unsupported feature type or measure assignment: {unsupported}."),
            unsupported=paste(unsupported, collapse=", "))

    out <- as.data.frame(normalized, check.names=FALSE,
        stringsAsFactors=FALSE)
    names(out) <- names(comm)
    out
}

miso_prepare_resemblance <- function(data, vars, factor=NULL, transform, distance, seed=0, extraVars=NULL, strata=NULL, requireFactor=TRUE, covariates=NULL, distBinary=FALSE) {
    warnings <- character()

    if (length(vars) == 0)
        return(list(error=TRUE, message="Select one or more feature variables."))
    if (requireFactor && miso_is_missing_var(factor))
        return(list(error=TRUE, message="Select a primary grouping factor."))

    species <- vars
    primary <- if (miso_is_missing_var(factor)) NULL else factor[[1]]
    extra <- miso_clean_vars(extraVars)
    extra <- setdiff(extra, c(primary, species))
    strata <- miso_clean_vars(strata)
    strata <- setdiff(strata, c(species, primary, extra))
    covs <- miso_clean_vars(covariates)
    covs <- setdiff(covs, c(species, primary, extra, strata))
    selected <- unique(c(species, primary, extra, strata, covs))
    selected <- selected[selected %in% names(data)]

    missingCols <- setdiff(unique(c(species, primary, extra, strata, covs)), names(data))
    if (length(missingCols) > 0)
        return(list(error=TRUE, message=paste0("Selected variable(s) not found in the data: ", paste(missingCols, collapse=", "))))

    dat <- data[, selected, drop=FALSE]
    comm <- miso_normalize_feature_columns(data[, species, drop=FALSE])
    if (length(covs) > 0) {
        covRaw <- data[, covs, drop=FALSE]
        covNonNumeric <- names(covRaw)[! vapply(covRaw, is.numeric, logical(1))]
        if (length(covNonNumeric) > 0)
            return(list(error=TRUE, message=paste0("Covariates must be numeric. Non-numeric: ", paste(covNonNumeric, collapse=", "))))
    }

    commMat <- as.matrix(comm)
    storage.mode(commMat) <- "double"
    if (any(commMat < 0, na.rm=TRUE)) {
        badCols <- colnames(commMat)[colSums(commMat < 0, na.rm=TRUE) > 0]
        return(list(error=TRUE, message=paste0("Negative values detected in feature variables: ", paste(badCols, collapse=", "), ". These analyses require non-negative values.")))
    }

    rowIndex <- seq_len(nrow(data))

    keep <- stats::complete.cases(dat)
    rowsMissingExcluded <- sum(!keep)
    if (! all(keep)) {
        warnings <- c(warnings, sprintf("%d rows excluded due to missing values in selected variables.", sum(!keep)))
        commMat <- commMat[keep, , drop=FALSE]
        dat <- dat[keep, , drop=FALSE]
        rowIndex <- rowIndex[keep]
    }

    emptyRows <- rowSums(commMat, na.rm=TRUE) == 0
    rowsZeroExcluded <- sum(emptyRows)
    if (any(emptyRows)) {
        warnings <- c(warnings, sprintf("%d all-zero samples excluded.", sum(emptyRows)))
        commMat <- commMat[!emptyRows, , drop=FALSE]
        dat <- dat[!emptyRows, , drop=FALSE]
        rowIndex <- rowIndex[!emptyRows]
    }

    emptyCols <- colSums(commMat, na.rm=TRUE) == 0
    featuresZeroExcluded <- sum(emptyCols)
    if (any(emptyCols)) {
        warnings <- c(warnings, sprintf("%d all-zero feature variables excluded.", sum(emptyCols)))
        commMat <- commMat[, !emptyCols, drop=FALSE]
    }

    if (ncol(commMat) == 0)
        return(list(error=TRUE, message="No feature variables with non-zero values remain after filtering."))
    if (nrow(commMat) < 3)
        return(list(error=TRUE, message=sprintf("Too few samples (%d) for multivariate analysis.", nrow(commMat))))

    if (identical(distance, "mahalanobis") && nrow(commMat) <= ncol(commMat))
        return(list(error=TRUE, message=sprintf("mahalanobis requires more samples than features; got n=%d p=%d. Choose another index or reduce features.", nrow(commMat), ncol(commMat))))

    group <- NULL
    if (! is.null(primary)) {
        group <- droplevels(as.factor(dat[[primary]]))
        if (nlevels(group) < 2)
            return(list(error=TRUE, message=sprintf("Primary factor '%s' has fewer than 2 groups after filtering.", primary)))
    }

    transformed <- tryCatch(miso_transform_community(commMat, transform), error=function(e) e)
    if (inherits(transformed, "error"))
        return(list(error=TRUE, message=paste0("Transformation failed: ", transformed$message)))
    if (any(! is.finite(transformed)))
        return(list(error=TRUE, message="Transformation produced non-finite values. Check for empty samples/features or incompatible data."))

    if (any(transformed < 0, na.rm=TRUE))
        warnings <- c(warnings, "Transformation produced negative values; abundance dissimilarity indices (e.g. Bray-Curtis) may be meaningless -- consider a Euclidean-type index for centered/log-ratio transforms.")

    distObj <- tryCatch(
        withCallingHandlers(
            vegan::vegdist(transformed, method=distance, binary=isTRUE(distBinary)),
            warning=function(w) {
                warnings <<- c(warnings, paste0("Dissimilarity warning: ", conditionMessage(w)))
                invokeRestart("muffleWarning")
            }),
        error=function(e) e)
    if (inherits(distObj, "error"))
        return(list(error=TRUE, message=paste0("Could not compute dissimilarity matrix: ", distObj$message)))

    seed <- as.integer(seed)
    if (is.na(seed) || seed <= 0)
        seed <- NA_integer_

    if (nrow(commMat) > 5000)
        warnings <- c(warnings, sprintf("Large dataset (%d samples). Distance matrix computation may be slow.", nrow(commMat)))

    countMethods <- c("morisita", "horn", "chao", "cao")
    if (distance %in% countMethods && any(abs(commMat - round(commMat)) > .Machine$double.eps^0.5, na.rm=TRUE))
        warnings <- c(warnings, sprintf("'%s' is designed for count data. Non-integer values detected.", distance))

    if (isTRUE(distBinary))
        warnings <- c(warnings, "Binary (presence/absence) dissimilarity requested: abundance magnitudes ignored.")

    covDF <- if (length(covs) > 0) dat[, covs, drop=FALSE] else NULL

    list(
        error=FALSE,
        data=dat,
        comm=commMat,
        transformed=transformed,
        dist=distObj,
        group=group,
        primary=primary,
        extra=extra,
        strata=strata,
        covariates=covDF,
        covariateNames=covs,
        warnings=warnings,
        rowIndex=rowIndex,
        rowsMissingExcluded=rowsMissingExcluded,
        rowsZeroExcluded=rowsZeroExcluded,
        featuresZeroExcluded=featuresZeroExcluded,
        rowsUsed=nrow(commMat),
        varsUsed=ncol(commMat),
        varsNames=colnames(commMat),
        seed=seed)
}

miso_summary_rows <- function(prep, transform, distance) {
    list(
        c("Samples used", as.character(prep$rowsUsed)),
        c("Feature variables used", as.character(prep$varsUsed)),
        c("Transformation", transform),
        c("Dissimilarity index", distance),
        c("Grouping variable", if (is.null(prep$primary)) "not selected" else prep$primary),
        c("Seed", ifelse(is.na(prep$seed), "random", as.character(prep$seed)))
    )
}

miso_clear_table <- function(table) {
    try(table$deleteRows(), silent=TRUE)
    if (!is.null(table$.__enclos_env__$private$.rowNames))
        table$.__enclos_env__$private$.rowNames <- character()
}

# Fixed-shape result tables keep their schema rows for the whole analysis
# lifecycle. Clearing writes typed blanks into those rows instead of replacing
# the Cell objects, so display-only reruns cannot collapse the report.
miso_clear_fixed_table <- function(table, rows) {
    values <- setNames(lapply(table$columns, function(column) {
        if (column$type %in% c("integer", "number")) NA_real_ else ""
    }), vapply(table$columns, `[[`, character(1), "name"))
    if (length(table$rowKeys) == 0L)
        for (rowNo in seq_len(rows))
            table$addRow(rowKey=as.character(rowNo), values=values)
    if (length(table$rowKeys) != rows)
        stop("fixed result table has an unexpected row count", call.=FALSE)
    for (rowNo in seq_len(rows))
        table$setRow(rowNo=rowNo, values=values)
    invisible(NULL)
}

miso_set_fixed_row <- function(table, rowNo, values) {
    rowNo <- as.integer(rowNo)
    if (length(rowNo) == 0L || is.na(rowNo)) {
        rowNo <- 1L
        if (length(table$rowKeys) > 0L) {
            current <- table$asDF
            rowNo <- which(rowSums(as.data.frame(lapply(
                current, function(column) is.na(column) | column == ""))) == ncol(current))[[1L]]
        }
    }
    if (length(table$rowKeys) < rowNo) {
        blank <- setNames(lapply(table$columns, function(column) {
            if (column$type %in% c("integer", "number")) NA_real_ else ""
        }), vapply(table$columns, `[[`, character(1), "name"))
        for (key in seq.int(length(table$rowKeys) + 1L, rowNo))
            table$addRow(rowKey=as.character(key), values=blank)
    }
    table$setRow(rowNo=rowNo, values=values)
    invisible(NULL)
}

miso_update_row_where <- function(table, column, value, values) {
    if (length(table$rowKeys) == 0L)
        return(invisible(FALSE))
    data <- table$asDF
    if (!column %in% names(data))
        return(invisible(FALSE))
    rowNo <- match(as.character(value), as.character(data[[column]]))
    if (is.na(rowNo))
        return(invisible(FALSE))
    table$setRow(rowNo=rowNo, values=values)
    invisible(TRUE)
}

miso_add_or_set_row <- function(table, rowKey, values) {
    numericKey <- suppressWarnings(as.integer(rowKey))
    rowNo <- if (length(numericKey) == 1L &&
            !is.na(numericKey) && numericKey <= length(table$rowKeys))
        numericKey
    else if (length(table$rowKeys) > 0L)
        match(as.character(rowKey), as.character(unlist(table$rowKeys, use.names=FALSE)))
    else
        NA_integer_
    if (length(rowNo) == 1L && !is.na(rowNo))
        table$setRow(rowNo=rowNo, values=values)
    else
        table$addRow(rowKey=as.character(rowKey), values=values)
    invisible(NULL)
}

miso_set_seed <- function(prep) {
    if (! is.na(prep$seed))
        set.seed(prep$seed)
}

miso_num_or_na <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    if (length(x) == 0 || is.na(x) || ! is.finite(x))
        NA_real_
    else
        x
}

miso_html_escape <- function(value) {
    value <- gsub("&", "&amp;", as.character(value), fixed=TRUE)
    value <- gsub("<", "&lt;", value, fixed=TRUE)
    value <- gsub(">", "&gt;", value, fixed=TRUE)
    value <- gsub("\"", "&quot;", value, fixed=TRUE)
    gsub("'", "&#39;", value, fixed=TRUE)
}

miso_html_block <- function(paragraphs, ariaLabel=NULL, title=NULL) {
    paragraphs <- as.character(paragraphs)
    paragraphs <- paragraphs[! is.na(paragraphs) & nzchar(paragraphs)]
    escaped <- miso_html_escape(paragraphs)
    escaped <- gsub("\n", "<br>", escaped, fixed=TRUE)
    accessibility <- if (is.null(ariaLabel) || !nzchar(ariaLabel)) {
        ""
    } else {
        paste0(
            ' role="note" aria-label="',
            miso_html_escape(as.character(ariaLabel[[1L]])),
            '"')
    }
    heading <- if (is.null(title) || !nzchar(title)) {
        ""
    } else {
        paste0(
            '<div role="heading" aria-level="3" ',
            'style="margin: 0 0 0.35em 0; font-weight: 600;">',
            miso_html_escape(as.character(title[[1L]])),
            '</div>\n')
    }
    paste0(
        '<div', accessibility,
        ' style="margin: 0; max-width: 44em; line-height: 1.45; ',
        'font-family: inherit; font-size: inherit; ',
        'white-space: normal; overflow-wrap: anywhere; word-break: normal;">\n',
        heading,
        paste0(
            '<p style="margin: 0 0 0.65em 0;">\n',
            escaped,
            '\n</p>',
            collapse=""),
        '\n</div>')
}

miso_warning_block <- function(paragraphs, title="Data handling warning",
        ariaLabel=title) {
    paragraphs <- as.character(paragraphs)
    paragraphs <- paragraphs[! is.na(paragraphs) & nzchar(paragraphs)]
    escaped <- miso_html_escape(paragraphs)
    escaped <- gsub("\n", "<br>", escaped, fixed=TRUE)
    title <- miso_html_escape(as.character(title[[1L]]))
    accessibility <- if (is.null(ariaLabel) || !nzchar(ariaLabel)) {
        ""
    } else {
        paste0(
            ' aria-label="',
            miso_html_escape(as.character(ariaLabel[[1L]])),
            '"')
    }
    paste0(
        '<div role="note"', accessibility,
        ' style="box-sizing: border-box; width: 100%; max-width: 100%; ',
        'margin: 0; padding: 0.55em 0.75em; border-left: 0.25em solid #b36b00; ',
        'line-height: 1.45; font-family: inherit; font-size: inherit; ',
        'white-space: normal; overflow-wrap: anywhere; word-break: normal;">\n',
        '<strong style="display: block; margin: 0 0 0.35em 0;">',
        title,
        '</strong>\n',
        paste0(
            '<p style="margin: 0 0 0.45em 0;">\n',
            escaped,
            '\n</p>',
            collapse=""),
        '\n</div>')
}

miso_populate_purposes <- function(results, purposes) {
    for (name in names(purposes)) {
        purpose <- purposes[[name]]
        if (length(purpose) != 2L)
            stop("each result purpose requires a label and one sentence",
                call.=FALSE)
        results[[name]]$setContent(miso_html_block(
            purpose[[2L]],
            ariaLabel=paste("About", purpose[[1L]]),
            title=purpose[[1L]]))
    }
    invisible(NULL)
}

miso_populate_summary <- function(results, prep, transform, distance) {
    rows <- miso_summary_rows(prep, transform, distance)
    for (i in seq_along(rows))
        results$summary$addRow(rowKey=as.character(i), values=list(item=rows[[i]][1], value=rows[[i]][2]))
}

miso_display_term <- function(term, prep) {
    labels <- c(.f1=prep$primary)
    if (length(prep$extra) > 0) {
        for (i in seq_along(prep$extra))
            labels[paste0(".f", i + 1)] <- prep$extra[[i]]
    }
    if (length(prep$covariateNames) > 0) {
        for (i in seq_along(prep$covariateNames))
            labels[paste0(".c", i)] <- prep$covariateNames[[i]]
    }

    out <- term
    keys <- names(labels)[order(nchar(names(labels)), decreasing=TRUE)]
    for (key in keys)
        out <- gsub(key, labels[[key]], out, fixed=TRUE)
    out
}

# Parallel cluster lifecycle. enabled=FALSE or cluster creation fails -> NULL (vegan runs serial).
miso_parallel <- function(enabled, n=NULL) {
    if (! isTRUE(enabled)) return(NULL)
    cores <- if (is.null(n) || n < 2) max(2, parallel::detectCores() - 1L) else as.integer(n)
    cl <- tryCatch(parallel::makeCluster(cores), error=function(e) NULL)
    if (is.null(cl)) return(NULL)
    # adonis2/anosim/permutest dispatch vegan internals (e.g. do_getF) to the
    # workers; a fresh PSOCK worker has no vegan namespace, so load vegan+permute.
    ok <- tryCatch({ parallel::clusterEvalQ(cl, { library(vegan); library(permute) }); TRUE }, error=function(e) FALSE)
    if (! ok) { try(parallel::stopCluster(cl), silent=TRUE); return(NULL) }
    cl
}

miso_parallel_stop <- function(cl) {
    if (! is.null(cl)) try(parallel::stopCluster(cl), silent=TRUE)
}

# Build a permute::how() from the module's scheme preset. strata is the blocking factor
# VECTOR (not a name); 'free' ignores it. 'series' assumes sample order = sequence order.
miso_permutation <- function(permN, scheme=c("free","stratified","series"), strata=NULL) {
    scheme <- match.arg(scheme)
    nperm <- as.integer(permN)
    blocks <- if (is.null(strata) || length(strata) == 0) NULL else as.factor(strata)
    switch(scheme,
        free       = permute::how(nperm=nperm),
        stratified = permute::how(nperm=nperm, blocks=blocks),
        series     = permute::how(nperm=nperm, blocks=blocks,
                       within=permute::Within(type="series", mirror=FALSE)))
}
