tofu_clean_vars <- function(x) {
    if (is.null(x) || length(x) == 0)
        character()
    else
        x[! is.na(x) & x != ""]
}

tofu_is_missing_var <- function(x) {
    is.null(x) || length(x) == 0 || all(is.na(x) | x == "")
}

tofu_transform_community <- function(x, transform) {
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

tofu_prepare_resemblance <- function(data, vars, factor=NULL, transform, distance, seed=0, extraVars=NULL, strata=NULL, requireFactor=TRUE, covariates=NULL, distBinary=FALSE) {
    warnings <- character()

    if (length(vars) == 0)
        return(list(error=TRUE, message="Select one or more feature variables."))
    if (requireFactor && tofu_is_missing_var(factor))
        return(list(error=TRUE, message="Select a primary grouping factor."))

    species <- vars
    primary <- if (tofu_is_missing_var(factor)) NULL else factor[[1]]
    extra <- tofu_clean_vars(extraVars)
    extra <- setdiff(extra, c(primary, species))
    strata <- tofu_clean_vars(strata)
    strata <- setdiff(strata, c(species, primary, extra))
    covs <- tofu_clean_vars(covariates)
    covs <- setdiff(covs, c(species, primary, extra, strata))
    selected <- unique(c(species, primary, extra, strata, covs))
    selected <- selected[selected %in% names(data)]

    missingCols <- setdiff(unique(c(species, primary, extra, strata, covs)), names(data))
    if (length(missingCols) > 0)
        return(list(error=TRUE, message=paste0("Selected variable(s) not found in the data: ", paste(missingCols, collapse=", "))))

    dat <- data[, selected, drop=FALSE]
    comm <- data[, species, drop=FALSE]
    nonNumeric <- names(comm)[! vapply(comm, is.numeric, logical(1))]
    if (length(nonNumeric) > 0)
        return(list(error=TRUE, message=paste0("Feature variables must be numeric. Non-numeric: ", paste(nonNumeric, collapse=", "))))
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

    keep <- stats::complete.cases(dat)
    if (! all(keep)) {
        warnings <- c(warnings, sprintf("%d rows excluded due to missing values in selected variables.", sum(!keep)))
        commMat <- commMat[keep, , drop=FALSE]
        dat <- dat[keep, , drop=FALSE]
    }

    emptyRows <- rowSums(commMat, na.rm=TRUE) == 0
    if (any(emptyRows)) {
        warnings <- c(warnings, sprintf("%d all-zero samples excluded.", sum(emptyRows)))
        commMat <- commMat[!emptyRows, , drop=FALSE]
        dat <- dat[!emptyRows, , drop=FALSE]
    }

    emptyCols <- colSums(commMat, na.rm=TRUE) == 0
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

    transformed <- tryCatch(tofu_transform_community(commMat, transform), error=function(e) e)
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
        rowsUsed=nrow(commMat),
        varsUsed=ncol(commMat),
        varsNames=colnames(commMat),
        seed=seed)
}

tofu_summary_rows <- function(prep, transform, distance) {
    list(
        c("Samples used", as.character(prep$rowsUsed)),
        c("Feature variables used", as.character(prep$varsUsed)),
        c("Transformation", transform),
        c("Dissimilarity index", distance),
        c("Grouping variable", if (is.null(prep$primary)) "not selected" else prep$primary),
        c("Seed", ifelse(is.na(prep$seed), "random", as.character(prep$seed)))
    )
}

tofu_clear_table <- function(table) {
    try(table$deleteRows(), silent=TRUE)
}

tofu_set_seed <- function(prep) {
    if (! is.na(prep$seed))
        set.seed(prep$seed)
}

tofu_num_or_na <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    if (length(x) == 0 || is.na(x) || ! is.finite(x))
        NA_real_
    else
        x
}

tofu_populate_summary <- function(results, prep, transform, distance) {
    rows <- tofu_summary_rows(prep, transform, distance)
    for (i in seq_along(rows))
        results$summary$addRow(rowKey=as.character(i), values=list(item=rows[[i]][1], value=rows[[i]][2]))
}

tofu_display_term <- function(term, prep) {
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
tofu_parallel <- function(enabled, n=NULL) {
    if (! isTRUE(enabled)) return(NULL)
    cores <- if (is.null(n) || n < 2) max(2, parallel::detectCores() - 1L) else as.integer(n)
    tryCatch(parallel::makeCluster(cores), error=function(e) NULL)
}

tofu_parallel_stop <- function(cl) {
    if (! is.null(cl)) try(parallel::stopCluster(cl), silent=TRUE)
}

# Build a permute::how() from a tofu scheme preset. strata is the blocking factor
# VECTOR (not a name); 'free' ignores it. 'series' assumes sample order = sequence order.
tofu_permutation <- function(permN, scheme=c("free","stratified","series"), strata=NULL) {
    scheme <- match.arg(scheme)
    nperm <- as.integer(permN)
    blocks <- if (is.null(strata) || length(strata) == 0) NULL else as.factor(strata)
    switch(scheme,
        free       = permute::how(nperm=nperm),
        stratified = permute::how(nperm=nperm, blocks=blocks),
        series     = permute::how(nperm=nperm, blocks=blocks,
                       within=permute::Within(type="series", mirror=FALSE)))
}
