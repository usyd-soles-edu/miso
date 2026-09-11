.misoPcoa <- function(distance, correction="none", sqrtDist=FALSE,
        groups=NULL) {
    failure <- function(message) list(error=TRUE, message=message)

    if (!inherits(distance, "dist"))
        return(failure("PCoA requires a prepared dissimilarity object."))
    correction <- match.arg(correction, c("none", "lingoes", "cailliez"))
    if (length(sqrtDist) != 1L || is.na(sqrtDist))
        return(failure("Square-root-distance selection must be true or false."))

    distances <- unclass(distance)
    if (any(!is.finite(distances)))
        return(failure("Dissimilarities contain non-finite values."))
    if (any(distances < 0))
        return(failure("Dissimilarities must be non-negative."))

    sampleCount <- attr(distance, "Size")
    if (is.null(sampleCount) || !is.finite(sampleCount) || sampleCount < 2L)
        return(failure("At least two sites are required for PCoA."))

    siteNames <- attr(distance, "Labels")
    if (is.null(siteNames))
        siteNames <- as.character(seq_len(sampleCount))
    if (length(siteNames) != sampleCount)
        return(failure("Dissimilarity labels do not match the number of sites."))
    siteNames <- as.character(siteNames)

    groupFactor <- NULL
    if (!is.null(groups)) {
        if (length(groups) != sampleCount)
            return(failure("Grouping assignments do not match the PCoA sites."))
        if (anyNA(groups))
            return(failure("Grouping assignments contain missing values."))
        groupFactor <- droplevels(as.factor(groups))
        names(groupFactor) <- siteNames
    }

    workingDistance <- distance
    attr(workingDistance, "Labels") <- siteNames
    if (isTRUE(sqrtDist))
        workingDistance[] <- sqrt(workingDistance[])

    if (all(workingDistance == 0))
        return(failure(
            "All dissimilarities are zero, so PCoA axes cannot be estimated."))

    add <- if (identical(correction, "none")) FALSE else correction
    fit <- tryCatch(
        withCallingHandlers(
            vegan::wcmdscale(
                workingDistance,
                k=sampleCount - 1L,
                eig=TRUE,
                add=add,
                x.ret=TRUE),
            warning=function(w) invokeRestart("muffleWarning")),
        error=function(e) e)
    if (inherits(fit, "error"))
        return(failure(paste0("PCoA could not be fitted: ", fit$message)))

    eigenvalues <- as.numeric(fit$eig)
    if (length(eigenvalues) == 0L || any(!is.finite(eigenvalues)))
        return(failure("PCoA did not return finite eigenvalues."))
    positive <- eigenvalues > 0
    negative <- eigenvalues < 0
    positiveTotal <- sum(eigenvalues[positive])
    explained <- rep(NA_real_, length(eigenvalues))
    if (is.finite(positiveTotal) && positiveTotal > 0)
        explained[positive] <- 100 * eigenvalues[positive] / positiveTotal

    points <- as.matrix(fit$points)
    if (is.null(dim(points)))
        points <- matrix(points, ncol=1L)
    if (nrow(points) != sampleCount)
        return(failure("PCoA site coordinates do not match the input sites."))
    rownames(points) <- siteNames
    if (ncol(points) > 0L)
        colnames(points) <- paste0("PCoA", seq_len(ncol(points)))

    correctionConstant <- if (identical(correction, "none")) {
        NA_real_
    } else {
        as.numeric(fit$ac)
    }
    correctedDistance <- workingDistance
    if (!identical(correction, "none")) {
        if (!is.finite(correctionConstant))
            return(failure("The additive correction constant is not finite."))
        correctedDistance[] <- if (identical(correction, "lingoes")) {
            sqrt(workingDistance[]^2 + 2 * correctionConstant)
        } else {
            workingDistance[] + correctionConstant
        }
    }

    centroids <- NULL
    groupSizes <- NULL
    if (!is.null(groupFactor) && ncol(points) > 0L) {
        groupLevels <- levels(groupFactor)
        centroids <- do.call(rbind, lapply(groupLevels, function(level) {
            colMeans(points[groupFactor == level, , drop=FALSE])
        }))
        rownames(centroids) <- groupLevels
        colnames(centroids) <- colnames(points)
        groupSizes <- as.integer(table(groupFactor)[groupLevels])
        names(groupSizes) <- groupLevels
    }

    warnings <- character()
    if (any(negative)) {
        warnings <- c(warnings, sprintf(
            paste(
                "%d negative eigenvalue%s retained in the eigenvalue table;",
                "explained percentages use the sum of positive eigenvalues."),
            sum(negative), if (sum(negative) == 1L) " is" else "s are"))
    }
    if (sum(positive) < 2L) {
        warnings <- c(warnings,
            "Fewer than two positive axes are available, so a two-dimensional ordination cannot be drawn.")
    }

    list(
        error=FALSE,
        fit=fit,
        inputDistance=distance,
        transformedDistance=workingDistance,
        correctedDistance=correctedDistance,
        sqrtDist=isTRUE(sqrtDist),
        correction=correction,
        correctionConstant=correctionConstant,
        siteNames=siteNames,
        groups=groupFactor,
        points=points,
        centroids=centroids,
        groupSizes=groupSizes,
        eigenvalues=eigenvalues,
        positive=positive,
        negative=negative,
        explained=explained,
        positiveTotal=positiveTotal,
        positiveAxisCount=sum(positive),
        negativeAxisCount=sum(negative),
        axisLabels=if (ncol(points) > 0L) {
            paste0(
                colnames(points), " (",
                format(round(explained[seq_len(ncol(points))], 1), nsmall=1),
                "%)")
        } else {
            character()
        },
        warnings=warnings)
}

.misoPcoaBalancedIndices <- function(groups, maxPoints=1000L,
        identities=seq_along(groups)) {
    total <- length(groups)
    maxPoints <- suppressWarnings(as.integer(maxPoints))
    if (length(maxPoints) != 1L || is.na(maxPoints) || maxPoints < 1L)
        stop("maxPoints must be a positive integer.", call.=FALSE)
    if (length(identities) != total)
        stop("identities must match the number of groups.", call.=FALSE)
    if (total == 0L)
        return(integer())

    groupValues <- as.character(groups)
    identityValues <- as.character(identities)
    if (anyNA(groupValues) || anyNA(identityValues))
        stop("groups and identities cannot contain missing values.",
            call.=FALSE)

    allGroups <- sort(unique(groupValues), method="radix")
    target <- min(total, maxPoints)
    if (target == total) {
        selected <- seq_len(total)
        attr(selected, "allGroups") <- allGroups
        attr(selected, "displayedGroups") <- allGroups
        attr(selected, "omittedGroups") <- character()
        attr(selected, "allocation") <- stats::setNames(
            as.integer(table(factor(groupValues, levels=allGroups))),
            allGroups)
        return(selected)
    }
    displayedGroups <- allGroups[seq_len(min(length(allGroups), target))]
    omittedGroups <- setdiff(allGroups, displayedGroups)
    splitRows <- lapply(displayedGroups, function(group) {
        rows <- which(groupValues == group)
        rows[order(identityValues[rows], rows, method="radix")]
    })
    names(splitRows) <- displayedGroups

    allocation <- integer(length(splitRows))
    remaining <- target
    while (remaining > 0L) {
        available <- which(allocation < lengths(splitRows))
        if (length(available) == 0L)
            break
        take <- available[seq_len(min(length(available), remaining))]
        allocation[take] <- allocation[take] + 1L
        remaining <- remaining - length(take)
    }

    selected <- unlist(Map(function(rows, count) {
        positions <- unique(pmax(1L, pmin(
            length(rows),
            round(seq(1, length(rows), length.out=count)))))
        if (length(positions) < count)
            positions <- seq_len(count)
        rows[positions]
    }, splitRows, allocation), use.names=FALSE)
    attr(selected, "allGroups") <- allGroups
    attr(selected, "displayedGroups") <- displayedGroups
    attr(selected, "omittedGroups") <- omittedGroups
    attr(selected, "allocation") <- stats::setNames(allocation,
        displayedGroups)
    selected
}

.misoPreparePcoaPlot <- function(result, showCentroids=FALSE,
        showSpiders=FALSE, maxPoints=1000L) {
    if (isTRUE(result$error) || ncol(result$points) < 2L)
        return(list(available=FALSE, total=nrow(result$points), displayed=0L))

    grouped <- !is.null(result$groups)
    groupValues <- if (grouped) {
        as.character(result$groups)
    } else {
        rep("Sites", nrow(result$points))
    }
    identityValues <- paste(
        result$siteNames,
        format(result$points[, 1L], digits=17, scientific=TRUE,
            trim=TRUE),
        format(result$points[, 2L], digits=17, scientific=TRUE,
            trim=TRUE),
        sep="\r")
    indices <- .misoPcoaBalancedIndices(
        groupValues, maxPoints=maxPoints, identities=identityValues)
    allGroups <- attr(indices, "allGroups", exact=TRUE)
    displayedGroups <- attr(indices, "displayedGroups", exact=TRUE)
    omittedGroups <- attr(indices, "omittedGroups", exact=TRUE)
    sites <- data.frame(
        site=result$siteNames[indices],
        group=groupValues[indices],
        axis1=result$points[indices, 1L],
        axis2=result$points[indices, 2L],
        stringsAsFactors=FALSE)
    neutral <- grouped && length(allGroups) > 64L
    effectiveCentroids <- grouped && !neutral &&
        (isTRUE(showCentroids) || isTRUE(showSpiders))
    effectiveSpiders <- grouped && !neutral && isTRUE(showSpiders)
    centroids <- NULL
    if (effectiveCentroids) {
        centroidRows <- match(displayedGroups, rownames(result$centroids))
        centroids <- data.frame(
            group=displayedGroups,
            axis1=result$centroids[centroidRows, 1L],
            axis2=result$centroids[centroidRows, 2L],
            stringsAsFactors=FALSE)
    }
    if (effectiveSpiders) {
        lookup <- match(sites$group, rownames(result$centroids))
        sites$centroid1 <- result$centroids[lookup, 1L]
        sites$centroid2 <- result$centroids[lookup, 2L]
    }

    list(
        available=TRUE,
        sites=sites,
        centroids=centroids,
        grouped=grouped,
        neutral=neutral,
        showCentroids=effectiveCentroids,
        showSpiders=effectiveSpiders,
        total=length(groupValues),
        displayed=nrow(sites),
        allGroups=if (grouped) allGroups else character(),
        displayedGroups=if (grouped) displayedGroups else character(),
        omittedGroups=if (grouped) omittedGroups else character(),
        omittedGroupCount=if (grouped) length(omittedGroups) else 0L,
        xLabel=result$axisLabels[[1L]],
        yLabel=result$axisLabels[[2L]])
}

.misoPcoaSamplingDisclosure <- function(plotData) {
    content <- .misoPlotDisclosure(
        plotData$displayed, plotData$total, noun="sites")
    if (isTRUE(plotData$grouped) && plotData$omittedGroupCount > 0L) {
        content <- c(content, sprintf(
            paste(
                "%d of %d groups are represented in the image;",
                "%d %s %s omitted from the image and retained",
                "in the coordinate and centroid tables."),
            length(plotData$displayedGroups),
            length(plotData$allGroups),
            plotData$omittedGroupCount,
            if (plotData$omittedGroupCount == 1L) "group" else "groups",
            if (plotData$omittedGroupCount == 1L) "is" else "are"))
    }
    content
}

.misoBuildPcoaPlot <- function(plotData) {
    if (is.null(plotData) || !isTRUE(plotData$available))
        return(NULL)

    plot <- ggplot2::ggplot()
    if (isTRUE(plotData$showSpiders)) {
        plot <- plot + ggplot2::geom_segment(
            data=plotData$sites,
            ggplot2::aes(
                x=axis1, y=axis2, xend=centroid1, yend=centroid2,
                group=interaction(group, site)),
            colour="#666666", linewidth=0.5, alpha=0.85)
    }

    if (isTRUE(plotData$grouped) && !isTRUE(plotData$neutral)) {
        aesthetics <- .misoGroupAesthetics(plotData$sites$group)
        plot <- plot +
            ggplot2::geom_point(
                data=plotData$sites,
                ggplot2::aes(x=axis1, y=axis2, colour=group, shape=group),
                size=2.2, alpha=0.85)
    } else {
        plot <- plot + ggplot2::geom_point(
            data=plotData$sites,
            ggplot2::aes(x=axis1, y=axis2),
            colour="#333333", shape=16L, size=2.2, alpha=0.85)
    }

    if (isTRUE(plotData$showCentroids) && !is.null(plotData$centroids)) {
        if (!isTRUE(plotData$neutral)) {
            aesthetics <- .misoGroupAesthetics(plotData$centroids$group)
            plot <- plot + ggplot2::geom_point(
                data=plotData$centroids,
                ggplot2::aes(x=axis1, y=axis2, colour=group),
                shape=4L, size=4, stroke=1.2,
                show.legend=FALSE)
        } else {
            plot <- plot + ggplot2::geom_point(
                data=plotData$centroids,
                ggplot2::aes(x=axis1, y=axis2),
                colour="#000000", shape=4L, size=4, stroke=1.2)
        }
    }

    if (isTRUE(plotData$grouped) && !isTRUE(plotData$neutral)) {
        aesthetics <- .misoGroupAesthetics(plotData$sites$group)
        plot <- plot +
            ggplot2::scale_colour_manual(values=aesthetics$colour) +
            ggplot2::scale_shape_manual(values=aesthetics$shape)
    }

    plot +
        ggplot2::labs(x=plotData$xLabel, y=plotData$yLabel,
            colour="Group", shape="Group") +
        ggplot2::coord_equal() +
        .misoPlotTheme()
}

.misoPcoaPlotData <- function(result, showCentroids=FALSE,
        showSpiders=FALSE, maxPoints=1000L) {
    .misoPreparePcoaPlot(
        result,
        showCentroids=showCentroids,
        showSpiders=showSpiders,
        maxPoints=maxPoints)
}

.buildPcoaPlot <- function(plotData) {
    .misoBuildPcoaPlot(plotData)
}
