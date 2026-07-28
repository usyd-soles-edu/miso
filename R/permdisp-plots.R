.tofuBalancedIndices <- function(groups, values, maxRaw=600L) {
    groups <- as.character(groups)
    values <- as.numeric(values)
    if (length(groups) != length(values))
        stop("'groups' and 'values' must have the same length", call.=FALSE)
    if (length(maxRaw) != 1L || !is.numeric(maxRaw) || !is.finite(maxRaw) ||
            maxRaw < 0L || maxRaw != floor(maxRaw) ||
            maxRaw > .Machine$integer.max)
        stop("'maxRaw' must be one finite non-negative whole number", call.=FALSE)

    finite <- which(!is.na(groups) & nzchar(groups) & is.finite(values))
    if (length(finite) <= maxRaw)
        return(finite)
    groupLevels <- unique(groups[finite])
    candidates <- lapply(groupLevels, function(group) {
        index <- finite[groups[finite] == group]
        index[order(values[index], index, method="radix")]
    })
    targets <- integer(length(candidates))
    targetTotal <- min(as.integer(maxRaw), length(finite))
    while (sum(targets) < targetTotal) {
        available <- which(targets < lengths(candidates))
        add <- available[seq_len(min(
            length(available), targetTotal - sum(targets)))]
        targets[add] <- targets[add] + 1L
    }

    selected <- lapply(seq_along(candidates), function(i) {
        take <- targets[[i]]
        if (take == 0L)
            return(integer())
        positions <- unique(as.integer(round(seq(
            1, length(candidates[[i]]), length.out=take))))
        candidates[[i]][positions]
    })
    unlist(selected, use.names=FALSE)
}

.preparePermdispDistanceDiagnostic <- function(fit, centre, maxRaw=600L) {
    distances <- as.numeric(fit$distances)
    groups <- as.character(fit$group)
    if (length(distances) == 0L || length(distances) != length(groups))
        return(NULL)
    centreLabel <- switch(
        centre,
        median="Median",
        centroid="Centroid",
        as.character(centre))
    all <- data.frame(
        sample=seq_along(distances),
        group=groups,
        distance=distances,
        stringsAsFactors=FALSE)
    finite <- is.finite(all$distance) & !is.na(all$group) & nzchar(all$group)
    all <- all[finite, , drop=FALSE]
    if (nrow(all) == 0L)
        return(NULL)
    groupLevels <- levels(droplevels(fit$group))
    groupLevels <- groupLevels[groupLevels %in% all$group]
    all$group <- factor(all$group, levels=groupLevels)
    all$groupIndex <- match(as.character(all$group), groupLevels)

    selected <- .tofuBalancedIndices(
        all$group, all$distance, maxRaw=maxRaw)
    raw <- all[selected, , drop=FALSE]
    raw$x <- raw$groupIndex + .tofuJitter(
        nrow(raw), width=.16, seed=104729L)
    rownames(raw) <- NULL

    summaries <- do.call(rbind, lapply(groupLevels, function(group) {
        values <- all$distance[all$group == group]
        data.frame(
            group=group,
            n=length(values),
            centre=centreLabel,
            mean=unname(mean(values)),
            median=unname(stats::median(values)),
            q1=unname(stats::quantile(values, .25, names=FALSE)),
            q3=unname(stats::quantile(values, .75, names=FALSE)),
            sd=unname(stats::sd(values)),
            min=unname(min(values)),
            max=unname(max(values)),
            stringsAsFactors=FALSE)
    }))
    rownames(summaries) <- NULL

    list(
        all=all,
        raw=raw,
        summaries=summaries,
        groups=groupLevels,
        centre=centreLabel,
        displayed=nrow(raw),
        total=nrow(all))
}

.buildPermdispDistancePlot <- function(diagnostic) {
    if (is.null(diagnostic) || nrow(diagnostic$all) == 0L)
        return(NULL)
    all <- diagnostic$all
    groups <- diagnostic$groups
    labels <- .tofuUniqueShortLabels(groups, width=24L)
    counts <- stats::setNames(diagnostic$summaries$n, diagnostic$summaries$group)
    axisLabels <- sprintf("%s\n(n = %d)", labels[groups], counts[groups])
    if (length(groups) <= 64L) {
        aesthetics <- .tofuGroupAesthetics(groups)
        colours <- aesthetics$colour[groups]
        shapes <- aesthetics$shape[groups]
    } else {
        colours <- stats::setNames(rep("#777777", length(groups)), groups)
        shapes <- stats::setNames(rep(16L, length(groups)), groups)
    }

    ggplot2::ggplot(
        all,
        ggplot2::aes(
            x=groupIndex, y=distance, group=group, fill=group)) +
        ggplot2::geom_boxplot(
            width=.58, outlier.shape=NA, alpha=.28,
            colour="#333333", linewidth=.55) +
        ggplot2::geom_point(
            data=diagnostic$raw,
            ggplot2::aes(
                x=x, y=distance, colour=group, shape=group),
            inherit.aes=FALSE, size=2.1, alpha=.9, stroke=.45) +
        ggplot2::scale_x_continuous(
            breaks=seq_along(groups), labels=axisLabels,
            expand=ggplot2::expansion(mult=c(.06, .06)),
            guide=ggplot2::guide_axis(n.dodge=2L)) +
        ggplot2::scale_fill_manual(values=colours, guide="none") +
        ggplot2::scale_colour_manual(values=colours, guide="none") +
        ggplot2::scale_shape_manual(values=shapes, guide="none") +
        ggplot2::labs(x="Group", y="Distance to group centre") +
        .tofuPlotTheme()
}

.preparePermdispOrdination <- function(fit, rowIndex=NULL, maxRaw=600L) {
    sitesMatrix <- tryCatch(as.matrix(fit$vectors), error=function(e) NULL)
    centresMatrix <- tryCatch(as.matrix(fit$centroids), error=function(e) NULL)
    if (is.null(sitesMatrix) || is.null(centresMatrix) ||
            nrow(sitesMatrix) == 0L || ncol(sitesMatrix) == 0L ||
            nrow(centresMatrix) == 0L || ncol(centresMatrix) == 0L)
        return(list(
            available=FALSE, tableAvailable=FALSE,
            reason=paste(
                "The fitted dispersion geometry does not contain site and group-centre coordinates.")))

    groups <- as.character(fit$group)
    if (nrow(sitesMatrix) != length(groups))
        return(list(
            available=FALSE, tableAvailable=FALSE,
            reason="Site scores could not be aligned with the fitted groups."))
    if (is.null(rowIndex) || length(rowIndex) != nrow(sitesMatrix))
        rowIndex <- seq_len(nrow(sitesMatrix))
    groupLevels <- levels(droplevels(fit$group))
    if (length(groupLevels) == 0L)
        groupLevels <- unique(groups[!is.na(groups) & nzchar(groups)])
    centreRows <- match(groupLevels, rownames(centresMatrix))
    if (anyNA(centreRows))
        return(list(
            available=FALSE, tableAvailable=FALSE,
            reason="Group centres could not be aligned with the fitted groups."))

    fittedAxes <- min(ncol(sitesMatrix), ncol(centresMatrix))
    displayedAxes <- min(2L, fittedAxes)
    axisNames <- colnames(sitesMatrix)[seq_len(displayedAxes)]
    if (is.null(axisNames))
        axisNames <- rep("", displayedAxes)
    missingNames <- is.na(axisNames) | !nzchar(axisNames)
    axisNames[missingNames] <- paste("Axis", seq_len(displayedAxes))[missingNames]
    if (displayedAxes < 2L)
        axisNames <- c(axisNames, "Axis 2 unavailable")

    siteAxis1 <- as.numeric(sitesMatrix[, 1L])
    siteAxis2 <- if (displayedAxes >= 2L)
        as.numeric(sitesMatrix[, 2L])
    else
        rep(NA_real_, nrow(sitesMatrix))
    centreAxis1 <- as.numeric(centresMatrix[centreRows, 1L])
    centreAxis2 <- if (displayedAxes >= 2L)
        as.numeric(centresMatrix[centreRows, 2L])
    else
        rep(NA_real_, length(centreRows))

    centres <- data.frame(
        point=paste("Centre:", groupLevels),
        pointType="Group centre",
        group=groupLevels,
        axis1=centreAxis1,
        axis2=centreAxis2,
        stringsAsFactors=FALSE)
    groupKeys <- stats::setNames(
        if (length(groupLevels) <= 12L)
            unname(.tofuUniqueShortLabels(groupLevels, width=24L))
        else
            paste0("G", seq_along(groupLevels)),
        groupLevels)
    centres$plotKey <- unname(groupKeys[centres$group])
    sites <- data.frame(
        point=paste("Row", rowIndex),
        pointType="Site",
        group=groups,
        axis1=siteAxis1,
        axis2=siteAxis2,
        stringsAsFactors=FALSE)
    sites$plotKey <- unname(groupKeys[sites$group])
    centreMatch <- match(sites$group, centres$group)
    sites$centre1 <- centres$axis1[centreMatch]
    sites$centre2 <- centres$axis2[centreMatch]
    finiteSites <- is.finite(sites$axis1) & is.finite(sites$axis2) &
        is.finite(sites$centre1) & is.finite(sites$centre2)
    finiteCentres <- is.finite(centres$axis1) & is.finite(centres$axis2)
    eligibleSites <- sites[finiteSites, , drop=FALSE]
    selected <- .tofuBalancedIndices(
        eligibleSites$group,
        eligibleSites$axis1 + eligibleSites$axis2 / 1000,
        maxRaw=maxRaw)
    raw <- eligibleSites[selected, , drop=FALSE]
    rownames(raw) <- NULL

    available <- displayedAxes >= 2L && nrow(eligibleSites) > 0L
    reason <- if (displayedAxes < 2L)
        "The fitted dispersion geometry contains fewer than two usable axes, so the ordination plot is unavailable."
    else if (nrow(eligibleSites) == 0L)
        paste(
            "No fitted sites had finite coordinates on both axes together with",
            "finite coordinates for their assigned group centre, so the",
            "ordination plot is unavailable.")
    else
        character()

    list(
        available=available,
        tableAvailable=TRUE,
        reason=reason,
        sites=sites,
        centres=centres,
        plotCentres=centres[finiteCentres, , drop=FALSE],
        eligibleSites=eligibleSites,
        raw=raw,
        groups=groupLevels,
        axisNames=axisNames[1:2],
        displayed=nrow(raw),
        plotEligible=nrow(eligibleSites),
        unplottable=nrow(sites) - nrow(eligibleSites),
        total=nrow(sites))
}

.buildPermdispOrdinationPlot <- function(ordination) {
    if (is.null(ordination) || !isTRUE(ordination$available))
        return(NULL)
    groups <- ordination$groups
    if (length(groups) <= 64L) {
        aesthetics <- .tofuGroupAesthetics(groups)
        colours <- aesthetics$colour[groups]
        shapes <- aesthetics$shape[groups]
    } else {
        colours <- stats::setNames(rep("#777777", length(groups)), groups)
        shapes <- stats::setNames(rep(16L, length(groups)), groups)
    }
    showLegend <- length(groups) <= 12L
    groupLabels <- stats::setNames(
        if (length(groups) <= 12L)
            unname(.tofuUniqueShortLabels(groups, width=24L))
        else
            paste0("G", seq_along(groups)),
        groups)

    plot <- ggplot2::ggplot() +
        ggplot2::geom_hline(yintercept=0, colour="#D9D9D9", linewidth=.35) +
        ggplot2::geom_vline(xintercept=0, colour="#D9D9D9", linewidth=.35) +
        ggplot2::geom_segment(
            data=ordination$raw,
            ggplot2::aes(
                x=axis1, y=axis2, xend=centre1, yend=centre2,
                colour=group),
            linewidth=.5, alpha=.75) +
        ggplot2::geom_point(
            data=ordination$raw,
            ggplot2::aes(
                x=axis1, y=axis2, colour=group, shape=group),
            size=2.2, alpha=.9, stroke=.45) +
        ggplot2::geom_point(
            data=ordination$plotCentres,
            ggplot2::aes(x=axis1, y=axis2, colour=group),
            shape=23L, fill="white", size=4, stroke=1.1,
            show.legend=FALSE) +
        ggplot2::scale_colour_manual(
            values=colours, labels=groupLabels[groups],
            guide=if (showLegend)
                ggplot2::guide_legend(ncol=min(4L, length(groups)))
            else "none") +
        ggplot2::scale_shape_manual(
            values=shapes, labels=groupLabels[groups],
            guide=if (showLegend)
                ggplot2::guide_legend(ncol=min(4L, length(groups)))
            else "none") +
        ggplot2::labs(
            x=ordination$axisNames[[1L]],
            y=ordination$axisNames[[2L]],
            colour="Group", shape="Group") +
        ggplot2::coord_equal() +
        .tofuPlotTheme()

    if (!showLegend && length(groups) <= 64L) {
        centres <- ordination$plotCentres
        centres$label <- unname(groupLabels[centres$group])
        plot <- plot + ggplot2::geom_text(
            data=centres,
            ggplot2::aes(x=axis1, y=axis2, label=label),
            colour="#222222", size=3.5, nudge_y=.025,
            check_overlap=TRUE, show.legend=FALSE)
    }
    plot
}
