.misoGroupAesthetics <- function(groups) {
    groupLabels <- sort(
        unique(as.character(stats::na.omit(groups))),
        method = "radix"
    )
    colours <- c(
        "#005A9C", "#A43E00", "#006B50", "#8F3A75",
        "#3B4CC0", "#7A4E00", "#000000", "#595959"
    )
    shapes <- c(16L, 17L, 15L, 18L, 3L, 7L, 8L, 4L)
    linetypes <- c(
        "solid", "dashed", "dotted", "dotdash",
        "longdash", "twodash", "44", "13"
    )
    groupCount <- length(groupLabels)

    if (groupCount > 64L)
        stop("at most 64 unique groups are supported", call. = FALSE)

    groupIndex <- seq_along(groupLabels) - 1L
    colourIndex <- (groupIndex %% length(colours)) + 1L
    shapeIndex <- (
        (groupIndex + groupIndex %/% length(colours)) %% length(shapes)
    ) + 1L

    list(
        colour = stats::setNames(
            colours[colourIndex],
            groupLabels
        ),
        shape = stats::setNames(
            shapes[shapeIndex],
            groupLabels
        ),
        linetype = stats::setNames(
            linetypes[shapeIndex],
            groupLabels
        )
    )
}

.misoJitter <- function(n, width = 0.16, seed = 104729L) {
    hadSeed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    if (hadSeed)
        oldSeed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)

    on.exit({
        if (hadSeed) {
            assign(".Random.seed", oldSeed, envir = .GlobalEnv)
        } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
            rm(".Random.seed", envir = .GlobalEnv)
        }
    }, add = TRUE)

    set.seed(seed)
    stats::runif(n, -width, width)
}

.misoShortLabel <- function(value, width = 24L) {
    if (length(width) != 1L ||
            !is.numeric(width) ||
            !is.finite(width) ||
            width < 1L ||
            width != floor(width) ||
            width > .Machine$integer.max) {
        stop("'width' must be one positive whole number", call. = FALSE)
    }
    width <- as.integer(width)

    originalNames <- names(value)
    value <- as.character(value)
    names(value) <- originalNames
    long <- !is.na(value) & nchar(value, type = "chars") > width

    if (any(long)) {
        if (width == 1L) {
            value[long] <- "\u2026"
        } else {
            value[long] <- paste0(
                substr(value[long], 1L, width - 1L),
                "\u2026"
            )
        }
    }

    value
}

.misoUniqueShortLabels <- function(values, width = 24L) {
    if (length(width) != 1L || !is.numeric(width) || !is.finite(width) ||
            width < 4L || width != floor(width) ||
            width > .Machine$integer.max) {
        stop("'width' must be one whole number of at least 4", call. = FALSE)
    }
    width <- as.integer(width)
    identities <- unique(as.character(values))
    identities <- identities[!is.na(identities)]
    labels <- .misoShortLabel(identities, width=width)
    names(labels) <- identities
    collisions <- duplicated(labels) | duplicated(labels, fromLast=TRUE)
    if (!any(collisions))
        return(labels)

    used <- labels[!collisions]
    collisionIndices <- which(collisions)
    for (index in collisionIndices) {
        suffixNumber <- index
        repeat {
            suffix <- paste0("~", suffixNumber)
            prefixWidth <- width - nchar(suffix)
            if (prefixWidth < 1L)
                stop("too many colliding labels for the requested width", call.=FALSE)
            candidate <- paste0(
                substr(identities[[index]], 1L, prefixWidth), suffix)
            if (!candidate %in% used)
                break
            suffixNumber <- suffixNumber + length(identities)
        }
        labels[[index]] <- candidate
        used <- c(used, candidate)
    }
    labels
}

.misoPlotTheme <- function(baseSize = 12) {
    ggplot2::theme_minimal(base_size = baseSize) +
        ggplot2::theme(
            panel.grid.minor = ggplot2::element_blank(),
            legend.position = "bottom",
            legend.box = "vertical",
            plot.title = ggplot2::element_blank()
        )
}

.misoPlotDisclosure <- function(shown, total, noun = "observations") {
    validateCount <- function(value, name) {
        if (length(value) != 1L ||
                !is.numeric(value) ||
                !is.finite(value) ||
                value < 0 ||
                value != floor(value)) {
            stop(
                sprintf("'%s' must be one finite non-negative whole number", name),
                call. = FALSE
            )
        }
        value
    }

    shown <- validateCount(shown, "shown")
    total <- validateCount(total, "total")
    if (shown > total)
        stop("'shown' must not exceed 'total'", call. = FALSE)

    omitted <- total - shown
    countText <- function(value) format(value, scientific = FALSE, trim = TRUE)

    if (omitted == 0) {
        sprintf("All %s %s are shown.", countText(total), noun)
    } else {
        sprintf(
            paste0(
                "%s of %s %s are shown; %s %s omitted from the image ",
                "but retained in summaries."
            ),
            countText(shown),
            countText(total),
            noun,
            countText(omitted),
            if (omitted == 1) "is" else "are"
        )
    }
}
