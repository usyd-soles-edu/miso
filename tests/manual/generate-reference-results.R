#!/usr/bin/env Rscript

stopifnot(requireNamespace("vegan", quietly = TRUE))
stopifnot(requireNamespace("permute", quietly = TRUE))

set_reference_rng <- function(seed) {
    RNGkind("Mersenne-Twister", "Inversion", "Rejection")
    RNGversion("4.0.0")
    set.seed(as.integer(seed))
}

parse_output_dir <- function(args = commandArgs(trailingOnly = TRUE)) {
    output_arg <- grep("^--output-dir=", args, value = TRUE)
    if (length(output_arg) > 1L)
        stop("Use only one --output-dir argument.")
    if (length(output_arg) == 1L)
        return(sub("^--output-dir=", "", output_arg[[1L]]))
    if (length(args) == 0L || "--write" %in% args)
        return(file.path("tests", "manual"))
    stop("Unsupported arguments. Use --write or --output-dir=<path>.")
}

write_stable_csv <- function(x, path) {
    character_columns <- which(vapply(x, is.character, logical(1)))
    write.table(
        x,
        file = path,
        sep = ",",
        row.names = FALSE,
        col.names = TRUE,
        quote = character_columns,
        na = "",
        dec = ".",
        eol = "\n",
        qmethod = "double",
        fileEncoding = "UTF-8"
    )
}

feature_columns <- function(x) grep("^feature_[0-9]+$", names(x), value = TRUE)

transform_features <- function(x, method) {
    switch(
        method,
        none = as.data.frame(x),
        sqrt = sqrt(as.data.frame(x)),
        hellinger = as.data.frame(vegan::decostand(x, method = "hellinger")),
        pa = as.data.frame(vegan::decostand(x, method = "pa")),
        stop("Unsupported reference transformation: ", method)
    )
}

make_distance <- function(x, method, binary = FALSE) {
    vegan::vegdist(x, method = method, binary = isTRUE(binary))
}

make_permutation <- function(nperm, scheme = "free", strata = NULL) {
    blocks <- if (is.null(strata) || length(strata) == 0L) NULL else factor(strata)
    switch(
        scheme,
        free = permute::how(nperm = as.integer(nperm)),
        stratified = permute::how(nperm = as.integer(nperm), blocks = blocks),
        series = permute::how(
            nperm = as.integer(nperm),
            blocks = blocks,
            within = permute::Within(type = "series", mirror = FALSE)
        ),
        stop("Unsupported permutation scheme: ", scheme)
    )
}

raw_number <- function(x) {
    if (length(x) == 0L || is.na(x)) return(NA_character_)
    format(as.numeric(x), digits = 15L, scientific = FALSE, trim = TRUE)
}

display_number <- function(x, digits = 4L) {
    if (length(x) == 0L || is.na(x)) return(NA_character_)
    formatC(as.numeric(x), format = "f", digits = digits)
}

display_p <- function(x) {
    if (length(x) == 0L || is.na(x)) return(NA_character_)
    if (x < 0.001) "< .001" else sub("^0", "", formatC(x, format = "f", digits = 3L))
}

reference_row <- function(
    dataset, scenario_id, analysis, result_slot, metric, value,
    display = NULL, abs_tolerance = 1e-6, rel_tolerance = 0,
    reference_function, note = ""
) {
    if (is.null(display)) display <- display_number(value)
    data.frame(
        dataset = dataset,
        scenario_id = scenario_id,
        analysis = analysis,
        result_slot = result_slot,
        metric = metric,
        raw_value = raw_number(value),
        display_value = as.character(display),
        abs_tolerance = raw_number(abs_tolerance),
        rel_tolerance = raw_number(rel_tolerance),
        reference_function = reference_function,
        note = note,
        stringsAsFactors = FALSE
    )
}

text_row <- function(dataset, scenario_id, analysis, result_slot, metric, display, reference_function, note = "") {
    reference_row(
        dataset, scenario_id, analysis, result_slot, metric, NA_real_,
        display = display, abs_tolerance = NA_real_, rel_tolerance = NA_real_,
        reference_function = reference_function, note = note
    )
}

reference_permanova <- function(data, dataset, scenario_id, permutations, transform = "none", distance = "bray", binary = FALSE, strata = NULL) {
    features <- transform_features(data[feature_columns(data)], transform)
    dissimilarity <- make_distance(features, distance, binary)
    set_reference_rng(123)
    fit <- vegan::adonis2(
        dissimilarity ~ group,
        data = data.frame(group = factor(data$group)),
        permutations = make_permutation(permutations, if (is.null(strata)) "free" else "stratified", strata),
        by = "terms"
    )
    tab <- as.data.frame(fit)
    rows <- list(
        reference_row(dataset, scenario_id, "PERMANOVA", "PERMANOVA Table", "group pseudo-F", tab$F[[1L]], reference_function = "vegan::adonis2"),
        reference_row(dataset, scenario_id, "PERMANOVA", "PERMANOVA Table", "group R2", tab$R2[[1L]], reference_function = "vegan::adonis2"),
        reference_row(dataset, scenario_id, "PERMANOVA", "PERMANOVA Table", "group p", tab$`Pr(>F)`[[1L]], display = display_p(tab$`Pr(>F)`[[1L]]), abs_tolerance = 0, reference_function = "vegan::adonis2")
    )
    do.call(rbind, rows)
}

reference_anosim <- function(data, dataset, scenario_id, permutations, strata = NULL, pairwise = TRUE) {
    features <- data[feature_columns(data)]
    dissimilarity <- make_distance(features, "bray", FALSE)
    group <- factor(data$group)
    scheme <- if (is.null(strata)) "free" else "stratified"
    set_reference_rng(123)
    global <- vegan::anosim(
        dissimilarity,
        group,
        permutations = make_permutation(permutations, scheme, strata)
    )
    rows <- list(
        reference_row(dataset, scenario_id, "ANOSIM", "Global ANOSIM", "Global R", global$statistic, reference_function = "vegan::anosim"),
        reference_row(dataset, scenario_id, "ANOSIM", "Global ANOSIM", "Permutation p", global$signif, display = display_p(global$signif), abs_tolerance = 0, reference_function = "vegan::anosim")
    )

    if (isTRUE(pairwise)) {
        contrasts <- combn(levels(group), 2L, simplify = FALSE)
        pairwise_rows <- lapply(contrasts, function(pair) {
            keep <- group %in% pair
            matrix_distance <- as.matrix(dissimilarity)
            sub_distance <- stats::as.dist(matrix_distance[keep, keep, drop = FALSE])
            sub_group <- droplevels(group[keep])
            sub_strata <- if (is.null(strata)) NULL else droplevels(factor(strata[keep]))
            set_reference_rng(123)
            fit <- vegan::anosim(
                sub_distance,
                sub_group,
                permutations = make_permutation(permutations, scheme, sub_strata)
            )
            data.frame(
                contrast = paste(pair, collapse = " vs "),
                statistic = unname(fit$statistic),
                p = fit$signif,
                stringsAsFactors = FALSE
            )
        })
        pairwise_rows <- do.call(rbind, pairwise_rows)
        pairwise_rows$padj <- stats::p.adjust(pairwise_rows$p, method = "holm")
        for (i in seq_len(nrow(pairwise_rows))) {
            label <- pairwise_rows$contrast[[i]]
            rows[[length(rows) + 1L]] <- reference_row(dataset, scenario_id, "ANOSIM", "Pairwise ANOSIM", paste0(label, " R"), pairwise_rows$statistic[[i]], reference_function = "vegan::anosim pairwise loop")
            rows[[length(rows) + 1L]] <- reference_row(dataset, scenario_id, "ANOSIM", "Pairwise ANOSIM", paste0(label, " adjusted p"), pairwise_rows$padj[[i]], display = display_p(pairwise_rows$padj[[i]]), abs_tolerance = 0, reference_function = "vegan::anosim pairwise loop")
        }
    }
    do.call(rbind, rows)
}

reference_permdisp <- function(data, dataset, scenario_id, permutations, scheme = "free", centre = "median", bias = FALSE, pairwise = TRUE) {
    features <- data[feature_columns(data)]
    dissimilarity <- make_distance(features, "bray", FALSE)
    group <- factor(data$group)
    set_reference_rng(123)
    fit <- vegan::betadisper(dissimilarity, group, type = centre, bias.adjust = bias)
    omnibus <- vegan::permutest(fit, permutations = make_permutation(permutations, scheme, NULL))
    tab <- as.data.frame(omnibus$tab)
    rows <- list(
        reference_row(dataset, scenario_id, "PERMDISP", "Dispersion Test", "F", tab$F[[1L]], reference_function = "vegan::betadisper + vegan::permutest"),
        reference_row(dataset, scenario_id, "PERMDISP", "Dispersion Test", "Permutation p", tab$`Pr(>F)`[[1L]], display = display_p(tab$`Pr(>F)`[[1L]]), abs_tolerance = 0, reference_function = "vegan::betadisper + vegan::permutest")
    )
    distances <- split(fit$distances, group, drop = TRUE)
    for (level in names(distances)) {
        values <- distances[[level]]
        rows[[length(rows) + 1L]] <- reference_row(dataset, scenario_id, "PERMDISP", "Distances to Group Centre", paste0(level, " n"), length(values), display = as.character(length(values)), abs_tolerance = 0, reference_function = "vegan::betadisper")
        rows[[length(rows) + 1L]] <- reference_row(dataset, scenario_id, "PERMDISP", "Distances to Group Centre", paste0(level, " mean distance"), mean(values), reference_function = "vegan::betadisper")
        rows[[length(rows) + 1L]] <- reference_row(dataset, scenario_id, "PERMDISP", "Distances to Group Centre", paste0(level, " median distance"), stats::median(values), reference_function = "vegan::betadisper")
        rows[[length(rows) + 1L]] <- reference_row(dataset, scenario_id, "PERMDISP", "Distances to Group Centre", paste0(level, " standard deviation"), stats::sd(values), reference_function = "vegan::betadisper")
        rows[[length(rows) + 1L]] <- reference_row(dataset, scenario_id, "PERMDISP", "Distances to Group Centre", paste0(level, " minimum"), min(values), reference_function = "vegan::betadisper")
        rows[[length(rows) + 1L]] <- reference_row(dataset, scenario_id, "PERMDISP", "Distances to Group Centre", paste0(level, " maximum"), max(values), reference_function = "vegan::betadisper")
    }
    if (isTRUE(pairwise)) {
        pair <- vegan::permutest(
            fit,
            permutations = make_permutation(permutations, scheme, NULL),
            pairwise = TRUE
        )
        observed <- pair$pairwise$observed
        permuted <- pair$pairwise$permuted
        adjusted <- stats::p.adjust(permuted, method = "holm")
        for (i in seq_along(observed)) {
            label <- names(observed)[[i]]
            rows[[length(rows) + 1L]] <- reference_row(dataset, scenario_id, "PERMDISP", "Pairwise Dispersion Comparisons", paste0(label, " t"), observed[[i]], reference_function = "vegan::permutest pairwise")
            rows[[length(rows) + 1L]] <- reference_row(dataset, scenario_id, "PERMDISP", "Pairwise Dispersion Comparisons", paste0(label, " permutation p"), permuted[[i]], display = display_p(permuted[[i]]), abs_tolerance = 0, reference_function = "vegan::permutest pairwise")
            rows[[length(rows) + 1L]] <- reference_row(dataset, scenario_id, "PERMDISP", "Pairwise Dispersion Comparisons", paste0(label, " adjusted p"), adjusted[[i]], display = display_p(adjusted[[i]]), abs_tolerance = 0, reference_function = "vegan::permutest pairwise")
        }
    }
    do.call(rbind, rows)
}

finite_integer <- function(x) {
    if (is.null(x) || length(x) == 0L)
        return(NA_integer_)
    value <- suppressWarnings(as.integer(x[[1L]]))
    if (length(value) == 0L || is.na(value)) NA_integer_ else value
}

nmds_stopping_reason <- function(fit) {
    if (!identical(fit$engine, "monoMDS"))
        return("Not reported by this engine")
    switch(
        as.character(finite_integer(fit$icause)),
        `1` = "Maximum iterations reached",
        `2` = "Stress nearly zero",
        `3` = "Stress nearly unchanged",
        `4` = "Gradient scale factor nearly zero",
        "Unavailable"
    )
}

nmds_best_start <- function(fit) {
    best <- finite_integer(fit$bestry)
    if (is.na(best))
        "Unavailable"
    else if (best == 0L)
        "Initial configuration"
    else
        paste("Start", best)
}

reference_nmds <- function(
    data, dataset, scenario_id, binary = FALSE, k = 2L,
    feature_scores = FALSE, environmental = c("temperature", "pH")
) {
    features <- data[feature_columns(data)]
    set_reference_rng(123)
    fit <- suppressWarnings(vegan::metaMDS(
        features,
        distance = "bray",
        k = as.integer(k),
        trymax = 20L,
        maxit = 200L,
        wascores = isTRUE(feature_scores),
        autotransform = FALSE,
        trace = FALSE
    ))
    post_fit_rng <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    } else {
        NULL
    }
    sites <- as.matrix(vegan::scores(
        fit, display = "sites", choices = seq_len(k)))
    feature_points <- if (isTRUE(feature_scores)) {
        as.matrix(vegan::scores(
            fit, display = "species", choices = seq_len(k)))
    } else {
        NULL
    }
    distances <- sort(as.numeric(stats::dist(sites)))
    distance_probs <- c(0, 0.25, 0.5, 0.75, 1)
    distance_quantiles <- stats::quantile(
        distances, probs = distance_probs, names = FALSE, type = 7)
    fitted_environment <- list()
    for (name in environmental) {
        if (!is.null(post_fit_rng))
            assign(".Random.seed", post_fit_rng, envir = .GlobalEnv)
        fitted_environment[[name]] <- vegan::envfit(
            sites,
            data.frame(value = data[[name]], row.names = rownames(sites)),
            permutations = 99L,
            choices = seq_len(k)
        )
    }
    rows <- list(
        reference_row(dataset, scenario_id, "nMDS", "Stress and convergence diagnostics", "Stress", fit$stress, display = display_number(fit$stress, 4L), abs_tolerance = 5e-5, reference_function = "vegan::metaMDS", note = "Axis signs and orientation are not frozen; configuration checks use rotation-invariant distances."),
        reference_row(dataset, scenario_id, "nMDS", "Stress and convergence diagnostics", "Effective dimensions", k, display = as.character(k), abs_tolerance = 0, reference_function = "vegan::metaMDS"),
        reference_row(dataset, scenario_id, "nMDS", "Stress and convergence diagnostics", "Random starts tried", finite_integer(fit$tries), display = as.character(finite_integer(fit$tries)), abs_tolerance = 0, reference_function = "vegan::metaMDS"),
        reference_row(dataset, scenario_id, "nMDS", "Stress and convergence diagnostics", "Similar best solution repeats", finite_integer(fit$converged), display = as.character(finite_integer(fit$converged)), abs_tolerance = 0, reference_function = "vegan::metaMDS"),
        reference_row(dataset, scenario_id, "nMDS", "Stress and convergence diagnostics", "Best solution first found", finite_integer(fit$bestry), display = nmds_best_start(fit), abs_tolerance = 0, reference_function = "vegan::metaMDS"),
        reference_row(dataset, scenario_id, "nMDS", "Stress and convergence diagnostics", "Iterations in retained solution", finite_integer(fit$iters), display = as.character(finite_integer(fit$iters)), abs_tolerance = 0, reference_function = "vegan::metaMDS"),
        text_row(dataset, scenario_id, "nMDS", "Stress and convergence diagnostics", "Engine", as.character(fit$engine[[1L]]), "vegan::metaMDS"),
        text_row(dataset, scenario_id, "nMDS", "Stress and convergence diagnostics", "Retained optimization stopping reason", nmds_stopping_reason(fit), "vegan::monoMDS icause"),
        reference_row(dataset, scenario_id, "nMDS", "Site Scores", "table rows", nrow(sites), display = as.character(nrow(sites)), abs_tolerance = 0, reference_function = "vegan::scores"),
        reference_row(dataset, scenario_id, "nMDS", "Feature Scores", "table rows", if (is.null(feature_points)) 0L else nrow(feature_points), display = as.character(if (is.null(feature_points)) 0L else nrow(feature_points)), abs_tolerance = 0, reference_function = "vegan::scores"),
        reference_row(dataset, scenario_id, "nMDS", "Environmental Fit", "table rows", length(fitted_environment), display = as.character(length(fitted_environment)), abs_tolerance = 0, reference_function = "vegan::envfit"),
        text_row(dataset, scenario_id, "nMDS", if (k == 3L) "NMDS1-NMDS2 view of a three-dimensional nMDS solution" else "Two-dimensional nMDS ordination", "configuration reference", "pairwise site distances", "stats::dist", "Rotation and reflection invariant; raw axes are intentionally not stored."),
        text_row(dataset, scenario_id, "nMDS", "Shepard diagram", "plot", "present", "vegan::stressplot")
    )
    for (i in seq_along(distance_probs)) {
        label <- paste0(
            "Site distance quantile ",
            c("0", "25", "50", "75", "100")[[i]],
            "%")
        rows[[length(rows) + 1L]] <- reference_row(
            dataset, scenario_id, "nMDS", "Site Scores", label,
            distance_quantiles[[i]], abs_tolerance = 1e-10,
            reference_function = "stats::quantile(stats::dist(vegan::scores))",
            note = "Rotation and reflection invariant.")
    }
    for (name in names(fitted_environment)) {
        env <- fitted_environment[[name]]
        rows[[length(rows) + 1L]] <- reference_row(dataset, scenario_id, "nMDS", "Environmental Fit", paste0(name, " r2"), env$vectors$r[[1L]], reference_function = "vegan::envfit")
        rows[[length(rows) + 1L]] <- reference_row(dataset, scenario_id, "nMDS", "Environmental Fit", paste0(name, " p"), env$vectors$pvals[[1L]], display = display_p(env$vectors$pvals[[1L]]), abs_tolerance = 0, reference_function = "vegan::envfit")
    }
    do.call(rbind, rows)
}

simper_contrast_label <- function(pair) {
    if (any(grepl("\\bvs\\b", pair, ignore.case = TRUE)))
        paste0("\u201c", pair[[1L]], "\u201d vs \u201c", pair[[2L]], "\u201d")
    else
        paste(pair, collapse = " vs ")
}

simper_display <- function(data, transform = "none", top_n = 10L, threshold = 70) {
    features <- transform_features(data[feature_columns(data)], transform)
    group <- factor(data$group)
    pairs <- combn(as.character(unique(group)), 2L, simplify = FALSE)
    fit <- vegan::simper(features, group, permutations = 0L)
    summaries <- summary(fit)
    rows <- list()
    for (index in seq_along(pairs)) {
        tab <- as.data.frame(summaries[[index]])
        tab$feature <- rownames(tab)
        tab <- tab[is.finite(tab$average) & tab$average >= 0, , drop = FALSE]
        tab <- tab[order(-tab$average, tab$feature), , drop = FALSE]
        tab$contribution <- tab$average / sum(tab$average, na.rm = TRUE)
        tab$cumulative <- cumsum(tab$contribution)
        crossing <- which(tab$cumulative >= threshold / 100)[1L]
        if (is.na(crossing)) crossing <- nrow(tab)
        display_n <- min(as.integer(top_n), crossing, nrow(tab))
        tab <- tab[seq_len(display_n), , drop = FALSE]
        tab$contrast_index <- index
        tab$contrast <- simper_contrast_label(pairs[[index]])
        rows[[length(rows) + 1L]] <- tab
    }
    list(
        features = features,
        group = group,
        pairs = pairs,
        fit = fit,
        displayed = do.call(rbind, rows))
}

reference_simper <- function(
    data, dataset, scenario_id, transform = "none",
    selected_distance = "bray", binary = FALSE,
    assessment = FALSE, permutations = 999L, details = FALSE
) {
    reference <- simper_display(data, transform, top_n = 10L, threshold = 70)
    displayed <- reference$displayed
    rows <- list()
    for (index in seq_along(reference$pairs)) {
        pair <- reference$pairs[[index]]
        contrast <- simper_contrast_label(pair)
        tab <- displayed[displayed$contrast == contrast, , drop = FALSE]
        rows[[length(rows) + 1L]] <- reference_row(dataset, scenario_id, "SIMPER", "Contrast summary", paste0(contrast, " average dissimilarity"), reference$fit[[index]]$overall, reference_function = "vegan::simper(permutations = 0)")
        rows[[length(rows) + 1L]] <- reference_row(dataset, scenario_id, "SIMPER", "Contrast summary", paste0(contrast, " n first group"), sum(as.character(reference$group) == pair[[1L]]), display = as.character(sum(as.character(reference$group) == pair[[1L]])), abs_tolerance = 0, reference_function = "group count")
        rows[[length(rows) + 1L]] <- reference_row(dataset, scenario_id, "SIMPER", "Contrast summary", paste0(contrast, " n second group"), sum(as.character(reference$group) == pair[[2L]]), display = as.character(sum(as.character(reference$group) == pair[[2L]])), abs_tolerance = 0, reference_function = "group count")
            rows[[length(rows) + 1L]] <- text_row(dataset, scenario_id, "SIMPER", "Descriptive feature contributions", paste0(contrast, " rows"), as.character(nrow(tab)), "vegan::simper + independent crossing-feature filter")
        if (nrow(tab) > 0L) {
            rows[[length(rows) + 1L]] <- text_row(dataset, scenario_id, "SIMPER", "Descriptive feature contributions", paste0(contrast, " first feature"), tab$feature[[1L]], "vegan::simper(permutations = 0)")
            rows[[length(rows) + 1L]] <- reference_row(dataset, scenario_id, "SIMPER", "Descriptive feature contributions", paste0(contrast, " first contribution percent"), 100 * tab$contribution[[1L]], reference_function = "vegan::simper(permutations = 0)")
            rows[[length(rows) + 1L]] <- reference_row(dataset, scenario_id, "SIMPER", "Descriptive feature contributions", paste0(contrast, " displayed cumulative percent"), 100 * tail(tab$cumulative, 1L), reference_function = "vegan::simper + independent crossing-feature filter")
            if (isTRUE(details)) {
                rows[[length(rows) + 1L]] <- reference_row(dataset, scenario_id, "SIMPER", "Contribution variability", paste0(contrast, " first average contribution"), tab$average[[1L]], reference_function = "vegan::simper(permutations = 0)")
                rows[[length(rows) + 1L]] <- reference_row(dataset, scenario_id, "SIMPER", "Contribution variability", paste0(contrast, " first SD"), tab$sd[[1L]], reference_function = "vegan::simper sd")
                rows[[length(rows) + 1L]] <- reference_row(dataset, scenario_id, "SIMPER", "Contribution variability", paste0(contrast, " first average divided by SD"), tab$ratio[[1L]], reference_function = "vegan::simper ratio")
                rows[[length(rows) + 1L]] <- reference_row(dataset, scenario_id, "SIMPER", "Group means", paste0(contrast, " first-group mean"), tab$ava[[1L]], reference_function = "vegan::simper ava")
                rows[[length(rows) + 1L]] <- reference_row(dataset, scenario_id, "SIMPER", "Group means", paste0(contrast, " second-group mean"), tab$avb[[1L]], reference_function = "vegan::simper avb")
            }
        }
    }

    if (isTRUE(assessment)) {
        set_reference_rng(123)
        assessed <- vegan::simper(
            reference$features,
            reference$group,
            permutations = as.integer(permutations))
        for (index in seq_along(reference$pairs)) {
            contrast <- simper_contrast_label(reference$pairs[[index]])
            tab <- displayed[displayed$contrast == contrast, , drop = FALSE]
            adjusted <- stats::p.adjust(assessed[[index]]$p, method = "holm")
            if (nrow(tab) > 0L) {
                feature <- tab$feature[[1L]]
                rows[[length(rows) + 1L]] <- reference_row(dataset, scenario_id, "SIMPER", "Exploratory permutation assessment", paste0(contrast, " first displayed p"), assessed[[index]]$p[[feature]], display = display_p(assessed[[index]]$p[[feature]]), abs_tolerance = 0, reference_function = "vegan::simper permutation p")
                rows[[length(rows) + 1L]] <- reference_row(dataset, scenario_id, "SIMPER", "Exploratory permutation assessment", paste0(contrast, " first displayed adjusted p"), adjusted[[feature]], display = display_p(adjusted[[feature]]), abs_tolerance = 0, reference_function = "p.adjust across full contrast")
            }
        }
        rows[[length(rows) + 1L]] <- reference_row(dataset, scenario_id, "SIMPER", "Analysis settings", "effective permutations", attr(assessed, "permutations"), display = as.character(attr(assessed, "permutations")), abs_tolerance = 0, reference_function = "attr(vegan::simper, permutations)")
    }

    settings <- paste0(
        "Permutation assessment: ", if (assessment) "Enabled" else "Disabled",
        "; detailed statistics: ", if (details) "Shown" else "Hidden",
        "; effective dissimilarity: Bray-Curtis",
        if (!identical(selected_distance, "bray")) paste0("; ignored legacy distance: ", selected_distance) else "",
        if (isTRUE(binary)) "; ignored legacy Binary request (separate from Presence/absence transformation)" else "")
    rows[[length(rows) + 1L]] <- text_row(dataset, scenario_id, "SIMPER", "Analysis settings", "effective choices", settings, "approved SIMPER contract")
    do.call(rbind, rows)
}

generate_references <- function(output_dir) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    scenarios <- read.csv(file.path("tests", "manual", "scenarios.csv"), stringsAsFactors = FALSE, check.names = FALSE)
    datasets <- c("tofu-small.csv", "tofu-large.csv")
    data <- setNames(lapply(datasets, function(name) {
        read.csv(file.path(output_dir, name), stringsAsFactors = FALSE, check.names = FALSE)
    }), datasets)

    rows <- list()
    for (dataset in datasets) {
        current <- data[[dataset]]
        permutations <- if (identical(dataset, "tofu-small.csv")) 999L else 199L
        size <- if (identical(dataset, "tofu-small.csv")) "small" else "large"
        rows[[length(rows) + 1L]] <- reference_permanova(current, dataset, paste0("permanova-", size, "-baseline"), permutations)
        rows[[length(rows) + 1L]] <- reference_anosim(current, dataset, paste0("anosim-", size, "-baseline"), permutations)
        rows[[length(rows) + 1L]] <- reference_permdisp(current, dataset, paste0("permdisp-", size, "-baseline"), permutations)
        rows[[length(rows) + 1L]] <- reference_nmds(current, dataset, paste0("nmds-", size, "-baseline"))
        rows[[length(rows) + 1L]] <- reference_simper(current, dataset, paste0("simper-", size, "-baseline"))
    }

    small <- data[["tofu-small.csv"]]
    rows[[length(rows) + 1L]] <- reference_permanova(small, "tofu-small.csv", "permanova-small-hellinger", 999L, transform = "hellinger", distance = "euclidean")
    rows[[length(rows) + 1L]] <- reference_permanova(small, "tofu-small.csv", "permanova-small-pa", 999L, transform = "pa", distance = "jaccard", binary = TRUE)
    rows[[length(rows) + 1L]] <- reference_anosim(small, "tofu-small.csv", "anosim-small-blocked", 999L, strata = small$block)
    rows[[length(rows) + 1L]] <- reference_permdisp(small, "tofu-small.csv", "permdisp-small-centroid", 999L, centre = "centroid", bias = TRUE)
    rows[[length(rows) + 1L]] <- reference_permdisp(small, "tofu-small.csv", "permdisp-small-legacy-stratified", 999L, scheme = "stratified")
    rows[[length(rows) + 1L]] <- reference_nmds(small, "tofu-small.csv", "nmds-small-binary-noop", binary = TRUE)
    rows[[length(rows) + 1L]] <- reference_nmds(
        small, "tofu-small.csv", "nmds-small-legacy-3d",
        k = 3L, feature_scores = TRUE)
    rows[[length(rows) + 1L]] <- reference_simper(small, "tofu-small.csv", "simper-small-transform", transform = "sqrt", details = TRUE)
    rows[[length(rows) + 1L]] <- reference_simper(small, "tofu-small.csv", "simper-small-distance-noop", selected_distance = "euclidean", binary = TRUE)
    rows[[length(rows) + 1L]] <- reference_simper(small, "tofu-small.csv", "simper-small-assessment", assessment = TRUE, permutations = 19L)
    rows[[length(rows) + 1L]] <- reference_simper(small, "tofu-small.csv", "simper-small-details", details = TRUE)

    result <- do.call(rbind, rows)
    result <- result[order(result$dataset, result$analysis, result$scenario_id, result$result_slot, result$metric), ]
    rownames(result) <- NULL

    stopifnot(!any(is.na(result$display_value) | result$display_value == ""))
    baseline_ids <- scenarios$scenario_id[grepl("-baseline$", scenarios$scenario_id)]
    stopifnot(all(baseline_ids %in% result$scenario_id))

    stress_rows <- result$analysis == "nMDS" & result$metric == "Stress"
    stress <- as.numeric(result$raw_value[stress_rows])
    stopifnot(all(is.finite(stress) & stress > 0 & stress < 0.2))

    for (dataset in datasets) {
        size <- if (identical(dataset, "tofu-small.csv")) "small" else "large"
        scenario <- paste0("permdisp-", size, "-baseline")
        means <- result[
            result$dataset == dataset & result$scenario_id == scenario &
                grepl("mean distance$", result$metric),
            c("metric", "raw_value")
        ]
        named_means <- setNames(as.numeric(means$raw_value), sub(" mean distance$", "", means$metric))
        stopifnot(named_means[["C"]] > named_means[["A"]], named_means[["C"]] > named_means[["B"]])
    }

    pa_f <- result$raw_value[result$scenario_id == "permanova-small-pa" & result$metric == "group pseudo-F"]
    stopifnot(length(pa_f) == 1L, is.finite(as.numeric(pa_f)))

    metric_values <- function(scenario, pattern) {
        selected <- result[result$scenario_id == scenario & grepl(pattern, result$metric), c("metric", "raw_value")]
        setNames(as.numeric(selected$raw_value), selected$metric)
    }
    simper_baseline <- metric_values("simper-small-baseline", "first contribution percent$")
    simper_distance <- metric_values("simper-small-distance-noop", "first contribution percent$")
    simper_transform <- metric_values("simper-small-transform", "first contribution percent$")
    stopifnot(isTRUE(all.equal(simper_baseline, simper_distance, tolerance = 0)))
    stopifnot(!isTRUE(all.equal(simper_baseline, simper_transform, tolerance = 1e-10)))

    nmds_baseline <- metric_values("nmds-small-baseline", "^Stress$")
    nmds_binary <- metric_values("nmds-small-binary-noop", "^Stress$")
    stopifnot(isTRUE(all.equal(nmds_baseline, nmds_binary, tolerance = 0)))

    nmds_values <- function(scenario) {
        selected <- result[result$scenario_id == scenario, ]
        selected <- selected[order(selected$result_slot, selected$metric), ]
        selected[c(
            "result_slot", "metric", "raw_value", "display_value",
            "abs_tolerance", "rel_tolerance", "reference_function", "note")]
    }
    stopifnot(isTRUE(all.equal(
        nmds_values("nmds-small-baseline"),
        nmds_values("nmds-small-binary-noop"),
        tolerance = 0,
        check.attributes = FALSE)))
    for (scenario in c(
            "nmds-small-baseline", "nmds-large-baseline",
            "nmds-small-legacy-3d")) {
        current <- result[result$scenario_id == scenario, ]
        stopifnot(
            sum(grepl("^Site distance quantile ", current$metric)) == 5L,
            !any(grepl("NMDS[123]", current$metric)),
            all(c(
                "Random starts tried", "Similar best solution repeats",
                "Best solution first found", "Iterations in retained solution",
                "Engine", "Retained optimization stopping reason",
                "table rows") %in% current$metric))
    }

    write_stable_csv(result, file.path(output_dir, "reference-results.csv"))

    commit <- tryCatch(system2("git", c("rev-parse", "HEAD"), stdout = TRUE, stderr = FALSE), error = function(e) "unavailable")
    generated_at <- Sys.getenv("SOURCE_DATE_EPOCH", unset = "2026-07-15T00:00:00Z")
    metadata <- c(
        paste0("Generated-at-UTC: ", generated_at),
        paste0("Source-commit: ", commit[[1L]]),
        paste0("R: ", getRversion()),
        paste0("vegan: ", as.character(utils::packageVersion("vegan"))),
        paste0(
            "Signed-input classifier: euclidean, manhattan, canberra, gower, ",
            "mahalanobis (vegan ",
            as.character(utils::packageVersion("vegan")), ")"),
        paste0("permute: ", as.character(utils::packageVersion("permute"))),
        paste0("RNGkind: ", paste(RNGkind(), collapse = ", ")),
        "RNGversion: 4.0.0",
        paste0("Platform: ", R.version$platform),
        paste0("Locale: ", Sys.getlocale("LC_NUMERIC")),
        paste0("Scenarios: ", nrow(scenarios)),
        paste0("Reference-rows: ", nrow(result))
    )
    writeLines(metadata, file.path(output_dir, "reference-session-info.txt"), useBytes = TRUE)

    for (analysis in c("PERMANOVA", "ANOSIM", "PERMDISP", "nMDS", "SIMPER")) {
        stopifnot(any(result$analysis == analysis & result$dataset == "tofu-small.csv"))
        stopifnot(any(result$analysis == analysis & result$dataset == "tofu-large.csv"))
        cat(analysis, "small and large references: PASS\n")
    }
    cat("reference assertions: PASS\n")
    invisible(result)
}

if (sys.nframe() == 0L)
    generate_references(parse_output_dir())
