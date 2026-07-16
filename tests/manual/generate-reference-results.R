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
    if ("--write" %in% args)
        return(file.path("tests", "manual"))
    stop("Refusing to overwrite references. Use --write or --output-dir=<path>.")
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

reference_anosim <- function(data, dataset, scenario_id, permutations, strata = NULL) {
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
        reference_row(dataset, scenario_id, "ANOSIM", "Global Test", "Global R", global$statistic, reference_function = "vegan::anosim"),
        reference_row(dataset, scenario_id, "ANOSIM", "Global Test", "p", global$signif, display = display_p(global$signif), abs_tolerance = 0, reference_function = "vegan::anosim")
    )

    contrasts <- combn(levels(group), 2L, simplify = FALSE)
    pairwise <- lapply(contrasts, function(pair) {
        keep <- group %in% pair
        matrix_distance <- as.matrix(dissimilarity)
        sub_distance <- stats::as.dist(matrix_distance[keep, keep, drop = FALSE])
        sub_group <- droplevels(group[keep])
        fit <- vegan::anosim(sub_distance, sub_group, permutations = as.integer(permutations))
        data.frame(
            contrast = paste(pair, collapse = " - "),
            statistic = unname(fit$statistic),
            p = fit$signif,
            stringsAsFactors = FALSE
        )
    })
    pairwise <- do.call(rbind, pairwise)
    pairwise$padj <- stats::p.adjust(pairwise$p, method = "holm")
    for (i in seq_len(nrow(pairwise))) {
        label <- pairwise$contrast[[i]]
        rows[[length(rows) + 1L]] <- reference_row(dataset, scenario_id, "ANOSIM", "Pairwise ANOSIM", paste0(label, " R"), pairwise$statistic[[i]], reference_function = "vegan::anosim pairwise loop")
        rows[[length(rows) + 1L]] <- reference_row(dataset, scenario_id, "ANOSIM", "Pairwise ANOSIM", paste0(label, " adjusted p"), pairwise$padj[[i]], display = display_p(pairwise$padj[[i]]), abs_tolerance = 0, reference_function = "vegan::anosim pairwise loop")
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
        reference_row(dataset, scenario_id, "PERMDISP", "Dispersion Test", "p", tab$`Pr(>F)`[[1L]], display = display_p(tab$`Pr(>F)`[[1L]]), abs_tolerance = 0, reference_function = "vegan::betadisper + vegan::permutest")
    )
    means <- tapply(fit$distances, group, mean)
    for (level in names(means)) {
        rows[[length(rows) + 1L]] <- reference_row(dataset, scenario_id, "PERMDISP", "Group Distances to Centre", paste0(level, " mean distance"), means[[level]], reference_function = "vegan::betadisper")
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
            rows[[length(rows) + 1L]] <- reference_row(dataset, scenario_id, "PERMDISP", "Pairwise Dispersion", paste0(label, " t"), observed[[i]], reference_function = "vegan::permutest pairwise")
            rows[[length(rows) + 1L]] <- reference_row(dataset, scenario_id, "PERMDISP", "Pairwise Dispersion", paste0(label, " adjusted p"), adjusted[[i]], display = display_p(adjusted[[i]]), abs_tolerance = 0, reference_function = "vegan::permutest pairwise")
        }
    }
    do.call(rbind, rows)
}

reference_nmds <- function(data, dataset, scenario_id, binary = FALSE) {
    features <- data[feature_columns(data)]
    set_reference_rng(123)
    fit <- suppressWarnings(vegan::metaMDS(
        features,
        distance = "bray",
        k = 2L,
        trymax = 20L,
        maxit = 200L,
        wascores = FALSE,
        autotransform = FALSE,
        trace = FALSE
    ))
    env <- vegan::envfit(fit, data[c("temperature", "pH")], permutations = 99L)
    rows <- list(
        reference_row(dataset, scenario_id, "nMDS", "Stress and Convergence", "Stress", fit$stress, display = display_number(fit$stress, 4L), abs_tolerance = 5e-5, reference_function = "vegan::metaMDS", note = if (binary) "Binary control is currently ignored before metaMDS." else "Axes may rotate or reflect."),
        text_row(dataset, scenario_id, "nMDS", "Ordination Plot", "site points", as.character(nrow(data)), "vegan::scores", "Plot orientation is not frozen."),
        text_row(dataset, scenario_id, "nMDS", "Shepard Diagram", "plot", "present", "vegan::stressplot")
    )
    for (name in names(env$vectors$r)) {
        rows[[length(rows) + 1L]] <- reference_row(dataset, scenario_id, "nMDS", "Environmental Fit", paste0(name, " r2"), env$vectors$r[[name]], reference_function = "vegan::envfit")
        rows[[length(rows) + 1L]] <- reference_row(dataset, scenario_id, "nMDS", "Environmental Fit", paste0(name, " p"), env$vectors$pvals[[name]], display = display_p(env$vectors$pvals[[name]]), abs_tolerance = 0, reference_function = "vegan::envfit")
    }
    do.call(rbind, rows)
}

simper_display <- function(data, transform = "none", permutations = 999L, top_n = 10L, threshold = 70) {
    features <- transform_features(data[feature_columns(data)], transform)
    set_reference_rng(123)
    fit <- vegan::simper(features, factor(data$group), permutations = as.integer(permutations))
    summaries <- summary(fit)
    rows <- list()
    for (contrast in names(summaries)) {
        tab <- as.data.frame(summaries[[contrast]])
        tab$feature <- rownames(tab)
        tab$contribution <- tab$average / sum(tab$average, na.rm = TRUE)
        tab$cumulative <- cumsum(tab$contribution)
        keep <- seq_len(nrow(tab)) <= as.integer(top_n) & tab$cumulative <= threshold / 100
        if (!any(keep)) keep[seq_len(min(top_n, nrow(tab)))] <- TRUE
        tab <- tab[keep, , drop = FALSE]
        tab$contrast <- contrast
        rows[[length(rows) + 1L]] <- tab
    }
    do.call(rbind, rows)
}

reference_simper <- function(data, dataset, scenario_id, permutations, transform = "none", selected_distance = "bray") {
    displayed <- simper_display(data, transform, permutations, top_n = 10L, threshold = 70)
    rows <- list()
    for (contrast in unique(displayed$contrast)) {
        tab <- displayed[displayed$contrast == contrast, , drop = FALSE]
        rows[[length(rows) + 1L]] <- text_row(dataset, scenario_id, "SIMPER", "Feature Contributions", paste0(contrast, " rows"), as.character(nrow(tab)), "vegan::simper + independent display filter")
        if (nrow(tab) > 0L) {
            rows[[length(rows) + 1L]] <- text_row(dataset, scenario_id, "SIMPER", "Feature Contributions", paste0(contrast, " first feature"), tab$feature[[1L]], "vegan::simper")
            rows[[length(rows) + 1L]] <- reference_row(dataset, scenario_id, "SIMPER", "Feature Contributions", paste0(contrast, " first contribution percent"), 100 * tab$contribution[[1L]], reference_function = "vegan::simper")
            rows[[length(rows) + 1L]] <- reference_row(dataset, scenario_id, "SIMPER", "Feature Contributions", paste0(contrast, " displayed cumulative percent"), 100 * tail(tab$cumulative, 1L), reference_function = "vegan::simper + independent display filter")
        }
    }
    note <- if (identical(selected_distance, "bray")) {
        paste0("Permutations: ", permutations, "; SIMPER uses Bray-Curtis.")
    } else {
        paste0("Selected ", selected_distance, " is ignored; SIMPER uses Bray-Curtis. Permutations: ", permutations, ".")
    }
    rows[[length(rows) + 1L]] <- text_row(dataset, scenario_id, "SIMPER", "Notes", "current behaviour", note, "vegan::simper")
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
        rows[[length(rows) + 1L]] <- reference_simper(current, dataset, paste0("simper-", size, "-baseline"), permutations)
    }

    small <- data[["tofu-small.csv"]]
    rows[[length(rows) + 1L]] <- reference_permanova(small, "tofu-small.csv", "permanova-small-hellinger", 999L, transform = "hellinger", distance = "euclidean")
    rows[[length(rows) + 1L]] <- reference_permanova(small, "tofu-small.csv", "permanova-small-pa", 999L, transform = "pa", distance = "jaccard", binary = TRUE)
    rows[[length(rows) + 1L]] <- reference_anosim(small, "tofu-small.csv", "anosim-small-blocked", 999L, strata = small$block)
    rows[[length(rows) + 1L]] <- reference_permdisp(small, "tofu-small.csv", "permdisp-small-centroid", 999L, centre = "centroid", bias = TRUE)
    rows[[length(rows) + 1L]] <- reference_permdisp(small, "tofu-small.csv", "permdisp-small-stratified-noop", 999L, scheme = "stratified")
    rows[[length(rows) + 1L]] <- reference_nmds(small, "tofu-small.csv", "nmds-small-binary-noop", binary = TRUE)
    rows[[length(rows) + 1L]] <- reference_simper(small, "tofu-small.csv", "simper-small-transform", 999L, transform = "sqrt")
    rows[[length(rows) + 1L]] <- reference_simper(small, "tofu-small.csv", "simper-small-distance-noop", 999L, selected_distance = "euclidean")
    rows[[length(rows) + 1L]] <- reference_simper(small, "tofu-small.csv", "simper-small-permutation-note", 19L)

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

    write_stable_csv(result, file.path(output_dir, "reference-results.csv"))

    commit <- tryCatch(system2("git", c("rev-parse", "HEAD"), stdout = TRUE, stderr = FALSE), error = function(e) "unavailable")
    generated_at <- Sys.getenv("SOURCE_DATE_EPOCH", unset = "2026-07-15T00:00:00Z")
    metadata <- c(
        paste0("Generated-at-UTC: ", generated_at),
        paste0("Source-commit: ", commit[[1L]]),
        paste0("R: ", getRversion()),
        paste0("vegan: ", as.character(utils::packageVersion("vegan"))),
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
