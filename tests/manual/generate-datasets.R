#!/usr/bin/env Rscript

set_reproducible_rng <- function(seed) {
    RNGkind("Mersenne-Twister", "Inversion", "Rejection")
    RNGversion("4.0.0")
    set.seed(seed)
}

parse_output_dir <- function(args = commandArgs(trailingOnly = TRUE)) {
    output_arg <- grep("^--output-dir=", args, value = TRUE)
    if (length(output_arg) > 1L)
        stop("Use only one --output-dir argument.")
    if (length(output_arg) == 1L)
        return(sub("^--output-dir=", "", output_arg[[1L]]))
    if ("--write" %in% args)
        return(file.path("tests", "manual"))
    stop("Refusing to overwrite fixtures. Use --write or --output-dir=<path>.")
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

make_small_data <- function() {
    set_reproducible_rng(20260715)

    design <- expand.grid(
        block = sprintf("B%02d", 1:4),
        treatment = c("Control", "Impact"),
        group = c("A", "B", "C"),
        KEEP.OUT.ATTRS = FALSE,
        stringsAsFactors = FALSE
    )
    design <- design[order(design$block, design$treatment, design$group), ]
    rownames(design) <- NULL

    base <- c(7, 5, 4, 8, 3, 6, 5, 2)
    group_multiplier <- rbind(
        A = c(1.75, 1.55, 1.30, 0.65, 0.70, 0.80, 1.00, 0.90),
        B = c(0.80, 0.95, 1.05, 1.75, 1.50, 1.25, 0.85, 1.05),
        C = c(0.65, 0.75, 0.85, 0.80, 1.05, 1.15, 1.80, 1.65)
    )
    treatment_multiplier <- c(1.00, 1.35, 1.00, 0.80, 1.25, 1.00, 1.30, 0.85)
    block_multiplier <- c(B01 = 0.92, B02 = 1.00, B03 = 1.08, B04 = 1.16)
    sample_dispersion <- ifelse(
        design$group == "C",
        exp(rnorm(nrow(design), 0, 0.48)),
        ifelse(design$group == "B", exp(rnorm(nrow(design), 0, 0.20)), 1)
    )

    features <- vapply(seq_along(base), function(j) {
        lambda <- base[[j]] *
            group_multiplier[design$group, j] *
            ifelse(design$treatment == "Impact", treatment_multiplier[[j]], 1) *
            block_multiplier[design$block] * sample_dispersion
        rpois(nrow(design), lambda = pmax(lambda, 0.1))
    }, integer(nrow(design)))
    small_offsets <- matrix(
        rep(seq(0.01, 0.08, by = 0.01), each = nrow(features)),
        nrow = nrow(features)
    )
    features <- features + (features > 0) * small_offsets
    colnames(features) <- sprintf("feature_%02d", seq_len(ncol(features)))

    block_number <- as.integer(sub("B", "", design$block, fixed = TRUE))
    temperature <- round(
        18 + c(A = -1.2, B = 0.4, C = 1.3)[design$group] +
            ifelse(design$treatment == "Impact", 0.8, -0.2) +
            0.25 * block_number + rnorm(nrow(design), 0, 0.65),
        2
    )
    pH <- round(
        7.3 + c(A = 0.25, B = -0.10, C = -0.30)[design$group] +
            ifelse(design$treatment == "Impact", -0.12, 0.08) +
            rnorm(nrow(design), 0, 0.12),
        2
    )

    data.frame(
        sample_id = sprintf("S%03d", seq_len(nrow(design))),
        group = design$group,
        treatment = design$treatment,
        block = design$block,
        temperature = temperature,
        pH = pH,
        features,
        check.names = FALSE,
        stringsAsFactors = FALSE
    )
}

make_large_data <- function() {
    set_reproducible_rng(20260716)

    design <- expand.grid(
        replicate = seq_len(10),
        block = sprintf("B%02d", 1:6),
        treatment = c("Control", "Impact"),
        group = c("A", "B", "C"),
        KEEP.OUT.ATTRS = FALSE,
        stringsAsFactors = FALSE
    )
    design <- design[order(design$block, design$replicate, design$treatment, design$group), ]
    rownames(design) <- NULL

    feature_count <- 48L
    base <- round(exp(seq(log(1.5), log(20), length.out = feature_count)), 3)
    group_preference <- rep(c("A", "B", "C"), length.out = feature_count)
    treatment_direction <- rep(c(1.35, 0.75, 1.00, 1.18), length.out = feature_count)
    block_number <- as.integer(sub("B", "", design$block, fixed = TRUE))
    block_multiplier <- 0.90 + 0.035 * block_number
    replicate_wave <- 0.92 + 0.10 * sin(design$replicate * pi / 5)
    group_size <- c(A = 30, B = 12, C = 3.5)

    features <- vapply(seq_len(feature_count), function(j) {
        preferred <- ifelse(design$group == group_preference[[j]], 1.85, 0.78)
        secondary <- ifelse(
            design$group == c(A = "B", B = "C", C = "A")[[group_preference[[j]]]],
            1.12,
            1
        )
        treatment <- ifelse(design$treatment == "Impact", treatment_direction[[j]], 1)
        mu <- base[[j]] * preferred * secondary * treatment * block_multiplier * replicate_wave
        rnbinom(nrow(design), mu = pmax(mu, 0.1), size = group_size[design$group])
    }, numeric(nrow(design)))
    large_offsets <- matrix(
        rep(seq(0.001, 0.048, by = 0.001), each = nrow(features)),
        nrow = nrow(features)
    )
    features <- features + (features > 0) * large_offsets
    colnames(features) <- sprintf("feature_%02d", seq_len(ncol(features)))

    temperature <- round(
        16.5 + c(A = -0.8, B = 0.5, C = 1.4)[design$group] +
            ifelse(design$treatment == "Impact", 0.7, -0.1) +
            0.18 * block_number + 0.04 * design$replicate +
            rnorm(nrow(design), 0, 0.9),
        2
    )
    pH <- round(
        7.5 + c(A = 0.22, B = -0.05, C = -0.28)[design$group] +
            ifelse(design$treatment == "Impact", -0.10, 0.06) -
            0.015 * block_number + rnorm(nrow(design), 0, 0.16),
        2
    )

    data.frame(
        sample_id = sprintf("L%04d", seq_len(nrow(design))),
        group = design$group,
        treatment = design$treatment,
        block = design$block,
        temperature = temperature,
        pH = pH,
        features,
        check.names = FALSE,
        stringsAsFactors = FALSE
    )
}

make_invalid_data <- function() {
    data.frame(
        sample_id = sprintf("I%02d", 1:8),
        group = rep(c("A", "B"), each = 4),
        single_group = rep("A", 8),
        valid_01 = c(4.1, 5.2, 6.3, 4.4, 8.5, 9.6, 7.7, 8.8),
        valid_02 = c(8.2, 7.3, 9.4, 6.5, 3.6, 4.7, 2.8, 3.9),
        negative_feature = c(1.1, 2.2, -1.3, 3.4, 4.5, 5.6, 6.7, 7.8),
        missing_feature = c(2.1, 3.2, 4.3, NA, 5.5, 6.6, 7.7, 8.8),
        all_zero_feature = rep(0, 8),
        zero_case_01 = c(2.1, 3.2, 4.3, 5.4, 6.5, 7.6, 8.7, 0),
        zero_case_02 = c(3.2, 2.3, 5.4, 4.5, 7.6, 6.7, 9.8, 0),
        text_feature = c("low", "low", "medium", "medium", "high", "high", "high", "low"),
        check.names = FALSE,
        stringsAsFactors = FALSE
    )
}

feature_columns <- function(x) grep("^feature_[0-9]+$", names(x), value = TRUE)

assert_clean_dataset <- function(x, expected_rows, expected_features, repeats_per_cell) {
    stopifnot(nrow(x) == expected_rows)
    features <- feature_columns(x)
    stopifnot(length(features) == expected_features)
    stopifnot(!anyDuplicated(x$sample_id))
    stopifnot(all(vapply(x[features], is.numeric, logical(1))))
    stopifnot(all(as.matrix(x[features]) >= 0))
    stopifnot(!any(rowSums(x[features]) == 0))
    stopifnot(!any(colSums(x[features]) == 0))
    balance <- xtabs(~ group + treatment + block, data = x)
    stopifnot(all(balance == repeats_per_cell))

    model <- model.matrix(~ group * treatment + block + temperature + pH, data = x)
    stopifnot(qr(model)$rank == ncol(model))

    if (requireNamespace("vegan", quietly = TRUE)) {
        distance <- vegan::vegdist(x[features], method = "bray")
        stopifnot(all(is.finite(distance)))
        stopifnot(all(distance > 0))
        pa_distance <- vegan::vegdist(x[features], method = "jaccard", binary = TRUE)
        stopifnot(all(is.finite(pa_distance)))
        stopifnot(any(pa_distance > 0))
        dispersion <- vegan::betadisper(distance, factor(x$group), type = "median")
        means <- tapply(dispersion$distances, x$group, mean)
        stopifnot(isTRUE(means[["C"]] > means[["A"]]))
        stopifnot(isTRUE(means[["C"]] > means[["B"]]))
    }
    invisible(TRUE)
}

generate_datasets <- function(output_dir) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    small <- make_small_data()
    large <- make_large_data()
    invalid <- make_invalid_data()

    assert_clean_dataset(small, 24L, 8L, 1L)
    assert_clean_dataset(large, 360L, 48L, 10L)

    write_stable_csv(small, file.path(output_dir, "tofu-small.csv"))
    write_stable_csv(large, file.path(output_dir, "tofu-large.csv"))
    write_stable_csv(invalid, file.path(output_dir, "tofu-invalid.csv"))

    cat("tofu-small.csv: 24 rows, 8 features\n")
    cat("tofu-large.csv: 360 rows, 48 features\n")
    cat("tofu-invalid.csv: 8 rows\n")
    cat("dataset assertions: PASS\n")
    invisible(list(small = small, large = large, invalid = invalid))
}

if (sys.nframe() == 0L)
    generate_datasets(parse_output_dir())
