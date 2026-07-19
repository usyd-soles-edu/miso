dune_fixture <- function() {
    data_env <- new.env(parent=emptyenv())
    utils::data("dune", package="vegan", envir=data_env)
    utils::data("dune.env", package="vegan", envir=data_env)

    community <- as.data.frame(data_env$dune)
    environment <- data_env$dune.env
    data <- data.frame(
        community,
        Management=environment$Management,
        Moisture=environment$Moisture,
        A1=environment$A1,
        check.names=FALSE)

    list(
        community=community,
        environment=environment,
        data=data,
        vars=names(community))
}

dune_tofu <- function(analysis, fixture, ...) {
    do.call(
        getExportedValue("tofu", analysis),
        c(list(data=fixture$data, vars=fixture$vars), list(...)))
}

dune_permutations <- function(n=99L) {
    permute::how(nperm=n)
}

dune_quiet <- function(expr) {
    suppressWarnings(suppressMessages(expr))
}

expect_dune_table_equal <- function(
        vegan_table,
        tofu_table,
        mapping,
        vegan_rows=seq_len(nrow(vegan_table)),
        tofu_rows=seq_len(nrow(tofu_table)),
        tolerance=1e-10) {
    for (vegan_name in names(mapping)) {
        expect_equal(
            as.numeric(tofu_table[tofu_rows, mapping[[vegan_name]]]),
            as.numeric(vegan_table[vegan_rows, vegan_name]),
            tolerance=tolerance,
            info=vegan_name)
    }
}

test_that("Module 3 transformation paths match vegan group tests", {
    fixture <- dune_fixture()
    transformations <- list(
        none=fixture$community,
        sqrt=sqrt(fixture$community),
        fourthroot=fixture$community ^ 0.25,
        pa=vegan::decostand(fixture$community, method="pa"))

    for (transform in names(transformations)) {
        distance <- vegan::vegdist(transformations[[transform]], method="bray")

        set.seed(123)
        vegan_permanova <- dune_quiet(vegan::adonis2(
            distance ~ Management,
            data=fixture$environment,
            permutations=dune_permutations(),
            by="terms"))
        tofu_permanova <- dune_quiet(dune_tofu(
            "permanova",
            fixture,
            factor="Management",
            transform=transform,
            seed=123,
            permN=99L))

        expect_equal(
            tofu_permanova$table$asDF$f[1],
            vegan_permanova$F[1],
            tolerance=1e-10,
            info=paste(transform, "PERMANOVA F"))
        expect_equal(
            tofu_permanova$table$asDF$p[1],
            vegan_permanova$`Pr(>F)`[1],
            info=paste(transform, "PERMANOVA p"))

        set.seed(123)
        vegan_anosim <- dune_quiet(vegan::anosim(
            distance,
            fixture$environment$Management,
            permutations=dune_permutations()))
        tofu_anosim <- dune_quiet(dune_tofu(
            "anosim",
            fixture,
            factor="Management",
            transform=transform,
            seed=123,
            anosimN=99L))

        expect_equal(
            tofu_anosim$global$asDF$value,
            unname(vegan_anosim$statistic),
            tolerance=1e-10,
            info=paste(transform, "ANOSIM R"))
        expect_equal(
            tofu_anosim$global$asDF$p,
            vegan_anosim$signif,
            info=paste(transform, "ANOSIM p"))
    }
})

test_that("Module 3 PERMANOVA designs match adonis2", {
    fixture <- dune_fixture()
    distance <- vegan::vegdist(fixture$community ^ 0.25, method="bray")
    mapping <- c(
        Df="df",
        SumOfSqs="sumsqs",
        R2="r2",
        F="f",
        `Pr(>F)`="p")

    set.seed(123)
    vegan_one_factor <- dune_quiet(vegan::adonis2(
        distance ~ Management,
        data=fixture$environment,
        permutations=dune_permutations(),
        by="terms"))
    tofu_one_factor <- dune_quiet(dune_tofu(
        "permanova",
        fixture,
        factor="Management",
        transform="fourthroot",
        seed=123,
        permN=99L))
    expect_dune_table_equal(
        vegan_one_factor,
        tofu_one_factor$table$asDF,
        mapping)

    set.seed(123)
    vegan_interaction <- dune_quiet(vegan::adonis2(
        distance ~ Management * Moisture,
        data=fixture$environment,
        permutations=dune_permutations(),
        by="terms"))
    tofu_interaction <- dune_quiet(dune_tofu(
        "permanova",
        fixture,
        factor="Management",
        permFactors="Moisture",
        permInteractions=TRUE,
        transform="fourthroot",
        seed=123,
        permN=99L,
        permBy="terms"))
    expect_equal(
        tofu_interaction$table$asDF$source,
        c("Management", "Moisture", "Management:Moisture", "Residual", "Total"))
    expect_dune_table_equal(
        vegan_interaction,
        tofu_interaction$table$asDF,
        mapping)

    set.seed(123)
    vegan_marginal <- dune_quiet(vegan::adonis2(
        distance ~ Management + Moisture,
        data=fixture$environment,
        permutations=dune_permutations(),
        by="margin"))
    tofu_marginal <- dune_quiet(dune_tofu(
        "permanova",
        fixture,
        factor="Management",
        permFactors="Moisture",
        transform="fourthroot",
        seed=123,
        permN=99L,
        permBy="margin"))
    expect_dune_table_equal(
        vegan_marginal,
        tofu_marginal$table$asDF,
        mapping)
})

test_that("Module 3 pairwise tests and PERMDISP match vegan", {
    fixture <- dune_fixture()
    group <- fixture$environment$Management
    distance <- vegan::vegdist(fixture$community ^ 0.25, method="bray")
    distance_matrix <- as.matrix(distance)
    pairs <- utils::combn(levels(group), 2L, simplify=FALSE)
    contrasts <- vapply(pairs, paste, character(1), collapse=" vs ")

    vegan_permanova <- vegan_anosim <- data.frame(
        contrast=contrasts,
        statistic=NA_real_,
        p=NA_real_)
    for (i in seq_along(pairs)) {
        keep <- group %in% pairs[[i]]
        pair_group <- droplevels(group[keep])
        pair_distance <- stats::as.dist(distance_matrix[keep, keep, drop=FALSE])

        set.seed(123)
        permanova_fit <- dune_quiet(vegan::adonis2(
            pair_distance ~ pair_group,
            permutations=dune_permutations(),
            by="terms"))
        vegan_permanova$statistic[i] <- permanova_fit$F[1]
        vegan_permanova$p[i] <- permanova_fit$`Pr(>F)`[1]

        set.seed(123)
        anosim_fit <- dune_quiet(vegan::anosim(
            pair_distance,
            pair_group,
            permutations=dune_permutations()))
        vegan_anosim$statistic[i] <- unname(anosim_fit$statistic)
        vegan_anosim$p[i] <- anosim_fit$signif
    }
    vegan_permanova$padj <- stats::p.adjust(vegan_permanova$p, method="holm")
    vegan_anosim$padj <- stats::p.adjust(vegan_anosim$p, method="holm")

    tofu_permanova <- dune_quiet(dune_tofu(
        "permanova",
        fixture,
        factor="Management",
        transform="fourthroot",
        seed=123,
        permN=99L,
        permPairwise=TRUE,
        permAdjust="holm"))$pairwise$asDF
    expect_equal(tofu_permanova$contrast, vegan_permanova$contrast)
    expect_equal(tofu_permanova$f, vegan_permanova$statistic, tolerance=1e-10)
    expect_equal(tofu_permanova$p, vegan_permanova$p)
    expect_equal(tofu_permanova$padj, vegan_permanova$padj)

    tofu_anosim <- dune_quiet(dune_tofu(
        "anosim",
        fixture,
        factor="Management",
        transform="fourthroot",
        seed=123,
        anosimN=99L,
        anosimPairwise=TRUE,
        anosimAdjust="holm"))$pairwise$asDF
    expect_equal(tofu_anosim$contrast, vegan_anosim$contrast)
    expect_equal(tofu_anosim$r, vegan_anosim$statistic, tolerance=1e-10)
    expect_equal(tofu_anosim$p, vegan_anosim$p)
    expect_equal(tofu_anosim$padj, vegan_anosim$padj)

    set.seed(123)
    vegan_dispersion <- dune_quiet(vegan::betadisper(
        distance,
        group,
        type="median"))
    vegan_permdisp <- dune_quiet(vegan::permutest(
        vegan_dispersion,
        permutations=dune_permutations()))$tab
    tofu_permdisp <- dune_quiet(dune_tofu(
        "permdisp",
        fixture,
        factor="Management",
        transform="fourthroot",
        seed=123,
        permN=99L))$anova$asDF
    expect_dune_table_equal(
        vegan_permdisp,
        tofu_permdisp,
        c(Df="df", `Sum Sq`="sumsqs", `Mean Sq`="meansq", F="f", `Pr(>F)`="p"))
})

test_that("Module 3 nMDS and environmental fit match vegan", {
    fixture <- dune_fixture()
    transformed <- fixture$community ^ 0.25

    set.seed(123)
    vegan_nmds <- dune_quiet(vegan::metaMDS(
        transformed,
        distance="bray",
        k=2L,
        trymax=20L,
        maxit=200L,
        wascores=FALSE,
        autotransform=FALSE,
        trace=FALSE))
    vegan_sites <- vegan::scores(
        vegan_nmds,
        display="sites",
        choices=1:2)
    vegan_envfit <- dune_quiet(vegan::envfit(
        vegan_sites,
        data.frame(
            value=fixture$environment$A1,
            row.names=rownames(fixture$community)),
        permutations=99L,
        choices=1:2))

    tofu_nmds <- dune_quiet(dune_tofu(
        "nmds",
        fixture,
        factor="Management",
        transform="fourthroot",
        seed=123,
        nmdsK=2L,
        nmdsTrymax=20L,
        nmdsMaxit=200L,
        nmdsEnv="A1",
        nmdsEnvPerm=99L))
    tofu_sites <- as.matrix(tofu_nmds$sites$asDF[, c("NMDS1", "NMDS2")])
    tofu_stress <- as.numeric(tofu_nmds$stress$asDF$value[
        tofu_nmds$stress$asDF$item == "Stress"])

    expect_equal(tofu_stress, round(vegan_nmds$stress, 4L))
    expect_equal(
        as.vector(stats::dist(tofu_sites)),
        as.vector(stats::dist(vegan_sites)),
        tolerance=1e-10)
    expect_equal(
        tofu_nmds$envfit$asDF$r2,
        unname(vegan_envfit$vectors$r),
        tolerance=1e-10)
    expect_equal(
        tofu_nmds$envfit$asDF$p,
        unname(vegan_envfit$vectors$pvals))
})

test_that("Module 3 SIMPER summaries match vegan", {
    fixture <- dune_fixture()
    transformed <- fixture$community ^ 0.25
    group <- fixture$environment$Management
    vegan_simper <- dune_quiet(vegan::simper(
        transformed,
        group,
        permutations=0L))
    vegan_summary <- summary(vegan_simper)
    tofu_simper <- dune_quiet(dune_tofu(
        "simper",
        fixture,
        factor="Management",
        transform="fourthroot",
        seed=123,
        simperTop=10L,
        simperCum=70,
        simperDetails=TRUE))

    contributions <- tofu_simper$contributions$asDF
    variability <- tofu_simper$variability$asDF
    means <- tofu_simper$means$asDF
    contrasts <- tofu_simper$contrasts$asDF
    contrast_keys <- names(vegan_simper)
    contrast_labels <- gsub("_", " vs ", contrast_keys, fixed=TRUE)

    expect_equal(contrasts$contrast, contrast_labels)
    expect_equal(nrow(contributions), 10L * length(contrast_keys))
    for (i in seq_along(contrast_keys)) {
        key <- contrast_keys[[i]]
        label <- contrast_labels[[i]]
        contribution_rows <- which(contributions$contrast == label)
        variation_rows <- match(
            paste(contributions$contrast[contribution_rows], contributions$feature[contribution_rows]),
            paste(variability$contrast, variability$feature))
        mean_rows <- match(
            paste(contributions$contrast[contribution_rows], contributions$feature[contribution_rows]),
            paste(means$contrast, means$feature))
        species_rows <- match(
            contributions$feature[contribution_rows],
            rownames(vegan_summary[[key]]))
        expected <- vegan_summary[[key]][species_rows, , drop=FALSE]
        expected_ratio <- expected$ratio
        expected_ratio[! is.finite(expected_ratio)] <- NA_real_

        expect_false(anyNA(species_rows), info=label)
        expect_equal(
            contributions$contribution[contribution_rows],
            100 * expected$average / vegan_simper[[key]]$overall,
            tolerance=1e-10,
            info=paste(label, "contribution"))
        expect_equal(
            contributions$cumulative[contribution_rows],
            100 * expected$cumsum,
            tolerance=1e-10,
            info=paste(label, "cumulative"))
        expect_equal(variability$average[variation_rows], expected$average, tolerance=1e-10)
        expect_equal(variability$sd[variation_rows], expected$sd, tolerance=1e-10)
        expect_equal(variability$ratio[variation_rows], expected_ratio, tolerance=1e-10)
        expect_equal(means$meanFirst[mean_rows], expected$ava, tolerance=1e-10)
        expect_equal(means$meanSecond[mean_rows], expected$avb, tolerance=1e-10)

        contrast_row <- match(label, contrasts$contrast)
        expect_equal(
            contrasts$overall[contrast_row],
            vegan_simper[[key]]$overall,
            tolerance=1e-10)
    }
})
