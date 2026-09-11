test_that("empty successful descriptions produce no extra HTML heading", {
    expect_identical(miso_html_block(character(),
        title="An existing image title", ariaLabel="About the image"), "")
    explanation <- miso_html_block("Two axes are unavailable.",
        title="Ordination", ariaLabel="About the image")
    expect_match(explanation, "Two axes are unavailable.", fixed=TRUE)
    expect_match(explanation, 'aria-label="About the image"', fixed=TRUE)
})

test_that("a valid binary distance is method information rather than a warning", {
    data <- data.frame(a=c(1, 2, 0, 4), b=c(0, 1, 3, 2))
    prep <- miso_prepare_resemblance(data=data, vars=c("a", "b"),
        transform="none", distance="bray", seed=123,
        requireFactor=FALSE, distBinary=TRUE)
    expect_false(isTRUE(prep$error))
    expect_false(any(grepl("abundance magnitudes ignored", prep$warnings,
        fixed=TRUE)))
    expect_equal(as.numeric(prep$dist),
        as.numeric(vegan::vegdist(data, method="bray", binary=TRUE)))
})
