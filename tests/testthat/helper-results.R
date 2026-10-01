miso_squish_result <- function(result) {
    text <- gsub("[[:space:]]+", " ", as.character(result$asString()))
    text <- gsub("&#39;", "'", text, fixed=TRUE)
    text <- gsub("&quot;", "\"", text, fixed=TRUE)
    text <- gsub("&lt;", "<", text, fixed=TRUE)
    text <- gsub("&gt;", ">", text, fixed=TRUE)
    gsub("&amp;", "&", text, fixed=TRUE)
}

# jmvcore Table cells are reference objects: rebuilding a table replaces its
# Cell objects, while in-place value refreshes keep them. Comparing a cell
# captured before a rerun with the cell after it detects structural rebuilds.
miso_table_first_cell <- function(table) {
    table$columns[[1L]]$.__enclos_env__$private$.cells[[1L]]
}

miso_array_first_item <- function(array) {
    array$items[[1L]]
}

miso_table_note <- function(table, key=NULL) {
    notes <- table$.__enclos_env__$private$.notes
    if (is.null(key))
        return(vapply(notes, function(note) note$note, character(1)))
    if (!key %in% names(notes))
        return("")
    notes[[key]]$note
}

# A native empty shell may have one blank row to refresh jamovi's table footer.
# Check absence of results rather than requiring a rowless table.
expect_miso_empty_table <- function(table) {
    cells <- unlist(table$asDF, use.names=FALSE)
    expect_true(all(is.na(cells) | cells == ""))
}

# Full descriptive numerical oracle retained in the analysis cache, rather
# than the removed hidden duplicate result table.
simper_test_full <- function(analysis) {
    rows <- if (inherits(analysis, "simperClass"))
        analysis$.__enclos_env__$private$.state$descriptive$fullRows
    else attr(analysis, "simperFullRows")
    columns <- c("contrast", "feature", "average", "sd", "ratio", "meanFirst",
        "meanSecond", "contribution", "cumulative")
    if (length(rows) == 0L)
        return(data.frame())
    out <- do.call(rbind, lapply(rows, function(row)
        as.data.frame(row[columns], stringsAsFactors=FALSE)))
    for (name in setdiff(columns, c("contrast", "feature")))
        out[[name]] <- as.numeric(out[[name]])
    rownames(out) <- paste0('"',seq_len(nrow(out)),'"')
    out
}

simper_test_run <- function(data, ...) {
    analysis <- simperClass$new(options=simperOptions$new(...), data=data)
    analysis$run()
    result <- analysis$results
    attr(result, "simperFullRows") <- analysis$.__enclos_env__$private$.state$descriptive$fullRows
    result
}
