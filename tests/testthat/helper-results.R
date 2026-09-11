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
