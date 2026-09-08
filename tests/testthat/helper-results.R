miso_squish_result <- function(result) {
    text <- gsub("[[:space:]]+", " ", as.character(result$asString()))
    text <- gsub("&#39;", "'", text, fixed=TRUE)
    text <- gsub("&quot;", "\"", text, fixed=TRUE)
    text <- gsub("&lt;", "<", text, fixed=TRUE)
    text <- gsub("&gt;", ">", text, fixed=TRUE)
    gsub("&amp;", "&", text, fixed=TRUE)
}
