construct_query_text <- function(query_text_start, query_text) {
  if (length(query_text) == 0) return(query_text_start)
  
  conditions <- paste(query_text, collapse = " AND ")
  
  paste(
    query_text_start,
    "WHERE",
    conditions
  )
}