# determine_type_pattern -------------------------------------------------------
determine_type_pattern <- function(word)
{
  paste(split_words(word)$type, collapse = "-")
}
