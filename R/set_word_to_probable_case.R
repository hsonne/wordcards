# set_word_to_probable_case ----------------------------------------------------
#' @importFrom kwb.utils selectColumns
set_word_to_probable_case <- function(word_table)
{
  words <- kwb.utils::selectColumns(word_table, "word")
  
  no_lower <- kwb.utils::selectColumns(word_table, "n_lower") == 0L
  
  words[no_lower] <- to_upper_case(words[no_lower])
  
  word_table[["word"]] <- words
  
  word_table
}
