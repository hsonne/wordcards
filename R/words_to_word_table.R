# words_to_word_table ----------------------------------------------------------
#' @importFrom stats setNames
words_to_word_table <- function(words)
{
  stats <- table(words) %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    stats::setNames(c("word", "frequency"))
  
  cbind(nchar = nchar(stats$word), stats) %>%
    order_by_frequency_and_word()
}


# order_by_frequency_and_word --------------------------------------------------
#' @importFrom kwb.utils orderBy
order_by_frequency_and_word <- function(
    data, 
    column_frequency = "frequency",
    column_word = "word"
)
{
  kwb.utils::orderBy(
    data, 
    by = c(column_frequency, column_word), 
    decreasing = c(TRUE, FALSE),
    method = "radix"
  )
}
