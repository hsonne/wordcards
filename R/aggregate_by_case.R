# aggregate_by_case ------------------------------------------------------------
#' @importFrom kwb.utils defaultIfNA mergeAll removeColumns selectElements
#' @importFrom stats setNames
aggregate_by_case <- function(data, column_word = "word")
{
  # Save the words in original case
  words <- kwb.utils::selectElements(data, column_word)
  
  stopifnot(!anyDuplicated(words))
  
  # Set all words in the data frame to lower case
  data[[column_word]] <- tolower(data[[column_word]])
  
  # Split data frame into two subsets. The first contains the rows belonging to
  # lower case words, the second contains the rows belonging to upper case words
  by_case <- split(data, factor(
    is_upper_case(words), 
    levels = c("FALSE", "TRUE"), 
    labels = c("lower", "upper")
  ))
  
  # Aggregate the original table by the (lower case) words so that upper case
  # and lower case frequencies are summed up
  result <- data %>%
    kwb.utils::removeColumns(column_word) %>%
    aggregate(by = data[column_word], FUN = sum) %>%
    # Prepare argument list for mergeAll()
    list() %>%
    stats::setNames("total") %>%
    c(by_case) %>%
    # Merge "total" frequencies from aggregation with "by_case"-frequencies
    kwb.utils::mergeAll(by = column_word, all = TRUE, dbg = FALSE)
  
  result %>%
    # Replace "." in column names (created by mergeAll()) with underscore
    stats::setNames(gsub("\\.", "_", names(result))) %>%
    # In integer columns, set all NA to 0 (0L = integer constant)
    lapply(function(x) {
      if (is.integer(x)) kwb.utils::defaultIfNA(x, 0L) else x
    }) %>%
    # Make sure that the result is a data frame again
    data.frame()
}
