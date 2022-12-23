# aggregate_syllable_data ------------------------------------------------------
#' @importFrom kwb.utils defaultIfNA multiSubstitute removeColumns
#' @importFrom stats aggregate setNames
aggregate_syllable_data <- function(syllable_data)
{
  # Aggregate by pure syllable (without hyphens)
  pure_syllable_data <- stats::aggregate(
    syllable_data[-1L], 
    by = list(syllable = remove_hyphens(syllable_data$syllable)),
    FUN = sum
  )
  
  #View(pure_syllable_data)
  
  syllable_stats <- aggregate_by_case(
    data = pure_syllable_data, 
    column_word = "syllable"
  )
  
  compressed_stats <- syllable_stats %>%
    kwb.utils::removeColumns(grep(
      pattern = "syllable|total", 
      x = names(syllable_stats), 
      value = TRUE
    )) %>%
    apply(1L, function(x) x[kwb.utils::defaultIfNA(x, 0L) > 0L]) %>%
    stats::setNames(syllable_stats$syllable)
  
  lapply(stats::setNames(nm = names(compressed_stats)), function(name) {
    #name <- names(compressed_stats)[1L]
    x <- sort(compressed_stats[[name]], decreasing = TRUE)
    lower <- tolower(name)
    upper <- to_upper_case(name)
    suffix <- function(x) paste0("-", x)
    prefix <- function(x) paste0(x, "-")
    infix <- function(x) paste0("-", x, "-")
    stats::setNames(x, kwb.utils::multiSubstitute(names(x), list(
      words_lower = lower,
      words_upper = upper,
      prefixes_lower = prefix(lower),
      prefixes_upper = prefix(upper),
      infixes_lower = infix(lower),
      infixes_upper = infix(upper),
      suffixes_lower = suffix(lower),
      suffixes_upper = suffix(upper)
    )))
  })
}
