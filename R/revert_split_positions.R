# revert_split_positions -------------------------------------------------------
revert_split_positions <- function(split_positions)
{
  patterns <- names(split_positions)

  length_lookup <- get_unique_sub_pattern_lengths(patterns)

  word_lengths <- sapply(split_patterns(patterns), function(x) {
    sum(length_lookup[x])
  })
  
  reverted <- lapply(seq_along(split_positions), function(i) {
    if (identical(split_positions[[i]], 0L)) {
      0L
    } else {
      rev(word_lengths[i] - split_positions[[i]])
    }
  })
  
  stats::setNames(reverted, patterns)
}

# get_unique_sub_pattern_lengths -----------------------------------------------
#' @importFrom stats setNames
get_unique_sub_pattern_lengths <- function(patterns)
{
  sub_patterns <- sort(unique(unlist(split_patterns(patterns))))
  sub_pattern_lengths <- sub_pattern_length(sub_patterns)
  stats::setNames(sub_pattern_lengths, sub_patterns)
}

# split_patterns ---------------------------------------------------------------
split_patterns <- function(patterns)
{
  strsplit(patterns, "-")
}

# sub_pattern_length -----------------------------------------------------------
sub_pattern_length <- function(x)
{
  stopifnot(is.character(x))

  numbers <- kwb.utils::extractSubstring("^(\\d+)", x, 1L)
  has_number <- nzchar(numbers)
  
  is_two_letter_special <- x %in% c("DT", "CH", "CK")
  is_sch <- x == "SCH"
  is_sz <- x == "SZ"
  
  result <- integer(length(x))
  result[has_number] <- as.integer(numbers[has_number])
  result[is_two_letter_special | is_sz] <- 2L
  result[is_sch] <- 3L
  
  stopifnot(!any(result == 0L))
  
  result
}
