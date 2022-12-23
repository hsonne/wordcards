# syllables_to_syllable_table --------------------------------------------------
syllables_to_syllable_table <- function(hyphenated, frequencies)
{
  syllables_in_words <- lapply(
    X = hyphenated, 
    FUN = split_hyphenated, 
    hyphen = "-"
  )
  
  syllable_counts <- unlist(lapply(
    seq_along(syllables_in_words),
    function(i) table(syllables_in_words[[i]]) * frequencies[i]
  ))
  
  # Sum up the counts for identical syllables
  unique_syllable_counts <- sapply(
    X = split(syllable_counts, names(syllable_counts)), 
    FUN = sum
  )
  
  unique_syllables <- names(unique_syllable_counts)
  
  stopifnot(!anyDuplicated(unique_syllables))
  
  syllable_type_matrix <- unique_syllable_counts * cbind(
    prefixes = is_prefix(unique_syllables),
    infixes = is_infix(unique_syllables),
    suffixes = is_suffix(unique_syllables),
    words = is_word(unique_syllables)
  )
  
  data.frame(
    syllable = unique_syllables,
    syllable_type_matrix,
    total = rowSums(syllable_type_matrix),
    row.names = NULL
  )
}

# split_hyphenated -------------------------------------------------------------
split_hyphenated <- function(x, hyphen = "")
{
  stopifnot(length(x) == 1L)
  
  parts <- strsplit(x, "-")[[1L]]
  
  if (hyphen == "" || length(parts) == 1L) {
    return(parts)
  }
  
  add_hyphens(parts, hyphen)
}
