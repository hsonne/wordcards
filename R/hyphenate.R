# hyphenate --------------------------------------------------------------------
hyphenate <- function(x)
{
  lower_x <- tolower(x)
  
  # For performance reasons, split only unique lower case words
  hyphenated <- split_into_syllables(full_words = unique(lower_x))
  
  if (is.null(hyphenated)) {
    return()
  }
  
  # Initialise result vector with lower case hyphenated versions of words in x
  indices <- match(lower_x, remove_hyphens(hyphenated))
  stopifnot(all(!is.na(indices)))
  result <- hyphenated[indices]
  
  # Restore original case
  is_upper <- is_upper_case(x)
  result[is_upper] <- to_upper_case(result[is_upper])
  
  # Return result vector
  result
}

# split_into_syllables ---------------------------------------------------------
#' @importFrom kwb.utils getAttribute
split_into_syllables <- function(full_words)
{
  # Has a word three or less characters and is thus a syllable?
  is_syllable <- nchar(full_words) <= 3L
  
  # Vector of short words consisting of one syllable
  short_words <- full_words[is_syllable]
  
  # Remaining words, to be split into syllables
  words <- full_words[!is_syllable]
  
  # Split words into parts of non-changing type of letter (consonant or vocal)
  word_data <- split_words(words, style = 1L)
  #View(word_data)
  
  # Split word_data into a set of data frames for each word
  all_sets <- split(word_data, word_data$word)
  
  # Identify syllables by going through list of predefined patterns  
  syllables_by_pattern <- find_syllables(sets = all_sets)
  
  n_unknown <- length(kwb.utils::getAttribute(syllables_by_pattern, "sets"))
  
  if (n_unknown > 0L) {
    
    message(
      "For ", n_unknown, " patterns, there are no split positions defined.\n",
      "In the opened file \"patterns_tmp.txt\", please insert hyphens at the ",
      "correct positions within the words. Then, save the file as ", 
      "\"patterns.txt\", run update_split_positions() and try again."
    )
    
    # Prepare split position assignments for non treated patterns  
    prepare_split_assignments_for_non_treated(x = syllables_by_pattern)
    
    return()
  }
  
  sorted_syllables <- sort(unname(unlist(syllables_by_pattern)))
  
  stopifnot(0L == length(
    setdiff(full_words, c(short_words, remove_hyphens(sorted_syllables)))
  ))
  
  write_lines_utf8(
    sort(c(short_words, sorted_syllables)), 
    path = "inst/extdata/output/syllables_tmp.txt"
  )
  
  result <- full_words
  
  i <- match(full_words, remove_hyphens(sorted_syllables))
  
  result[!is.na(i)] <- sorted_syllables[i[!is.na(i)]]
  
  result
}

# find_syllables ---------------------------------------------------------------
find_syllables <- function(sets)
{
  # Initialise result list
  syllables_by_pattern <- list()
  
  # Create type string representing sequence of consecutive vocals/consonants
  type_patterns <- sapply(sets, function(set) paste(set$type, collapse = "-"))
  
  # Get split positions per type pattern
  split_at <- read_split_positions()
  
  # Loop through the type patterns  
  for (pattern in names(split_at)) {
    
    # Which match the current pattern?
    is_match <- (type_patterns == pattern)
    
    if (any(is_match)) {
      
      # What are the corresponding words?
      matching_words <- names(sets[is_match])
      
      # Split the word at the positions defined in "split_at"
      syllables <- split_after(matching_words, split_at[[pattern]])
      
      # Save the syllables in the result list      
      syllables_by_pattern[[pattern]] <- syllables
      
      # Reduce word list and vector of type patterns
      sets <- sets[!is_match]
      type_patterns <- type_patterns[!is_match]
    }
  }
  
  # Return the result list, setting additional information as attributes
  structure(
    syllables_by_pattern,
    sets = sets,
    type_patterns = type_patterns
  )
}

# split_after ------------------------------------------------------------------
split_after <- function(x, i)
{
  stopifnot(is.character(x))
  
  if (length(x) == 0L || all(i == 0L)) {
    return(x)
  }
  
  n_char <- nchar(x)
  
  stopifnot(all(i < min(n_char)))
  
  from <- c(1L, i + 1L)
  to <- c(i, NA)
  
  paste_args <- lapply(seq_along(from), function(j) {
    substr(x, from[j], if (is.na(to[j])) n_char else to[j])
  })
  
  do.call(paste, c(paste_args, sep = "-"))
}

# prepare_split_assignments_for_non_treated ------------------------------------
#' @importFrom kwb.utils catAndRun getAttribute
#' @importFrom utils file.edit
prepare_split_assignments_for_non_treated <- function(x)
{
  # Non-handled words with corresponding patterns
  type_patterns <- kwb.utils::getAttribute(x, "type_patterns") 
  
  # Metadata for non-handled words
  sets <- kwb.utils::getAttribute(x, "sets")
  
  # Non-handled patterns, ordered by decreasing frequency
  patterns <- type_patterns %>%
    table() %>%
    sort(decreasing = TRUE) %>%
    names()
  
  if (is.null(patterns)) {
    return()
  }
  
  # For the non-handled patterns, find the matching words
  f <- factor(type_patterns, levels = patterns)
  matches <- split(names(type_patterns), f = f)
  
  split_assignments <- sapply(    
    USE.NAMES = FALSE,
    X = patterns[order(nchar(patterns), patterns)], 
    FUN = function(p) {
      sprintf("%s: %s", p, paste(matches[[p]], collapse = ","))
    }
  )
  
  file <- "patterns_tmp.txt"
  
  kwb.utils::catAndRun(
    paste0("Writing split position assignments to '", file, "'"),
    write_lines_utf8(split_assignments, path = file)
  )
  
  utils::file.edit(file)
}

# update_split_positions -------------------------------------------------------
update_split_positions <- function(file = "patterns.txt")
{
  # Modify "patterns_tmp.txt" by adding "-" within the words and save as
  # "patterns.txt"
  file %>%
    get_split_positions_from_pattern_file() %>%
    c(read_split_positions()) %>%
    order_split_positions() %>%
    write_split_positions()
}

# get_split_positions_from_pattern_file ----------------------------------------
#' @importFrom kwb.utils allAreIdentical extractSubstring readLinesWithEncoding
#' @importFrom stats setNames
get_split_positions_from_pattern_file <- function(file = "patterns.txt")
{
  substring_data <- kwb.utils::extractSubstring(
    pattern = "(^[^:]+):\\s+(.*)$", # "(^[^=]+)\\s+= 0L, #(.*)$"  
    x = kwb.utils::readLinesWithEncoding(file, fileEncoding = "UTF-8"), 
    index = c(pattern = 1L, words = 2L)
  )
  
  substring_data$words %>%
    strsplit("\\s*,\\s*") %>%
    stats::setNames(substring_data$pattern) %>%
    lapply(function(x) {
      print(x)
      positions <- lapply(strsplit(x, "-"), nchar)
      stopifnot(suppressMessages(kwb.utils::allAreIdentical(positions)))
      pos <- positions[[1L]]
      if (length(pos) > 1L) cumsum(pos)[-length(pos)] else 0L
    })
}

# read_split_positions ---------------------------------------------------------
#' @importFrom kwb.utils safePath
#' @importFrom yaml read_yaml
read_split_positions <- function(
    file = system.file("extdata/split-positions.yml", package = "wordcards")
)
{
  # To reorder the list after having added new patterns, copy the output of the
  # following command below into the body of this function:
  # cat_ordered_split_positions(read_split_positions())
  
  # TODO: handle exceptions
  # `1c-2v-1c` = 0L, "le-os" 
  # `1v-2c-1v-1c` = 2L, # "april" 
  # `1c-1v-3c-1v-2c` = 2L, # "wirklich"
  # `1c-1v-1c-1v-2c-1v-1c` = c(2L, 5L), # her-un-ter
  # `1c-1v-1c-1v-3c-1v-1c` = c(2L, 6L), # ko-mi-schen (sch)
  # `1c-1v-1c-2v-1c` = 3L, # ge-fiel
  # `1c-1v-2c-1v-3c` = 3L, # "gest<ae>rkt"
  # `1c-1v-3c-1v-1c` = 4L, # wi-scher (look for sch)
  # `1c-1v-4c-1v-1c` = 4L, # "menschen" (sch)
  # `2c-1v-2c-1v-1c` = 4L, # ch, ck: kra-chen ste-cken
  # `2c-1v-3c-1v-2c` = 5L, # "fr<ue>h-st<ue>ck"
  
  yaml::read_yaml(kwb.utils::safePath(file))
}

# order_split_positions --------------------------------------------------------
order_split_positions <- function(split_at)
{
  indices <- order(
    sapply(split_at, "[", 1L), 
    nchar(names(split_at)), 
    names(split_at)
  )
  
  split_at[indices]
}

# write_split_positions --------------------------------------------------------
#' @importFrom yaml write_yaml
write_split_positions <- function(
    split_positions, file = "split-positions.yml"
)
{
  yaml::write_yaml(split_positions, file)
}

# cat_ordered_split_positions --------------------------------------------------
cat_ordered_split_positions <- function(split_at)
{
  split_at %>%
    order_split_positions() %>%
    cat_split_positions()
}

# cat_split_positions ----------------------------------------------------------
#' @importFrom kwb.utils multiSubstitute objectToText
cat_split_positions <- function(x)
{
  x %>%
    kwb.utils::objectToText() %>%
    strsplit("\n") %>%
    do.call(what = c) %>%
    paste(collapse = " ") %>%
    kwb.utils::multiSubstitute(list(
      "\\s{2,}" = " ",
      ", `" = ",\n  `"
    )) %>%
    cat()
}

# output_split_positions -------------------------------------------------------
#' @importFrom yaml as.yaml
output_split_positions <- function(split_positions, as_yaml = FALSE)
{
  output <- if (as_yaml) {
    
    yaml::as.yaml(split_positions)
    
  } else {
    
    sprintf("%s = %s,", names(split_positions), sapply(
      split_positions, function(x) sprintf(
        if (length(x) > 1L) "c(%s)" else "%s", 
        paste0(x, "L", collapse = ", ")
      )
    ))
  }
  
  writeLines(output) 
}
