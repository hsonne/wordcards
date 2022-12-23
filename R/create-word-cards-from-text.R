# kwb.utils

# read_text --------------------------------------------------------------------
#' @importFrom kwb.utils readLinesWithEncoding selectElements
read_text <- function(name)
{
  list_available_text_files() %>%
    kwb.utils::selectElements(name) %>%
    kwb.utils::readLinesWithEncoding(fileEncoding = "UTF-8") %>%
    remove_comment_lines()
}

# list_available_text_files ----------------------------------------------------
#' @importFrom kwb.utils removeExtension
#' @importFrom stats setNames
list_available_text_files <- function()
{
  path <- system.file("extdata/texts", package = "wordcards")
  files <- as.list(dir(path, "\\.txt$", full.names = TRUE))
  stats::setNames(files, kwb.utils::removeExtension(basename(unlist(files))))
}

# remove_comment_lines ---------------------------------------------------------
remove_comment_lines <- function(x)
{
  grep("^#", x, invert = TRUE, value = TRUE)
}

# text_to_words ----------------------------------------------------------------
#' @importFrom kwb.utils removeEmpty2
text_to_words <- function(raw_text)
{
  raw_text %>%
    clean_text() %>%
    paste(collapse = " ") %>%
    strsplit("\\s+") %>%
    `[[`(1L) %>%
    kwb.utils::removeEmpty2()
}

# clean_text -------------------------------------------------------------------
#' @importFrom kwb.utils multiSubstitute
clean_text <- function(raw_text)
{
  kwb.utils::multiSubstitute(raw_text, list(
    "[^A-Za-zÄÖÜäöüß]" = ".",
    "[.]+" = " "
  ))
}

# guess_nouns_with_articles ----------------------------------------------------
guess_nouns_with_articles <- function(words)
{
  i <- which(is_article(words))
  
  is_noun <- i < length(words) & words[i + 1L] != tolower(words[i + 1L])
  
  j <- i[is_noun]
  
  articles <- words[j]
  
  nouns <- words[j + 1L]
  
  articles_per_noun <- split(tolower(articles), to_upper_case(nouns))
  
  article_guesses <- sapply(articles_per_noun, function(x) {
    names(sort(table(x), decreasing = TRUE))[1L]
  })
  
  article_guesses[order(names(article_guesses))]
  
  paste(article_guesses, names(article_guesses))
}

# is_article -------------------------------------------------------------------
is_article <- function(word)
{
  tolower(word) %in% c("der", "die", "das")
}

# correct_article_guesses ------------------------------------------------------
correct_article_guesses <- function(article_guesses)
{
  corrections <- c(
    "die Arbeitsplatte", 
    "die Arztpraxis", 
    "die Diele",
    "die Küche",
    "die Praxis", 
    "die Reihe",
    "die Spitze",
    "die Spüle", 
    "die Stube",
    "die Tulpen", 
    "die Untersuchung", 
    "die Zwischenzeit"
  )
  
  get_noun <- function(x) sapply(strsplit(x, " "), "[", 2L)
  
  i <- match(get_noun(article_guesses), get_noun(corrections))
  
  is_match <- !is.na(i)
  
  article_guesses[is_match] <- corrections[i[is_match]]
  
  article_guesses
}

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

# plot_word_cards --------------------------------------------------------------
#' @importFrom kwb.plot bestRowColumnSetting
#' @importFrom kwb.utils toPdf 
plot_word_cards <- function(
    words,
    frequencies = NULL,
    file = NULL,
    per_page = 32L, 
    ...
)
{
  kwb.utils::toPdf(pdfFile = file, landscape = FALSE, expressions = {
    
    mfrow <- kwb.plot::bestRowColumnSetting(per_page, target.ratio = 0.71)
    
    graphics::par(mfrow = mfrow, mar = c(0, 0, 0, 0))
    
    for (i in seq_along(words)) {
  
      word <- words[i]
      
      if (is.null(frequencies)) {
        plot_word_card(word, i, freq = NULL, ...)
      } else {
        plot_word_card(word, i, freq = frequencies[i], ...)
      }
    }
  })
}

# plot_word_card ---------------------------------------------------------------
plot_word_card <- function(
    word, 
    i,
    freq = NULL,
    cex = 2, 
    y = 0.3, 
    plot_rank = TRUE,
    plot_nchar = TRUE,
    both_cases = TRUE,
    label_types = c(1L, 1L, 1L)
)
{
  texts <- if (isTRUE(both_cases)) {
    # Write word in lower case and upper case
    c(word, to_upper_case(word))
  } else {
    # Write word in original case
    word
  }
  
  footer <- c(
    if (isTRUE(plot_rank)) label_rank(i, label_types[1L]) else NA,
    if (!is.null(freq)) label_times(freq, label_types[2L]) else NA,
    if (isTRUE(plot_nchar)) label_nchar(nchar(word)) else label_types[3L]
  )
  
  plot_card(texts, footer, cex = cex)
}

# plot_card --------------------------------------------------------------------
#' @importFrom graphics text
plot_card <- function(
    texts, footer = c(NA, NA, NA), ylim = c(-1, 1), squeeze = 0.3, cex = 1, 
    cex.footer = 1
)
{
  text_footer <- function(text, x, y = -0.9) {
    if (!is.na(text)) {
      graphics::text(x, y, text, cex = cex.footer)
    }
  }
  
  init_empty_plot()
  
  y <- positions_between(length(texts), squeeze * ylim)
  graphics::text(0, rev(y), texts, cex = cex)
  
  text_footer(footer[1L], x = -0.9)
  text_footer(footer[2L], x =  0.0)
  text_footer(footer[3L], x = +0.9)
}

# init_empty_plot --------------------------------------------------------------
init_empty_plot <- function(xlim = c(-1, 1), ylim = c(-1, 1))
{
  plot(
    x = 0, 
    y = 0, 
    type = "n", 
    xlab = "", 
    ylab = "", 
    xaxt = "n", 
    yaxt = "n",
    xlim = xlim, 
    ylim = ylim
  )
}

# positions_between ------------------------------------------------------------
positions_between <- function(n, limits = c(-1, 1))
{
  if (n == 1L) {
    mean(limits)
  } else {
    seq(limits[1L], by = diff(limits)/(n - 1), length.out = n)
  }
}

# label_rank -------------------------------------------------------------------
label_rank <- function(x, type = 2L)
{
  if (type == 1L) return(sprintf("#%d", x))
  if (type == 2L) return(sprintf("%d.", x))
}

# label_times ------------------------------------------------------------------
label_times <- function(x, type = 2L)
{
  if (type == 1L) return(sprintf("%d-mal", x))
  if (type == 2L) return(sprintf("%dx", x))
}

# label_nchar ------------------------------------------------------------------
label_nchar <- function(x, type = 2L)
{
  if (type == 1L) return(sprintf("%der", x))
  if (type == 2L) return(sprintf("%d", x))
}

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

# split_words ------------------------------------------------------------------
#' @importFrom kwb.utils findChanges moveColumnsToFront rbindAll removeColumns
#' @importFrom kwb.utils repeated renameColumns
#' @importFrom stats setNames
split_words <- function(words, style = 1L)
{
  word_chars <- stats::setNames(strsplit(tolower(words), ""), words)
  
  word_parts <- lapply(word_chars, function(chars) {
    changes <- kwb.utils::findChanges(is_vowel(chars))
    changes$part <- sapply(seq_len(nrow(changes)), function(i) {
      paste0(chars[changes$starts_at[i]:changes$ends_at[i]], collapse = "")
    })
    changes
  })
  
  word_data <- word_parts %>%
    kwb.utils::rbindAll(nameColumn = "word") %>%
    kwb.utils::renameColumns(list(starts_at = "from", ends_at = "to"))
  
  word_data$nchar <- word_data$to - word_data$from + 1L
  
  type_name <- function(n, type) {
    if (style == 1L) {
      paste0(n, type)
    } else if (style == 2L) {
      sapply(n, kwb.utils::repeated, x = type)
    }
  }
  
  word_data$type <- ifelse(
    word_data$value, 
    type_name(word_data$nchar, "v"), 
    type_name(word_data$nchar, "c")
  )
  
  parts <- word_data$part
  
  i <- which(parts %in% c("ch", "ck", "sch"))
  word_data$type[i] <- toupper(parts[i])
  
  pattern <- "^(.*)(sch)(.*)$"
  i <- which(grepl(pattern, parts) & word_data$type != "SCH")
  word_data$type[i] <- to_consonant_sequence_type(parts[i], pattern)
  
  pattern <- "^(.*)(ch|ck)(.*)$"
  i <- which(
    grepl(pattern, parts) & 
      !word_data$type %in% c("CH", "CK") & !grepl("SCH", word_data$type)
  )
  word_data$type[i] <- to_consonant_sequence_type(parts[i], pattern)
  
  i <- which(parts == "ß")
  word_data$type[i] <- "SZ"
  
  i <- which(is_diphthong(parts))
  word_data$type[i] <- "DT"
  
  word_data %>%
    kwb.utils::removeColumns("value") %>%
    kwb.utils::moveColumnsToFront("word")
}

# is_vowel ---------------------------------------------------------------------
is_vowel <- function(chars)
{
  grepl("[aeiouäöüy]", chars)
}

# to_consonant_sequence_type ---------------------------------------------------
#' @importFrom kwb.utils extractSubstring
to_consonant_sequence_type <- function(x, pattern)
{
  if (length(x) == 0L) {
    return(character())
  }
  
  parts <- kwb.utils::extractSubstring(pattern, x, 1:3)
  
  format_nchar <- function(x, fmt) ifelse(x == "", "", sprintf(fmt, nchar(x)))

  paste0(
    format_nchar(parts[[1L]], "%dc-"),
    toupper(parts[[2L]]),
    format_nchar(parts[[3L]], "-%dc")
  )
}

# is_diphthong -----------------------------------------------------------------
# Die bekanntesten Schreibungen von Diphthongen im Deutschen sind ei, au, äu und 
# eu; selten sind ai, oi und ui. 
is_diphthong <- function(x)
{
  x %in% c("ei", "au", "äu", "eu", "ai", "oi", "ui")
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
  # `1c-1v-2c-1v-3c` = 3L, # "gestärkt"
  # `1c-1v-3c-1v-1c` = 4L, # wi-scher (look for sch)
  # `1c-1v-4c-1v-1c` = 4L, # "menschen" (sch)
  # `2c-1v-2c-1v-1c` = 4L, # ch, ck: kra-chen ste-cken
  # `2c-1v-3c-1v-2c` = 5L, # "früh-stück"
  
  yaml::read_yaml(kwb.utils::safePath(file))
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

# write_lines_utf8 -------------------------------------------------------------
write_lines_utf8 <- function(x, path)
{
  con <- file(path, "wt", encoding = "UTF-8")
  on.exit(close(con))
  
  writeLines(x, con)
}

# remove_hyphens ---------------------------------------------------------------
remove_hyphens <- function(x)
{
  gsub("-", "", x)
}

# call_hyphenation_service -----------------------------------------------------
#' @importFrom kwb.utils catAndRun isTryError
#' @importFrom rvest html_element html_text read_html
#' @importFrom utils URLencode
call_hyphenation_service <- function(word)
{
  url <- "https://www.silbentrennung24.de/wort/"
  
  html <- kwb.utils::catAndRun(
    paste("Looking up hyphenation for", word),
    try(rvest::read_html(paste0(url, utils::URLencode(word))))
  )
  
  if (kwb.utils::isTryError(html)) {
    return(NULL)
  }
  
  html %>%
    rvest::html_element(xpath = "//div[@id = 'termresult']") %>%
    rvest::html_text()  
}

# get_syllable_replacements ----------------------------------------------------
get_syllable_replacements <- function()
{
  c(
    "^all[aeiou]"
    #stats::setNames(as.list(paste0("-", syllables, "-")), syllables),
    #replacements_double_consonants(),
    #"^(be)([^ist])" = "\\1-\\2"
    , "-+" = "-"
    , "^-" = ""
    , "-$" = ""
  )
}

# replacements_double_consonants -----------------------------------------------
#' @importFrom stats setNames
replacements_double_consonants <- function()
{
  consonants <- strsplit("bdfglmnprt", "")[[1L]]
  replacements <- as.list(paste0(consonants, "-", consonants, "\\1"))
  stats::setNames(replacements, paste0(consonants, consonants, "(.{1,})$"))
}

# is_true_for_part_at ----------------------------------------------------------
is_true_for_part_at <- function(x, i, fun, ...) {
  sapply(x, function(y) nrow(y) >= i && isTRUE(fun(y$part[i], ...)))
}

has_ch_at <- function(x, i) is_true_for_part_at(x, i, `==`, "ch")
has_ck_at <- function(x, i) is_true_for_part_at(x, i, `==`, "ck")
has_sz_at <- function(x, i) is_true_for_part_at(x, i, `==`, "ß")
has_ch_or_ck_at <- function(x, i) is_true_for_part_at(x, i, `%in%`, c("ch", "ck"))
has_diphthong_at <- function(x, i) is_true_for_part_at(x, i, is_diphthong)


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

# add_hyphens ------------------------------------------------------------------
add_hyphens <- function(x, hyphen = "-")
{
  except_last <- -length(x)
  except_first <- -1L
  
  x[except_last] <- paste0(x[except_last], hyphen)
  x[except_first] <- paste0(hyphen, x[except_first])
  
  x
}

# to_upper_case ----------------------------------------------------------------
to_upper_case <- function(x)
{
  if (length(x) == 0L) {
    return(character())
  }
  
  x %>%
    strsplit("") %>%
    lapply(function(y) `[<-`(y, 1L, toupper(y[1L]))) %>%
    sapply(paste0, collapse = "")
}

# is_upper_case ----------------------------------------------------------------
is_upper_case <- function(x)
{
  chars <- strsplit(x, "")
  sapply(chars, function(y) y[1L] == toupper(y[[1L]]) && y[[1L]] != "ß")
}

# is_prefix --------------------------------------------------------------------
is_prefix <- function(x) !startsWith(x, "-") & endsWith(x, "-")

# is_suffix --------------------------------------------------------------------
is_suffix <- function(x) startsWith(x, "-") & !endsWith(x, "-")

# is_infix ---------------------------------------------------------------------
is_infix <- function(x) startsWith(x, "-") & endsWith(x, "-")

# is_word ----------------------------------------------------------------------
is_word <- function(x) !(is_prefix(x) | is_suffix(x) | is_infix(x))

# determine_type_pattern -------------------------------------------------------
determine_type_pattern <- function(word)
{
  paste(split_words(word)$type, collapse = "-")
}

# plot_wordcloud ---------------------------------------------------------------
plot_wordcloud <- function(x, cex = 2)
{
  #cex <- 2
  
  x <- sort(x, decreasing = TRUE)
  
  # wordcloud::wordcloud(
  #   names(x), unname(x), scale = c(1, 10), min.freq = 1L, random.order = FALSE,
  #   rot.per = 0)
  init_empty_plot(xlim = c(0, 1), ylim = c(0, 1))
  add_sized_words_vertically(words = names(x), freqs = unname(x))
}

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
