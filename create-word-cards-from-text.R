library(magrittr)

# MAIN: Download texts that are available online -------------------------------
if (FALSE)
{
  text <- read_story_kater_leo_arzt()
  
  write_lines_utf8(text, "texts/kater-leo-arzt.txt")
}

# MAIN: Get raw text -----------------------------------------------------------
if (FALSE)
{
  name <- "die-groesste-getreidepflanze"
  name <- "hahn-und-huhn"
  name <- "kater-leo-arzt"
  
  raw_text <- read_text(name)
}

# MAIN: Create word cards ------------------------------------------------------
if (FALSE)
{
  words <- text_to_words(raw_text)

  article_guesses <- guess_nouns_with_articles(words)
  articles <- correct_article_guesses(article_guesses)
  writeLines(articles)
  
  word_table <- words %>%
    words_to_word_table(to_lower = FALSE) %>%
    aggregate_lower_upper_case()
  
  #View(word_table)

  hyphenated <- hyphenate(word_table$word)
  
  syllables_in_words <- lapply(hyphenated, split_hyphenated, hyphen = "-")

  syllable_counts <- unlist(lapply(
    seq_along(syllables_in_words),
    function(i) table(syllables_in_words[[i]]) * word_table$frequency[i]
  ))
  
  syllable_counts
  
  result <- syllable_counts %>%
    split(tolower(remove_hyphens(names(syllable_counts)))) %>%
    lapply(function(y) {
      #y <- x[[5]]
      z <- aggregate(y, by = list(names(y)), sum)
      sort(stats::setNames(z[[2]], z[[1]]), decreasing = TRUE)
    })
  
  # z <- result$an
  # writeLines(names(z))
  # sprintf("%d (%s)", sum(z), paste(z, collapse = "+"))
  # 
  # %>% 
  #   lapply(function(x) {
  #     table
  #   })
  
  #%>%
  #  sort(decreasing = TRUE)
  
  word_table <- data.frame(
    nchar = nchar(names(syllable_counts)),
    word = names(syllable_counts),
    frequency = unname(syllable_counts)
  )
  
  kwb.utils::createDirectory("output")
  
  #name <- kwb.utils::removeExtension(basename(file))
  name <- paste0(name, "_syllables")
  
  plot_word_cards(
    word_table$word, 
    frequencies = word_table$frequency,
    file = sprintf("output/cards_%s.pdf", name), 
    both_cases = FALSE, 
    cex = 2.2,
    label_types = c(1L, 2L, 2L)
  )
  
}

# MAIN: Find syllables ---------------------------------------------------------
if (FALSE)
{
  words <- text_to_words(raw_text)
  
  # All different words occurring in the text
  unique_words <- sort(unique(words))

  # Split the words at syllables (keeping upper/lower case)
  hyphenated <- hyphenate(unique_words)

  View(data.frame(unique_words, hyphenated))
}

# MAIN: Other approaches -------------------------------------------------------
if (FALSE)
{
  #hyphenation <- lapply(words, call_hyphenation_service)
  #words_raw <- kwb.utils::multiSubstitute(words, get_syllable_replacements())
  #writeLines(grep("-", words_raw, value = TRUE))
}

# read_story_kater_leo_arzt ----------------------------------------------------
read_story_kater_leo_arzt <- function()
{
  html <- rvest::read_html("https://www.zitronenbande.de/kater-leo-arzt/")
  
  text_lines <- strsplit(rvest::html_text(html), "\n")[[1L]]
  
  story_line <- grep("aktualisiert", text_lines, value = TRUE)
  
  pattern <- "aktualisiert: \\d{2}\\.\\d{2}\\.\\d{4}(.*)$"
  
  kwb.utils::extractSubstring(pattern, story_line, 1L)
}

# read_text --------------------------------------------------------------------
read_text <- function(name)
{
  list_available_text_files() %>%
    kwb.utils::selectElements(name) %>%
    kwb.utils::readLinesWithEncoding(fileEncoding = "UTF-8")
}

# list_available_text_files ----------------------------------------------------
list_available_text_files <- function()
{
  files <- as.list(dir("texts", "\\.txt$", full.names = TRUE))
  stats::setNames(files, kwb.utils::removeExtension(basename(unlist(files))))
}

# text_to_words ----------------------------------------------------------------
text_to_words <- function(raw_text)
{
  strsplit(paste(clean_text(raw_text), collapse = " "), " +")[[1L]]
}

# clean_text -------------------------------------------------------------------
clean_text <- function(raw_text)
{
  kwb.utils::multiSubstitute(raw_text, list(
    '[“„.,?!:-]' = " ",
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
words_to_word_table <- function(words, to_lower = TRUE)
{
  if (isTRUE(to_lower)) {
    words <- tolower(words)
  }
  
  word_groups <- split(words, nchar(words))
  
  lapply(
    word_groups, 
    function(x) stats::setNames(
      as.data.frame(table(x), stringsAsFactors = FALSE),
      c("word", "frequency")
    )
  ) %>%
    dplyr::bind_rows(.id = "nchar") %>%
    order_by_frequency_and_word()
}

# order_by_frequency_and_word --------------------------------------------------
order_by_frequency_and_word <- function(data)
{
  kwb.utils::orderBy(
    data, 
    c("frequency", "word"), 
    decreasing = c(TRUE, FALSE),
    method = "radix"
  )
}

# aggregate_lower_upper_case ---------------------------------------------------
aggregate_lower_upper_case <- function(word_table)
{
  result <- word_table %>%
    dplyr::group_by(nchar, tolower(word)) %>%
    dplyr::summarise(
      n_lower = sum(frequency[!is_upper_case(word)]),
      n_upper = sum(frequency[is_upper_case(word)]),
      frequency = n_lower + n_upper,
      .groups = "drop"
    ) %>%
    dplyr::rename(word = "tolower(word)")
  
  no_lower <- result$n_lower == 0L
  result$word[no_lower] <- to_upper_case(result$word[no_lower])
  
  result %>%
    as.data.frame() %>%
    order_by_frequency_and_word() %>%
    kwb.utils::moveColumnsToFront(c("nchar", "word", "frequency"))
}

# plot_word_cards --------------------------------------------------------------
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
    
    par(mfrow = mfrow, mar = c(0, 0, 0, 0))
    
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
plot_card <- function(
    texts, footer = c(NA, NA, NA), ylim = c(-1, 1), squeeze = 0.3, cex = 1, 
    cex.footer = 1
)
{
  text_footer <- function(text, x, y = -0.9) {
    if (!is.na(text)) {
      text(x, y, text, cex = cex.footer)
    }
  }
  
  init_empty_plot()
  
  y <- positions_between(length(texts), squeeze * ylim)
  text(0, rev(y), texts, cex = cex)
  
  text_footer(footer[1L], x = -0.9)
  text_footer(footer[2L], x =  0.0)
  text_footer(footer[3L], x = +0.9)
}

# init_empty_plot --------------------------------------------------------------
init_empty_plot <- function(xlim = c(-1, 1), ylim = c(-1, 1)
)
{
  plot(
    0, 0, pch = NA, xlab = "", ylab = "", xaxt = "n", yaxt = "n",
    xlim = xlim, ylim = ylim
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
  lower_case_x <- tolower(x)
  
  # For performance reasons, split only unique lower case words
  hyphenated <- split_into_syllables(unique(lower_case_x))

  # Initialise result vector with lower case hyphenated versions of words in x
  indices <- match(lower_case_x, remove_hyphens(hyphenated))
  stopifnot(all(!is.na(indices)))
  result <- hyphenated[indices]
  
  # Restore original case
  is_upper <- is_upper_case(x)
  result[is_upper] <- to_upper_case(result[is_upper])
  
  # Return result vector
  result
}

# split_into_syllables ---------------------------------------------------------
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
  
  if (FALSE) {
    
    # Prepare split position assignments for non treated patterns  
    prepare_split_assignments_for_non_treated(x = syllables_by_pattern)
    
    # Modify "patterns_tmp.txt" by adding "-" within the words and save as
    # "patterns.txt"
    create_split_assignments_from_pattern_file()
  }
  
  sorted_syllables <- sort(unname(unlist(syllables_by_pattern)))
  
  stopifnot(0L == length(
    setdiff(full_words, c(short_words, remove_hyphens(sorted_syllables)))
  ))
  
  write_lines_utf8(
    sort(c(short_words, sorted_syllables)), 
    path = "output/syllables_tmp.txt"
  )
  
  result <- full_words
  
  i <- match(full_words, remove_hyphens(sorted_syllables))
  
  result[!is.na(i)] <- sorted_syllables[i[!is.na(i)]]
  
  result
}

# split_words -------------------------------------------------------------------
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
    kwb.utils::renameColumns(list(starts_at = "from", "ends_at" = "to"))
  
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
to_consonant_sequence_type <- function(x, pattern)
{
  before_within_after <- kwb.utils::extractSubstring(pattern, x, 1:3)
  
  before <- before_within_after[[1L]]
  within <- before_within_after[[2L]]
  after <- before_within_after[[3L]]
  
  paste0(
    ifelse(before == "", "", sprintf("%dc-", nchar(before))),
    toupper(within),
    ifelse(after == "", "", sprintf("-%dc", nchar(after)))
  )
}

# is_diphthong -----------------------------------------------------------------
# Die bekanntesten Schreibungen von Diphthongen im Deutschen sind ei, au, äu und 
# eu; selten sind ai, oi und ui. 
is_diphthong <- function(x)
{
  x %in% c("ei", "au", "äu", "eu", "ai", "oi", "ui.")
}

# find_syllables ---------------------------------------------------------------
find_syllables <- function(sets)
{
  # Initialise result list
  syllables_by_pattern <- list()
  
  # Create type string representing sequence of consecutive vocals/consonants
  type_patterns <- sapply(sets, function(set) paste(set$type, collapse = "-"))
  
  # Get split positions per type pattern
  split_at <- get_split_positions_by_pattern()
  
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

# get_split_positions_by_pattern -----------------------------------------------
get_split_positions_by_pattern <- function()
{
  # To reorder the list after having added new patterns, copy the output of the
  # following command below into the body of this function:
  # cat_ordered_split_positions(get_split_positions_by_pattern())
  
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
  
  list(
    `2c-DT` = 0L,
    `2v-2c` = 0L,
    `DT-CH` = 0L,
    `1c-1v-2c` = 0L,
    `1c-1v-3c` = 0L,
    `1c-1v-4c` = 0L,
    `1c-1v-CH` = 0L,
    `1c-2v-1c` = 0L,
    `1c-2v-SZ` = 0L,
    `1c-DT-1c` = 0L,
    `2c-1v-1c` = 0L,
    `2c-1v-3c` = 0L,
    `2c-1v-CK` = 0L,
    `2c-1v-SZ` = 0L,
    `2c-2v-1c` = 0L,
    `2c-DT-1c` = 0L,
    `3c-1v-2c` = 0L,
    `4c-1v-1c` = 0L,
    `4c-1v-2c` = 0L,
    `4c-1v-3c` = 0L,
    `4c-1v-CK` = 0L,
    `1c-1v-SCH` = 0L,
    `SCH-1v-1c` = 0L,
    `1c-1v-CH-1c` = 0L,
    `1c-1v-CH-2c` = 0L,
    `1v-1c-1v-1c` = 0L,
    `1v-1c-1v-2c` = 0L,
    `1c-1v-SCH-1c` = 0L,
    `SCH-1c-1v-CK-1c` = 0L,
    `3v-2c` = 2L,
    `1v-2c-1v` = 2L,
    `2v-1c-1v` = 2L,
    `DT-1c-1v` = 2L,
    `1c-1v-1c-1v` = 2L,
    `1c-1v-2c-1v` = 3L,
    `1c-1v-CH-1v` = 2L,
    `1v-2c-1v-1c` = 2L,
    `1v-2c-1v-2c` = 2L,
    `2v-1c-1v-1c` = 2L,
    `DT-1c-1v-1c` = 2L,
    `1c-1v-1c-1v-1c` = 2L,
    `1c-1v-1c-1v-2c` = 2L,
    `1c-1v-1c-DT-1c` = 2L,
    `1c-1v-3c-1v-2c` = 3L,
    `1c-1v-CH-1v-1c` = 2L,
    `1c-1v-CK-1v-1c` = 2L,
    `1v-2c-1v-3c-1v` = c(2L, 6L),
    `1c-1v-1c-1v-1c-1v` = c(2L, 4L),
    `1v-2c-1v-1c-1v-1c` = c(2L, 4L),
    `1v-2c-1v-1c-1v-2c` = c(2L, 4L),
    `1v-2c-1v-1c-DT-1c` = c(2L, 4L),
    `1v-4c-1v-2c-1v-1c` = c(2L, 7L),
    `1v-4c-1v-3c-1v-CH` = c(2L, 8L),
    `1c-1v-1c-1v-2c-1v-1c` = c(2L, 5L),
    `1c-1v-1c-1v-3c-1v-1c` = c(2L, 6L),
    `1c-1v-4c-1v-3c-1v-1c` = c(2L, 9L),
    `1c-1v-CK-1v-2c-DT-1c` = c(2L, 6L),
    `1c-1v-1c-1v-2c-1v-1c-1v` = c(2L, 5L, 7L),
    `1c-1v-1c-1v-2c-1v-1c-1v-1c` = c(2L, 5L, 7L),
    `1c-1v-2c-1v-3c-1v-1c-1v-1c` = c(2L, 7L, 9L),
    `1c-1v-2c-1v-CH-1c-1v-1c-1v-1c` = c(2L, 7L, 9L),
    `1c-1v-2c-DT-1c-1v-3c-1v-2c-1v` = c(2L, 6L, 8L, 13L),
    `1c-3v-2c` = 3L,
    `1v-3c-1v` = 3L,
    `1c-2v-1c-1v` = 3L,
    `1c-3v-4c-2v` = c(3L, 6L),
    `1c-3v-4c-DT` = c(3L, 6L),
    `1c-DT-1c-1v` = 3L,
    `1v-3c-1v-2c` = 3L,
    `1v-3c-1v-CH` = 3L,
    `2c-1v-1c-1v` = 3L,
    `2c-1v-SZ-1v` = 3L,
    `2v-2c-1v-1c` = 3L,
    `DT-2c-1v-1c` = 3L,
    `1c-1v-1c-2v-1c` = 3L,
    `1c-1v-2c-1v-1c` = 3L,
    `1c-1v-2c-1v-2c` = 3L,
    `1c-1v-2c-1v-3c` = 3L,
    `1c-1v-2c-1v-CH` = 3L,
    `1c-3v-3c-1v-1c` = c(3L, 6L),
    `1c-DT-1c-1v-1c` = 3L,
    `1c-DT-1c-1v-2c` = 3L,
    `1c-DT-2c-2v-1c` = 3L,
    `2c-1v-1c-1v-1c` = 3L,
    `2c-1v-SZ-1v-1c` = 3L,
    `1c-2v-SCH-1v-1c` = 3L,
    `1c-1v-2c-1v-2c-1v` = c(3L, 6L),
    `2v-2c-1v-1c-1v-2c` = c(3L, 5L),
    `DT-2c-1v-1c-1v-2c` = c(3L, 5L),
    `1c-1v-1c-SCH-1v-1c` = 4L,
    `1c-1v-2c-1v-1c-1v-1c` = c(3L, 5L),
    `1c-1v-2c-1v-2c-1v-1c` = c(3L, 6L),
    `1c-1v-2c-1v-2c-1v-2c` = c(3L, 6L),
    `1c-1v-2c-1v-2c-1v-3c` = c(3L, 5L),
    `1c-1v-2c-1v-2c-1v-CH` = c(3L, 6L),
    `1c-1v-3c-1v-1c-1v-1c` = c(3L, 6L),
    `1c-1v-5c-1v-2c-1v-1c` = c(3L, 9L),
    `1c-1v-4c-2v-1c-1v-1c-1v` = c(3L, 8L, 10L),
    `2v-2c-1v-3c-1v-2c-1v-1c` = c(3L, 6L, 10L),
    `DT-2c-1v-3c-1v-2c-1v-1c` = c(3L, 6L, 10L),
    `1c-1v-2c-1v-2c-1v-2c-1v-1c` = c(3L, 6L, 9L),
    `1c-1v-2c-1v-4c-1v-2c-1v-1c` = c(3L, 6L, 11L),
    `1c-2v-1c-1v-1c-1v-2c-1v-1c` = c(3L, 5L, 7L),
    `1c-2v-2c-1v-2c-1v-1c-1v-1c` = c(3L, 7L, 9L),
    `1c-DT-1c-1v-1c-1v-CH-1v-1c` = c(3L, 5L, 7L),
    `1c-1v-1c-SCH-2v-1c-1v-1c-1v` = c(3L, 8L, 10L),
    `1c-1v-3c-1v` = 4L,
    `1c-2v-2c-1v` = 4L,
    `1c-DT-2c-1v` = 4L,
    `2c-1v-2c-1v` = 4L,
    `1c-1v-3c-1v-1c` = 4L,
    `1c-1v-3c-1v-CH` = 4L,
    `1c-1v-3c-DT-1c` = 4L,
    `1c-1v-4c-1v-1c` = 4L,
    `1c-2v-2c-1v-1c` = 4L,
    `1c-2v-2c-2v-3c` = 4L,
    `1c-2v-2c-DT-3c` = 4L,
    `1c-DT-2c-1v-1c` = 4L,
    `1c-DT-2c-1v-2c` = 4L,
    `2c-1v-2c-1v-1c` = 4L,
    `2c-1v-2c-1v-2c` = 4L,
    `2c-1v-2c-1v-CH` = 4L,
    `2c-1v-3c-1v-1c` = 4L,
    `2c-2v-1c-1v-1c` = 4L,
    `2c-DT-1c-1v-1c` = 4L,
    `3c-1v-1c-1v-1c` = 4L,
    `SCH-1v-1c-1v-1c` = 4L,
    `1c-1v-3c-1v-2c-1v` = c(4L, 7L),
    `1c-1v-CH-1c-1v-1c` = 4L,
    `1c-2v-4c-1v-2c-1v` = c(4L, 9L),
    `1c-DT-4c-1v-2c-1v` = c(4L, 9L),
    `2c-1v-1c-CH-1v-1c` = 4L,
    `1c-1v-3c-1v-CH-1v-1c` = c(4L, 6L),
    `1c-1v-4c-1v-1c-1v-1c` = 4L,
    `1c-DT-2c-1v-2c-1v-1c` = c(4L, 7L),
    `1c-DT-2c-1v-CH-1v-1c` = c(4L, 6L),
    `1c-DT-3c-1v-CK-1v-1c` = c(4L, 7L),
    `1c-DT-4c-1v-2c-1v-1c` = c(4L, 9L),
    `1v-1c-1v-2c-1v-2c-1v` = c(4L, 7L),
    `2c-1v-3c-1v` = 5L,
    `3c-1v-2c-1v` = 5L,
    `2c-1v-3c-1v-2c` = 5L,
    `2c-2v-2c-1v-1c` = 5L,
    `2c-DT-2c-1v-1c` = 5L,
    `3c-2v-1c-1v-1c` = 5L,
    `4c-1v-1c-1v-1c` = 5L,
    `SCH-DT-1c-1v-1c` = 5L,
    `1c-1v-4c-1v-1c-1v` = c(5L, 7L),
    `1c-DT-3c-1v-3c-1v-CH` = c(5L, 9L),
    `2c-1v-3c-1v-1c-1v-1c` = c(5L, 7L),
    `1c-DT-3c-1v-CH-1c-1v-CH` = c(5L, 9L),
    `1v-CK-1v-2c-1v-1c-1v-1c` = c(5L, 7L),
    `3c-1v-3c-1v` = 6L,
    `SCH-1v-3c-1v` = 6L,
    `4c-2v-1c-1v-1c` = 6L,
    `4c-1v-2c-1v-2c-1v` = c(6L, 9L),
    `4c-1v-3c-1v-1c` = 7L,
    "2c-1v-2c" = 0L,
    "SCH-1c-1v-1c" = 0L,
    "SCH-1c-1v-2c" = 0L,
    "SCH-1c-1v-CK" = 0L,
    "1c-1v-CH-1c-1v" = 4L,
    "1c-2v-1c-1v-1c" = 3L,
    "1c-2v-1c-1v-2c" = 3L,
    "1c-2v-2c-DT-CH-1c" = 4L,
    "SCH-1c-1v-1c-1v-1c" = 5L,
    "SCH-1c-2v-1c-1v-1c" = 6L,
    "SCH-1c-1v-2c-1v-2c-1v" = c(6L, 9L),
    "SCH-1c-1v-CH-1c-1v-1c" = 7L,
    "1c-1v-1c-CH-1c-1v-1c-1v" = c(5L, 7L),
    "1c-1v-1c-SCH-1c-1v-2c-1v-1c" = c(3L, 9L),
    "1c-1v-SCH-1c-1v-CH-1c-1v-1c" = c(2L, 9L),
    "1c-1v-2c-1v-1c-SCH-1v-2c-1v-1c" = c(3L, 6L, 11L)
  )
}

# cat_ordered_split_positions ------------------------------------------------
cat_ordered_split_positions <- function(split_at)
{
  ordered_indices <- order(
    sapply(split_at, "[", 1L), 
    nchar(names(split_at)), 
    names(split_at)
  )
  
  split_at[ordered_indices] %>%
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
  
  stopifnot(all(i < n_char))
  
  from <- c(1L, i + 1L)
  to <- c(i, NA)

  paste_args <- lapply(seq_along(from), function(j) {
    substr(x, from[j], if (is.na(to[j])) n_char else to[j])
  })
  
  do.call(paste, c(paste_args, sep = "-"))
}

# prepare_split_assignments_for_non_treated ------------------------------------
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
      sprintf('"%s" = 0L, # %s', p, paste(matches[[p]], collapse = ","))
    }
  )
  
  file <- "patterns_tmp.txt"
  
  kwb.utils::catAndRun(
    paste0("Writing split position assignments to '", file, "'"),
    write_lines_utf8(split_assignments, path = file)
  )
  
  file.edit(file)
}

# create_split_assignments_from_pattern_file -----------------------------------
create_split_assignments_from_pattern_file <- function(file = "patterns.txt")
{
  txt <- kwb.utils::readLinesWithEncoding(file, fileEncoding = "UTF-8")
  
  pattern <- "^[^#]+#\\s*"
  words_by_pattern <- strsplit(gsub(pattern, "", txt), "\\s*,\\s*")
  
  pattern <- "(^[^=]+)\\s+= 0L, #(.*)$"
  index <- c(pattern = 1L, words = 2L)
  assignment_data <- kwb.utils::extractSubstring(pattern, txt, index)
  
  syllable_lengths <- lapply(words_by_pattern, function(x) {
    positions <- lapply(strsplit(x, "-"), nchar)
    stopifnot(suppressMessages(kwb.utils::allAreIdentical(positions)))
    positions[[1L]]
  })
  
  writeLines(sprintf(
    "%s = %s,", 
    assignment_data$pattern,
    sapply(syllable_lengths, function(x) {
      #x <- split_positions[[1L]]
      if (length(x) == 1L) {
        "0L"
      } else if (length(x) == 2L) {
        paste0(x[1L], "L")
      } else {
        sprintf("c(%s)", paste0(cumsum(x)[-length(x)], "L", collapse = ", "))
      }
    })
  ))
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
call_hyphenation_service <- function(word)
{
  url <- "https://www.silbentrennung24.de/wort/"
  
  html <- kwb.utils::catAndRun(
    paste("Looking up hyphenation for", word),
    try(rvest::read_html(paste0(url, URLencode(word))))
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
  x %>%
    strsplit("") %>%
    lapply(function(y) `[<-`(y, 1L, toupper(y[1L]))) %>%
    sapply(paste0, collapse = "")
}

# is_upper_case ----------------------------------------------------------------
is_upper_case <- function(x)
{
  chars <- strsplit(x, "")
  sapply(chars, function(y) y[1L] == toupper(y[[1L]]))
}
