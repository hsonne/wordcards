#
# Source the whole script first to load the function definitions below
#

# MAIN: Size words according to their frequency and arrange them vertically ----
if (FALSE)
{
  graphics::par(mar = rep(3, 4))
  wordcards:::init_empty_plot(xlim = c(0, 1), ylim = c(0, 1))

  wordcards:::add_sized_words_vertically(
    words = c("hallo", "Testwort", "Wort"),
    freqs = c(13L, 1L, 2L), 
    vertical_space_share = 0.3, 
    spacing_method = "proportional" # "equal"
  )
}

# MAIN: Get raw text -----------------------------------------------------------
if (FALSE)
{
  name <- "das-hochnaesige-einhorn"
  name <- "die-groesste-getreidepflanze"
  name <- "hahn-und-huhn"
  name <- "kater-leo-arzt"
  
  raw_text <- wordcards:::read_text(name)
}

# MAIN: Guess articles ---------------------------------------------------------
if (FALSE)
{
  words <- wordcards:::text_to_words(raw_text)
  
  article_guesses <- wordcards:::guess_nouns_with_articles(words)
  articles <- wordcards:::correct_article_guesses(article_guesses)
  writeLines(articles)
}

# MAIN: Create word cards ------------------------------------------------------
if (FALSE)
{
  words <- wordcards:::text_to_words(raw_text)
  
  word_table <- words %>%
    wordcards:::words_to_word_table() %>%
    kwb.utils::renameAndSelect(list("word", frequency = "n")) %>%
    kwb.utils::selectColumns(c("word", "n")) %>%
    wordcards:::aggregate_by_case(column_word = "word") %>%
    wordcards:::set_word_to_probable_case() %>%
    wordcards:::order_by_frequency_and_word(column_frequency = "n_total")
  
  #View(word_table)
  
  word_table$hyphenated <- wordcards:::hyphenate(word_table$word)
  
  syllable_data <- wordcards:::syllables_to_syllable_table(
    hyphenated = word_table$hyphenated, 
    frequencies = word_table$n_total
  )
  
  #View(syllable_data)
  
  stopifnot(!anyDuplicated(syllable_data$syllable))
  
  formatted_stats <- wordcards:::aggregate_syllable_data(syllable_data)
  
  lapply(formatted_stats, function(x) 0.01 * kwb.utils::percentageOfSum(x))
  
  kwb.utils::toPdf(
    pdfFile = "./inst/extdata/output/syllables_hahn-und-huhn.pdf", 
    expressions = {
      graphics::par(mfrow = c(3L, 4L), mar = c(0.2, 0.2, 0.2, 0.2))
      lapply(formatted_stats, wordcards:::plot_wordcloud)
    }
  )
  
  card_info <- wordcards:::get_card_info(
    formatted_stats[order(lengths(formatted_stats))], 
    hyphenated_words = word_table$hyphenated
  )
  
  kwb.utils::toPdf(landscape = FALSE, {
    par(mar = c(0.1, 0.2, 0.1, 0.2), mfrow = c(8L, 2L))
    lapply(card_info, wordcards:::plot_card_from_card_info)
  })
  
  word_table <- data.frame(
    nchar = nchar(syllable_data$syllable),
    word = syllable_data$syllable,
    frequency = syllable_data$total
  )
  
  kwb.utils::createDirectory("inst/extdata/output")
  
  #name <- kwb.utils::removeExtension(basename(file))
  name <- paste0(name, "_syllables")
  
  wordcards:::plot_word_cards(
    word_table$word, 
    frequencies = word_table$frequency,
    file = sprintf("inst/extdata/output/cards_%s.pdf", name), 
    both_cases = FALSE, 
    cex = 2.2,
    label_types = c(1L, 2L, 2L),
    to_pdf = FALSE
  )
  
}

# MAIN: Find syllables ---------------------------------------------------------
if (FALSE)
{
  words <- wordcards:::text_to_words(raw_text)
  
  # All different words occurring in the text
  unique_words <- sort(unique(words))
  
  # Split the words at syllables (keeping upper/lower case)
  hyphenated <- wordcards:::hyphenate(x = unique_words)
  
  View(data.frame(unique_words, hyphenated))
  
  # Check pattern for a certain word
  patterns <- sapply(unique_words, wordcards:::determine_type_pattern)
  
  (pattern <- patterns[names(patterns) == "bewundern"])
  which(patterns == pattern)
}

# MAIN: Reorder split-positions.yml --------------------------------------------
if (FALSE)
{
  wordcards:::read_split_positions() %>%
    wordcards:::order_split_positions() %>%
    wordcards:::write_split_positions()
}

# MAIN: Reorganise split positions ---------------------------------------------
if (FALSE)
{
  split_positions <- wordcards:::read_split_positions()
  length(split_positions)
  length(unique(split_positions))
  position_strings <- unname(sapply(split_positions, paste, collapse = ","))
  split(names(split_positions), f = position_strings)
  split_positions[(sort(names(split_positions)))]
  split_positions[startsWith(names(split_positions), "1c-1v-1c-1v")]
  reverted <- wordcards:::revert_split_positions(split_positions)
  reverted
}

# MAIN: Other approaches -------------------------------------------------------
if (FALSE)
{
  #hyphenation <- lapply(words, wordcards:::call_hyphenation_service)
  #words_raw <- kwb.utils::multiSubstitute(words, wordcards:::get_syllable_replacements())
  #writeLines(grep("-", words_raw, value = TRUE))
}

# MAIN: Download texts that are available online -------------------------------
if (FALSE)
{
  text <- read_story_kater_leo_arzt()
  
  wordcards:::write_lines_utf8(text, "inst/extdata/texts/kater-leo-arzt.txt")
}

# MAIN: Save split positions to yaml file --------------------------------------
if (FALSE)
{
  file <- "inst/extdata/split-positions.yml"
  split_positions <- wordcards:::read_split_positions(file)
  yaml::write_yaml(split_positions, file)
}

# MAIN: Show files with non-ASCII characters -----------------------------------
if (FALSE)
{
  result <- lapply(
    stats::setNames(nm = dir("R", full.names = TRUE)), 
    tools::showNonASCIIfile
  )
  
  result[lengths(result) > 0L]
}
