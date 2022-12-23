library(magrittr)

#
# Size words according to their frequency and arrange them vertically
# 
if (FALSE)
{
  graphics::par(mar = rep(3, 4))
  plot(NA, type = "n", xlim = c(0, 1), ylim = c(0, 1), xlab = "", ylab = "")
  
  add_sized_words_vertically(
    words = c("hallo", "Testwort", "Wort"),
    freqs = c(13L, 1L, 2L), 
    vertical_space_share = 0.3, 
    spacing_method = "proportional" # "equal"
  )
}

# MAIN: Save split positions to yaml file --------------------------------------
if (FALSE)
{
  split_positions <- read_split_positions()
  yaml::write_yaml(split_positions, "split-positions.yml")
}

# MAIN: Download texts that are available online -------------------------------
if (FALSE)
{
  text <- read_story_kater_leo_arzt()
  
  write_lines_utf8(text, "texts/kater-leo-arzt.txt")
}

# MAIN: Get raw text -----------------------------------------------------------
if (FALSE)
{
  name <- "das-hochnaesige-einhorn"
  name <- "die-groesste-getreidepflanze"
  name <- "hahn-und-huhn"
  name <- "kater-leo-arzt"
  
  raw_text <- read_text(name)
}

# MAIN: Guess articles ---------------------------------------------------------
if (FALSE)
{
  words <- text_to_words(raw_text)
  
  article_guesses <- guess_nouns_with_articles(words)
  articles <- correct_article_guesses(article_guesses)
  writeLines(articles)
}

# MAIN: Create word cards ------------------------------------------------------
if (FALSE)
{
  words <- text_to_words(raw_text)
  
  word_table <- words %>%
    words_to_word_table() %>%
    kwb.utils::renameAndSelect(list("word", frequency = "n")) %>%
    kwb.utils::selectColumns(c("word", "n")) %>%
    aggregate_by_case(column_word = "word") %>%
    set_word_to_probable_case() %>%
    order_by_frequency_and_word(column_frequency = "n_total")
  
  #View(word_table)
  
  word_table$hyphenated <- hyphenate(word_table$word)
  
  syllable_data <- syllables_to_syllable_table(
    hyphenated = word_table$hyphenated, 
    frequencies = word_table$n_total
  )
  
  #View(syllable_data)
  
  stopifnot(!anyDuplicated(syllable_data$syllable))
  
  formatted_stats <- aggregate_syllable_data(syllable_data)
  
  lapply(formatted_stats, function(x) 0.01 * kwb.utils::percentageOfSum(x))
  
  kwb.utils::toPdf(pdfFile = "./output/syllables_hahn-und-huhn.pdf", {
    graphics::par(mfrow = c(3L, 4L), mar = c(0.2, 0.2, 0.2, 0.2))
    lapply(formatted_stats, plot_wordcloud)
  })
  
  formatted_stats[order(lengths(formatted_stats))]
  
  View(syllable_stats)
  stopifnot(!anyDuplicated(syllable_stats$syllable))
  
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
  hyphenated <- hyphenate(x = unique_words)
  
  View(data.frame(unique_words, hyphenated))
  
  # Check pattern for a certain word
  patterns <- sapply(unique_words, determine_type_pattern)
  
  (pattern <- patterns[names(patterns) == "bewundern"])
  which(patterns == pattern)
}

# MAIN: Reorder split-positions.yml --------------------------------------------
if (FALSE)
{
  read_split_positions() %>%
    order_split_positions() %>%
    write_split_positions()
}

# MAIN: Reorganise split positions ---------------------------------------------
if (FALSE)
{
  split_positions <- read_split_positions()
  length(split_positions)
  length(unique(split_positions))
  position_strings <- unname(sapply(split_positions, paste, collapse = ","))
  split(names(split_positions), f = position_strings)
  split_positions[(sort(names(split_positions)))]
  split_positions[startsWith(names(split_positions), "1c-1v-1c-1v")]
  reverted <- revert_split_positions(split_positions)
  reverted
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
