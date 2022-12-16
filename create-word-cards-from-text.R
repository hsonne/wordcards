library(magrittr)

# MAIN -------------------------------------------------------------------------
if (FALSE)
{
  files <- dir("texts", "\\.txt$", full.names = TRUE)
  
  file <- files[3L]

  raw_text <- read_text(file)

  words <- text_to_words(raw_text)

  article_guesses <- guess_nouns_with_articles(words)
  articles <- correct_article_guesses(article_guesses)
  writeLines(articles)
  
  #word_table_lower <- words_to_word_table(words)
  
  word_table_original <- words_to_word_table(words, to_lower = FALSE)
  word_table <- aggregate_lower_upper_case(word_table_original)

  #View(word_table)
  #View(word_table_original)
  
  kwb.utils::createDirectory("output")

  name <- kwb.utils::removeExtension(basename(file))

  plot_word_cards(
    word_table$word, 
    frequencies = word_table$frequency,
    file = sprintf("output/cards_%s.pdf", name), 
    both_cases = FALSE, 
    cex = 2.2,
    label_types = c(1L, 2L, 2L)
  )

}

# Download texts ---------------------------------------------------------------
if (FALSE)
{
  text <- read_story_kater_leo_arzt()
  
  con <- file("texts/kater-leo-arzt.txt", "wt", encoding = "UTF-8")
  
  writeLines(text, con)
  
  close(con)
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
read_text <- function(file)
{
  kwb.utils::readLinesWithEncoding(file, fileEncoding = "UTF-8")
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
  lower_words <- tolower(word_table$word)
  
  i <- which(duplicated(lower_words))  
  
  if (length(i) == 0L) {
    return(word_table)
  }
  
  j <- which(lower_words %in% lower_words[i])
  
  data <- word_table[j, ]
  data$word <- tolower(data$word)
  
  lower_upper_stats <- aggregate(frequency ~ nchar + word, data = data, sum)
  
  word_table <- rbind(word_table[-j, ], lower_upper_stats)
  
  order_by_frequency_and_word(word_table)
}

# plot_word_cards --------------------------------------------------------------
plot_word_cards <- function(
    words,
    file = NULL,
    per_page = 32L, 
    cex = 2, 
    y = 0.3, 
    plot_rank = TRUE,
    plot_nchar = TRUE,
    frequencies = NULL,
    both_cases = TRUE,
    label_types = c(1L, 1L, 1L)
)
{
  text_if <- function(condition, x, text, y = -0.9) {
    if (isTRUE(condition)) {
      text(x, y, text)
    }
  }
  
  kwb.utils::toPdf(pdfFile = file, landscape = FALSE, expressions = {

    mfrow <- kwb.plot::bestRowColumnSetting(per_page, target.ratio = 0.71)
    
    par(mfrow = mfrow, mar = c(0, 0, 0, 0))
    
    for (i in seq_along(words)) {
      
      word <- words[i]
      
      # Init empty plot
      plot(0, 0, pch = NA, xlab = "", ylab = "", xaxt = "n", yaxt = "n",
           xlim = c(-1, 1), ylim = c(-1, 1))

      if (isTRUE(both_cases)) {
        
        # Write word in lower case      
        text(0, y, word, cex = cex)
        
        # Write word in upper case
        text(0, -y, tools::toTitleCase(word), cex = cex)
        
      } else {
        
        # Write word in original case
        text(0, 0, word, cex = cex)
      }
      
      text_if(
        isTRUE(plot_rank), 
        x = -0.9, 
        text = label_rank(i, label_types[1L])
      )
      
      text_if(
        condition = !is.null(frequencies), 
        x = 0, 
        text = label_times(frequencies[i], label_types[2L])
      )
      
      text_if(
        condition = isTRUE(plot_nchar), 
        x = 0.9, 
        text = label_nchar(nchar(word), label_types[3L])
      )
    }
  })
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

# guess_nouns_with_articles ----------------------------------------------------
guess_nouns_with_articles <- function(words)
{
  i <- which(is_article(words))
  
  is_noun <- i < length(words) & words[i + 1L] != tolower(words[i + 1L])
  
  j <- i[is_noun]
  
  articles <- words[j]
  
  nouns <- words[j + 1L]
  
  articles_per_noun <- split(tolower(articles), tools::toTitleCase(nouns))
  
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
