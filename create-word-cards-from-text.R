library(magrittr)

# MAIN -------------------------------------------------------------------------
if (FALSE)
{
  raw_text <- read_story_kater_leo_arzt()
  
  raw_text <- read_text("texts/hahn-und-huhn.txt")
  raw_text <- read_text("texts/die-groesste-getreidepflanze")
  
  words <- text_to_words(raw_text)

  article_guesses <- guess_nouns_with_articles(words)

  articles <- correct_article_guesses(article_guesses, corrections = c(
    "die Diele",
    "die Küche",
    "die Spitze",
    "die Stube"
  ))

  writeLines(articles)
  
  word_table <- words_to_word_table(words)
  word_table <- words_to_word_table(words, to_lower = FALSE)

  View(word_table)
  
  kwb.utils::createDirectory("output")
  
  plot_word_cards(word_table$word, "output/cards_kater-leo.pdf")
  plot_word_cards(word_table$word, "output/cards_hahn-und-huhn.pdf")

  plot_word_cards(
    word_table$word, 
    frequencies = word_table$frequency,
    file = "output/cards_kater-leo_with-freq.pdf"
  )
  
  plot_word_cards(
    word_table$word, 
    frequencies = word_table$frequency,
    file = "output/cards_hahn-und-hun_with-freq.pdf", 
    both_cases = FALSE, 
    cex = 2.2
  )
  
  plot_word_cards(
    word_table$word, 
    frequencies = word_table$frequency,
    file = "output/cards_die-groesste-getreidepflanze.pdf", 
    both_cases = FALSE, 
    cex = 2.2
  )
  
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
    kwb.utils::orderBy(
      c("frequency", "word"), 
      decreasing = c(TRUE, FALSE),
      method = "radix"
    )
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
    both_cases = TRUE
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
      
      text_if(isTRUE(plot_rank), -0.9, sprintf("#%d", i))
      text_if(!is.null(frequencies), 0, sprintf("%d-mal", frequencies[i]))
      text_if(isTRUE(plot_nchar), 0.9, sprintf("%der", nchar(word)))
    }
  })
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
correct_article_guesses <- function(article_guesses, corrections)
{
  get_noun <- function(x) sapply(strsplit(x, " "), "[", 2L)
  
  i <- match(get_noun(article_guesses), get_noun(corrections))
  
  is_match <- !is.na(i)
  
  article_guesses[is_match] <- corrections[i[is_match]]

  article_guesses
}
