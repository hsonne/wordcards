library(magrittr)

# MAIN -------------------------------------------------------------------------
if (FALSE)
{
  raw_text <- read_story_kater_leo_arzt()
  
  words <- text_to_words(raw_text)

  word_table <- words_to_word_table(words)

  View(word_table)
  
  kwb.utils::createDirectory("output")
  
  plot_word_cards(word_table$word, "output/cards_kater-leo.pdf")
  
  plot_word_cards(
    word_table$word, 
    frequencies = word_table$frequency,
    file = "output/cards_kater-leo_with-freq.pdf"
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

# text_to_words ----------------------------------------------------------------
text_to_words <- function(raw_text)
{
  strsplit(clean_text(raw_text), " +")[[1L]]
}

# clean_text -------------------------------------------------------------------
clean_text <- function(raw_text)
{
  kwb.utils::multiSubstitute(raw_text, list(
    '[“„.,?!:-]' = " ",
    "[^A-Za-zäöüß]" = ".",
    "[.]+" = " "
  ))
}

# words_to_word_table ----------------------------------------------------------
words_to_word_table <- function(words)
{
  word_groups <- split(tolower(words), nchar(words))
  
  lapply(
    word_groups, 
    function(x) stats::setNames(
      as.data.frame(table(x), stringsAsFactors = FALSE),
      c("word", "frequency")
    )
  ) %>%
    dplyr::bind_rows(.id = "nchar") %>%
    kwb.utils::orderBy("frequency", decreasing = TRUE)
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
    frequencies = NULL
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
      plot(0, 0, pch = NA, xlab = "", ylab = "", xaxt = "n", yaxt = "n",
           xlim = c(-1, 1), ylim = c(-1, 1))
      text(0, y, word, cex = cex)
      text(0, -y, tools::toTitleCase(word), cex = cex)
      
      text_if(isTRUE(plot_rank), -0.9, sprintf("#%d", i))
      text_if(!is.null(frequencies), 0, sprintf("%d-mal", frequencies[i]))
      text_if(isTRUE(plot_nchar), 0.9, sprintf("%der", nchar(word)))
    }
  })
}
