library(magrittr)

# MAIN: Get raw text -----------------------------------------------------------
if (FALSE)
{
  files <- dir("texts", "\\.txt$", full.names = TRUE)
  
  file <- files[3L]

  raw_text <- read_text(file)
}

# MAIN: Create word cards ------------------------------------------------------
if (FALSE)
{
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

# MAIN: Find syllables ---------------------------------------------------------
if (FALSE)
{
  all_words <- sort(unique(tolower(text_to_words(raw_text))))
  
  is_syllable <- nchar(all_words) <= 3L
  
  syllables <- all_words[is_syllable]
  
  writeLines(syllables)
  
  # Remaining words, to be split into syllables
  words <- all_words[!is_syllable]
  
  #hyphenation <- lapply(words, call_hyphenation_service)
  #words_raw <- kwb.utils::multiSubstitute(words, get_syllable_replacements())
  #writeLines(grep("-", words_raw, value = TRUE))
  
  word_data <- split_words(words)
  
  lapply(split(word_data, word_data$type), function(df) {
    sort(table(df$part), decreasing = TRUE)
  })
  
  View(word_data)

  all_sets <- split(word_data, word_data$word)
  {
    sets <- all_sets
    syllables_by_pattern <- list()
    
    type_patterns <- sapply(sets, function(set) paste(set$type, collapse = "-"))
    
    pattern <- "1c-1v-2c-1v-1c"
    has_ch <- sapply(sets, function(x) nrow(x) >=3L && x$part[3L] == "ch")
    has_ck <- sapply(sets, function(x) nrow(x) >=3L && x$part[3L] == "ck")
    is_match <- (type_patterns == pattern & !has_ch & !has_ck)
    matching_words <- names(sets[is_match])
    syllables_by_pattern[[pattern]] <- split_after(matching_words, 3L)
    sets <- sets[!is_match]
    type_patterns <- type_patterns[!is_match]
    
    pattern <- "1c-1v-2c-1v"
    has_ch_ck <- sapply(sets, function(x) {
      nrow(x) >=3L && x$part[3L] %in% c("ch", "ck")
    })
    is_match <- (type_patterns == pattern & !has_ch_ck)
    matching_words <- names(sets[is_match])
    syllables_by_pattern[[pattern]] <- split_after(matching_words, 3L)
    sets <- sets[!is_match]
    type_patterns <- type_patterns[!is_match]
    
    pattern <- "2c-1v-1c-1v-1c"
    has_sz <- sapply(sets, function(x) nrow(x) >=3L && x$part[3L] == "ß")
    is_match <- (type_patterns == pattern & !has_sz)
    matching_words <- names(sets[is_match])
    syllables_by_pattern[[pattern]] <- split_after(matching_words, 3L)
    sets <- sets[!is_match]
    type_patterns <- type_patterns[!is_match]
    
    pattern <- "1c-2v-1c" # Ausnahme: säen
    has_diphthong <- sapply(sets, function(x) nrow(x) >=2L && is_diphthong(x$part[2L]))
    which(has_diphthong)
    is_match <- (type_patterns == pattern & has_diphthong)
    matching_words <- names(sets[is_match])
    syllables_by_pattern[[pattern]] <- split_after(matching_words, 0L)
    sets <- sets[!is_match]
    type_patterns <- type_patterns[!is_match]
    
    #"1c-2v-2c-1v-2c-1v-1c" = 0L, # "mais-kol-ben" "mais-sor-ten" "weib-lic-hen"
    #"1c-2v-1c" = 0L, # "hier" "säen"
    
    split_at <- list(
      "2v-2c" = 0L,
      "1c-1v-2c" = 0L,
      "1c-1v-3c" = 0L,
      "1c-1v-4c" = 0L,
      "1c-2v-1c" = 0L, # TODO: except: "le-os"
      "2c-1v-1c" = 0L,
      "2c-1v-2c" = 0L,
      "2c-2v-1c" = 0L,
      "3c-1v-2c" = 0L,
      "4c-1v-2c" = 0L,
      "1v-1c-1v-1c" = 0L,
      
      "1v-2c-1v" = 2L,
      "1v-2c-1v-1c" = 2L,
      "2v-1c-1v-1c" = 2L,
      "1c-1v-1c-1v-1c" = 2L,
      "1c-1v-1c-1v-2c" = 2L,
      "1c-1v-2c-1v-1c" = 2L, 
      "1c-1v-3c-1v-2c" = 2L, # TODO: except "wirklich"
      
      "1v-2c-1v-3c-1v" = c(2L, 6L),
      "1c-1v-1c-1v-1c-1v" = c(2L, 4L),
      "1v-2c-1v-1c-1v-1c" = c(2L, 4L),
      "1c-1v-1c-1v-2c-1v-1c" = c(2L, 5L), # TODO: exept: her-un-ter
      "1c-1v-1c-1v-3c-1v-1c" = c(2L, 6L), # TODO: except ko-mi-schen (sch)
            
      "1c-2v-1c-1v" = 3L,
      "2c-1v-1c-1v" = 3L,
      "2v-2c-1v-1c" = 3L,
      "1c-1v-1c-2v-1c" = 3L, # TODO: except: ge-fiel
      "1c-1v-2c-1v-2c" = 3L,
      "1c-1v-2c-1v-3c" = 3L, # TODO: except "gestärkt"
      "1c-2v-1c-1v-1c" = 3L,
      "1c-1v-2c-1v-2c-1v" = c(3L, 6L),  
      "1c-1v-2c-1v-2c-1v-1c" = c(3L, 6L),
      
      "1c-1v-3c-1v" = 4L,
      "2c-1v-2c-1v" = 4L,
      "1c-1v-3c-1v-1c" = 4L, # TODO: except: wi-scher (look for sch)
      "1c-1v-4c-1v-1c" = 4L, # TODO: except "menschen" (sch)
      "1c-2v-2c-1v-2c" = 4L,
      "2c-1v-2c-1v-1c" = 4L, # TODO: except ch, ck: kra-chen ste-cken
      "2c-1v-2c-1v-2c" = 4L,
      "2c-2v-1c-1v-1c" = 4L,
      
      "2c-1v-3c-1v" = 5L,
      "2c-1v-3c-1v-2c" = 5L, # TODO: except: "früh-stück"
      "4c-1v-1c-1v-1c" = 5L,
      
      "3c-1v-3c-1v" = 6L
    )
    
    for (pattern in names(split_at)) {
      is_match <- (type_patterns == pattern)
      matching_words <- names(sets[is_match])
      if (length(matching_words)) {
        syllables <- split_after(matching_words, split_at[[pattern]])
        syllables_by_pattern[[pattern]] <- syllables
        sets <- sets[!is_match]
        type_patterns <- type_patterns[!is_match]
      }
    }
  }

  (type_stats <- sort(table(type_patterns)))
  
  #"2c-1v-1c-1v", #3, skip sz at 4
  #"2c-1v-1c-1v" %in% names(type_stats)
  
  patterns <- rev(names(type_stats))
  
  p <- patterns

  kwb.utils::excludeNULL(lapply(p, function(pattern) {
    is_match <- (type_patterns == pattern)
    if (any(is_match)) names(sets[is_match])
  }))
  
  length(unlist(syllables_by_pattern))
  length(type_patterns)
  sort(names(sets))
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

# split_words -------------------------------------------------------------------
split_words <- function(words)
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
  
  type_name <- function(n, type) paste0(n, type) #, ifelse(n > 1L, "s", ""))

  word_data$type <- ifelse(
    word_data$value, 
    type_name(word_data$nchar, "v"), 
    type_name(word_data$nchar, "c")
  )
  
  word_data %>%
    kwb.utils::removeColumns("value") %>%
    kwb.utils::moveColumnsToFront("word")
}

# is_vowel ---------------------------------------------------------------------
is_vowel <- function(chars)
{
  grepl("[aeiouäöüy]", chars)
}

# split_after ------------------------------------------------------------------
split_after <- function(x, i)
{
  stopifnot(is.character(x))
  
  if (length(x) > 1L) {
    return(sapply(x, split_after, i, USE.NAMES = FALSE))
  }
  
  if (length(x) == 0L || all(i == 0L)) {
    return(x)
  }
  
  n_char <- nchar(x)
  
  stopifnot(all(i < n_char))
  
  if (length(i) == 1L) {
    return(paste0(substr(x, 1L, i), "-", substr(x, i + 1L, n_char)))
  }  
  
  if (length(i) == 2L) {
    return(paste0(
      substr(x, 1L, i[1L]), "-", 
      substr(x, i[1L] + 1L, i[2L]), "-",
      substr(x, i[2L] + 1L, n_char)
    ))
  }  
  
  stop("not implemented")  
}

# is_diphthong -----------------------------------------------------------------
# Die bekanntesten Schreibungen von Diphthongen im Deutschen sind ei, au, äu und 
# eu; selten sind ai, oi und ui. 
is_diphthong <- function(x)
{
  x %in% c("ei", "au", "äu", "eu", "ai", "oi", "ui.")
}
