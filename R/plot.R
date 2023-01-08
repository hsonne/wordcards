# plot_word_cards --------------------------------------------------------------
#' @importFrom kwb.plot bestRowColumnSetting
#' @importFrom kwb.utils toPdf 
plot_word_cards <- function(
    words,
    frequencies = NULL,
    file = "",
    per_page = 32L, 
    to_pdf = TRUE,
    ...
)
{
  file <- kwb.utils::preparePdfIf(to_pdf, file, landscape = FALSE)
  on.exit(kwb.utils::finishAndShowPdfIf(to_pdf, file))
  
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
init_empty_plot <- function(xlim = c(-1, 1), ylim = c(-1, 1), ...)
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
    ylim = ylim,
    ...
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

# plot_card_from_card_info -----------------------------------------------------
plot_card_from_card_info <- function(
    card, what = c("frequencies", "words"), index = 1L, times = 1L, 
    syllable = "Silbe?"
)
{
  init_empty_plot(c(0, 1), c(0, 1))
  
  if (is.null(card)) {
    return()
  }
  
  ylim <- c(0.25, 0.9)

  plot_frequencies <- "frequencies" %in% what
  plot_words <- "words" %in% what
  
  if (plot_frequencies) {
    x <- card$frequencies
    add_sized_words_vertically(
      words = names(x), 
      weights = unname(x),
      xlim = if (plot_words) c(0, 0.45) else c(0.15, 0.85),
      ylim = ylim
    )
    
    # frequency ("x Silben")
    text(0.95, 0.1, adj = c(1, 1), paste(
      times, ifelse(times > 1L, "Silben", "Silbe")
    ))
    
    # rank (#1, #2, #3, ...)
    text(0.05, 0.1, adj = c(0, 1), paste0("#", index))
  }

  if (plot_words) {
    x <- card$words
    add_sized_words_vertically(
      words = names(x), 
      weights = unname(x),
      xlim = if (plot_frequencies) c(0.55, 1) else c(0.15, 0.85),
      ylim = ylim
    )
    
    # centered: "x Woerter"
    text(0.95, 0.1, adj = c(1, 1), paste(
      times, ifelse(times > 1L, "W\uF6rter", "Wort")
    ))
  }

  if (plot_frequencies && plot_words) {
    abline(v = 0.5, lty = 3L)
  }
}

# plot_syllable_cards ----------------------------------------------------------
plot_syllable_cards <- function(card_info, mfrow = c(6L, 3L))
{
  flap_indices <- function(indices, mfrow) {
    m <- matrix(indices, nrow = mfrow[1L], ncol = mfrow[2L], byrow = TRUE)
    m[, rev(seq_len(mfrow[2L])), drop = FALSE]
  }
  
  kwb.utils::toPdf(
    landscape = FALSE, 
    borderWidth.cm = 3, 
    borderHeight.cm = 3, 
    expressions = {
      
      graphics::par(mar = c(0.3, 0.3, 0.3, 0.3), mfrow = mfrow)
      
      n <- length(card_info)
      n_per_page <- prod(mfrow)
      
      page_numbers <- (seq_len(n) - 1L) %/% n_per_page
      indices_by_page <- split(seq_len(n), page_numbers)
      
      for (indices in indices_by_page) {
        
        length(indices) <- prod(mfrow)
        
        for (index in indices) {
          plot_card_from_card_info(
            card = card_info[[index]],
            what = "frequencies",
            index = index,
            times = sum(card_info[[index]]$frequencies),
            syllable = names(card_info)[index]
          )
        }
        
        for (index in c(t(flap_indices(indices, mfrow)))) {
          plot_card_from_card_info(
            card_info[[index]], 
            what = "words",
            index = index,
            times = sum(card_info[[index]]$words),
            syllable = names(card_info)[index]
          )
        }
      }
    }
  )
}
