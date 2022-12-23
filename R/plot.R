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
