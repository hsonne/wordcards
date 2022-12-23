#
# Size words according to their frequency and arrange them vertically
# 
if (FALSE)
{
  par(mar = rep(3, 4))
  plot(NA, type = "n", xlim = c(0, 1), ylim = c(0, 1), xlab = "", ylab = "")
  
  add_sized_words_vertically(
    words = c("hallo", "Testwort", "Wort"),
    freqs = c(13L, 1L, 2L), 
    vertical_space_share = 0.3, 
    spacing_method = "proportional" # "equal"
  )
}

# add_sized_words_vertically ---------------------------------------------------
add_sized_words_vertically <- function(
    words, freqs, ylim = c(0.2, 0.8), vertical_space_share = 0.2, 
    spacing_method = "equal"
)
{
  weights <- kwb.utils::percentageOfSum(freqs)
  
  size <- get_size_info(words, cex = weights)
  
  abline(h = ylim, lty = 3L)
  
  expansion_factor <- cex_to_fit_rectangles(
    widths = size$width, 
    heights = size$height, 
    dx = 1,
    dy = diff(ylim),
    vertical_space_share = vertical_space_share
  )
  
  text_right_above(
    x = 0,
    y = arrange_vertically(
      heights = size$height * expansion_factor, 
      ylim = ylim, 
      method = spacing_method
    ),
    text = words, 
    cex = weights * expansion_factor
  )
}

# get_size_info ----------------------------------------------------------------
get_size_info <- function(words, cex)
{
  as.data.frame(do.call(rbind, mapply(
    FUN = get_text_size_and_line_space,
    words,
    cex,
    SIMPLIFY = FALSE
  )))
}

# get_text_size_and_line_space -------------------------------------------------
get_text_size_and_line_space <- function(word, cex = 1, units = "user")
{
  #word <- "hallo"
  
  width <- function(x) strwidth(x, units, cex = cex)
  height <- function(x) strheight(x, units, cex = cex)
  
  h1 <- height(word)
  h2 <- height(paste0(word, "\n", word))
  
  c(width = width(word), height = h1, space = h2 - 2 * h1)
}

# cex_to_fit_rectangles --------------------------------------------------------
cex_to_fit_rectangles <- function(
    widths, heights, dx = 1, dy = 1, vertical_space_share = 0.1
)
{
  w <- max(widths)
  h <- sum(heights)
  
  # h/w < 1: width determines cex
  # h/w > 1: height determines cex
  width_rules <- h/w < dy/dx
  
  kwb.utils::quotient(
    ifelse(width_rules, dx, (1 - vertical_space_share) * dy),
    ifelse(width_rules, w, h)
  )
}

# text_right_above -------------------------------------------------------------
text_right_above <- function(x, y, text, cex)
{
  text(x, y, text, cex = cex, adj = c(0, 0))
}

# arrange_vertically -----------------------------------------------------------
# arrange rectangles vertically
arrange_vertically <- function(heights, ylim, method = "proportional")
{
  method <- match.arg(method, c("proportional", "equal"))
  
  # ---------ymax 
  # |rect 3
  # --------
  # a*h2 + a*h3
  # -------- 
  # |rect 2
  # |
  # --------
  # a*h1 + a*h2
  # --------
  # |rect 1
  # ---------ymin
  #
  # space = a*h1 + a*h2 + a*h2 + a*h3 =
  #         a*(h1 + h2 + h2 + h3) = 
  #         a*(h1 + 2*h2 + h3)
  # <=> a = space / (h1 + 2*h2 + h3)
  # a = share of each height used as partial margin
  
  n <- length(heights)
  
  if (n == 1L) {
    return(ylim[1L])
  }
  
  space <- diff(ylim) - sum(heights)
  
  dy <- if (method == "equal") {
    
    space / (n - 1L)
    
  } else if (method == "proportional") {
    
    a <- space / sum(c(1, rep(2, n - 2L), 1) * heights)
    a * (heights[-n] + heights[-1L])
  }
  
  cumsum(c(ylim[1L], heights[-n] + dy))
}
