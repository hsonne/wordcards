# add_sized_words_vertically ---------------------------------------------------
#' @importFrom graphics abline
#' @importFrom kwb.utils percentageOfSum
add_sized_words_vertically <- function(
    words, 
    weights, 
    xlim = c(0, 1), 
    ylim = c(0.2, 0.8), 
    space_share_y = 0.2, 
    space_method = "equal",
    col = "black",
    resize = FALSE
)
{
  #graphics::abline(h = ylim, v = xlim, lt = 3)
  
  freqs <- weights
  
  if (!resize) {
    weights <- rep(1, length(freqs))
  }
  
  # Get width, height and space between words for each word
  size_info <- get_size_info(words, weights)
  
  h <- sum(size_info$height)
  w <- max(size_info$width)
  
  dx <- diff(xlim)
  dy <- diff(ylim)
  
  cex <- if (h/w > dy/dx) {
    (1 - space_share_y) * dy / h
  } else {
    cex <- dx / w
  }
  
  print(cex*weights)
  
  size_info <- get_size_info(words, cex*weights)
  
  y <- arrange_vertically(heights = size_info$height, ylim = ylim)
  
  kwb.utils::printIf(TRUE, size_info)  

  text_right_above(xlim[1L], y, words, cex = cex*weights, col = col)
  
  # Write word frequencies as "1", "2", ..., to the left of the words
  if (length(words) > 1L) {
    text(
      x = xlim[1L] * 0.8, 
      y = y, 
      labels = paste0(freqs, "x"),
      cex = 1, 
      adj = c(1, 0), 
      col = "darkgrey"
    )
  }
}

# get_size_info ----------------------------------------------------------------
get_size_info <- function(words, cex = 1, units = "user")
{
  width <- function(x, cex) graphics::strwidth(x, units, cex = cex)
  height <- function(x, cex) graphics::strheight(x, units, cex = cex)

  heights_one_row <- mapply(height, words, cex)
  heights_two_rows <- mapply(height, paste0(words, "\n", words), cex)

  data.frame(
    width = mapply(width, words, cex),
    height = heights_one_row,
    space = heights_two_rows - 2 * heights_one_row
  )
}

# cex_to_fit_rectangles --------------------------------------------------------
#' @importFrom kwb.utils quotient
cex_to_fit_rectangles <- function(
    width, height, dx = 1, dy = 1, space_share_x = 0, space_share_y = 0
)
{
  if (height/width < dy/dx) {
    (1 - space_share_x) * dx / width
  } else {
    (1 - space_share_y) * dy / height
  } 
}

# text_right_above -------------------------------------------------------------
text_right_above <- function(x, y, text, cex, ...)
{
  text(x, y, text, cex = cex, adj = c(0, 0), ...)
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

