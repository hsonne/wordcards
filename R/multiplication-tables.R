# MAIN -------------------------------------------------------------------------
if (FALSE)
{
  combis <- get_sample_of_multiplication_tasks()
  
  nrow(combis)
  
  head(combis)
  
  kwb.utils::toPdf(landscape = FALSE, {
    par(mfrow = c(2, 2), mar = c(1, 1, 1, 1))
    for (j in 1:3) {
      to <- j * 10L
      from <- to - 10L + 1L
      i <- from:to
      t1 <- to_product_text(combis$a, combis$b, combis$ab)
      t2 <- to_product_text(combis$a, combis$b, NA)
      t3 <- to_product_text(combis$a, NA, combis$ab)
      t4 <- to_product_text(NA, combis$b, combis$ab)
      for (text in list(t1, t2, t3, t4)) {
        text[i] %>%
          strings_to_character_matrix() %>%
          plot_text_matrix(cex = 1.8)
      }
    }
  })
  
}

`%>%` <- magrittr::`%>%`

# to_product_text --------------------------------------------------------------
to_product_text <- function(a, b, ab)
{
  na_to_empty <- function(x) kwb.utils::defaultIfNA(x, "")
  
  sprintf(
    "%2s*%2s=%2s", 
    na_to_empty(a), 
    na_to_empty(b), 
    na_to_empty(ab)
  )
}

# get_sample_of_multiplication_tasks -------------------------------------------
get_sample_of_multiplication_tasks <- function()
{
  combis <- kwb.utils::expandGrid(a = 0:11, b = 0:11) %>%
    dplyr::mutate(ab = a * b)
  
  combis[sample(nrow(combis)), ] %>%
    kwb.utils::resetRowNames()
}

# strings_to_character_matrix --------------------------------------------------
strings_to_character_matrix <- function(x)
{
  strsplit(x, "") %>%
    lapply(kwb.utils::enlargeVector, max(lengths(.))) %>%
    do.call(what = rbind)
}

# plot_text_matrix -------------------------------------------------------------
plot_text_matrix <- function(m, cex = 1)
{
  nr <- nrow(m)
  nc <- ncol(m)
  dx <- 0.5
  dy <- dx
  
  plot(
    NA, 
    NA, 
    xlim = c(0, nc), 
    ylim = c(nr, 0), 
    asp = 1, 
    axes = FALSE, 
    xlab = "",
    ylab = ""
  )
  
  abline(v = 0:nc, h = 0:nr, lty = 3L, col = "grey")
  
  text(c(col(m) - dx), c(row(m) - dy), m, cex = cex)
}
