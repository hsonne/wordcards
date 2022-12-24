# rescale ----------------------------------------------------------------------
rescale <- function(x, target_range = c(0, 1))
{
  x_range <- range(x)
  dx <- diff(x_range)
  
  if (dx == 0) {
    return(rep(mean(target_range), length(x)))
  }
  
  ratio <- diff(target_range) / dx
  
  (x - x_range[1L]) * ratio  + target_range[1L]
}

# write_lines_utf8 -------------------------------------------------------------
write_lines_utf8 <- function(x, path)
{
  con <- file(path, "wt", encoding = "UTF-8")
  on.exit(close(con))
  
  writeLines(x, con)
}
