# write_lines_utf8 -------------------------------------------------------------
write_lines_utf8 <- function(x, path)
{
  con <- file(path, "wt", encoding = "UTF-8")
  on.exit(close(con))
  
  writeLines(x, con)
}
