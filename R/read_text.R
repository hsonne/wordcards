# read_text --------------------------------------------------------------------
#' @importFrom kwb.utils readLinesWithEncoding selectElements
read_text <- function(name)
{
  list_available_text_files() %>%
    kwb.utils::selectElements(name) %>%
    kwb.utils::readLinesWithEncoding(fileEncoding = "UTF-8") %>%
    remove_comment_lines()
}

# list_available_text_files ----------------------------------------------------
#' @importFrom kwb.utils removeExtension
#' @importFrom stats setNames
list_available_text_files <- function()
{
  path <- system.file("extdata/texts", package = "wordcards")
  files <- as.list(dir(path, "\\.txt$", full.names = TRUE))
  stats::setNames(files, kwb.utils::removeExtension(basename(unlist(files))))
}

# remove_comment_lines ---------------------------------------------------------
remove_comment_lines <- function(x)
{
  grep("^#", x, invert = TRUE, value = TRUE)
}
