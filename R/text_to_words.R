# text_to_words ----------------------------------------------------------------
#' @importFrom kwb.utils removeEmpty2
text_to_words <- function(raw_text)
{
  raw_text %>%
    clean_text() %>%
    paste(collapse = " ") %>%
    strsplit("\\s+") %>%
    `[[`(1L) %>%
    kwb.utils::removeEmpty2()
}
