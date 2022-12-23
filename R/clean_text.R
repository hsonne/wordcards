# clean_text -------------------------------------------------------------------
#' @importFrom kwb.utils multiSubstitute
clean_text <- function(raw_text)
{
  kwb.utils::multiSubstitute(raw_text, list(
    "[^A-Za-zÄÖÜäöüß]" = ".",
    "[.]+" = " "
  ))
}
