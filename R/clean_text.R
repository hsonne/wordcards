# clean_text -------------------------------------------------------------------
#' @importFrom kwb.utils multiSubstitute
clean_text <- function(raw_text)
{
  replacements <- list(
    # <placeholders> to be replaced next!
    "[^A-Za-z<Ae><Oe><Ue><ae><oe><ue><sz>]" = ".",
    "[.]+" = " "
  )
  
  names(replacements) <- replace_special_char_placeholders(names(replacements))
  
  kwb.utils::multiSubstitute(raw_text, replacements)
}
