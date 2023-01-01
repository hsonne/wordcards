# call_hyphenation_service -----------------------------------------------------
#' @importFrom kwb.utils catAndRun isTryError
#' @importFrom rvest html_element html_text read_html
#' @importFrom utils URLencode
call_hyphenation_service <- function(word)
{
  url <- "https://www.silbentrennung24.de/wort/"
  
  html <- kwb.utils::catAndRun(
    paste("Looking up hyphenation for", word),
    try(rvest::read_html(paste0(url, utils::URLencode(word))))
  )
  
  if (kwb.utils::isTryError(html)) {
    return(NULL)
  }
  
  html %>%
    rvest::html_element(xpath = "//div[@id = 'termresult']") %>%
    rvest::html_text()  
}

# get_syllable_replacements ----------------------------------------------------
get_syllable_replacements <- function()
{
  c(
    "^all[aeiou]"
    #stats::setNames(as.list(paste0("-", syllables, "-")), syllables),
    #replacements_double_consonants(),
    #"^(be)([^ist])" = "\\1-\\2"
    , "-+" = "-"
    , "^-" = ""
    , "-$" = ""
  )
}

# replacements_double_consonants -----------------------------------------------
#' @importFrom stats setNames
replacements_double_consonants <- function()
{
  consonants <- strsplit("bdfglmnprt", "")[[1L]]
  replacements <- as.list(paste0(consonants, "-", consonants, "\\1"))
  stats::setNames(replacements, paste0(consonants, consonants, "(.{1,})$"))
}

