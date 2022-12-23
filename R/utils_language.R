# has_ch_at --------------------------------------------------------------------
has_ch_at <- function(x, i)
{
  is_true_for_part_at(x, i, `==`, "ch")
}

# has_ck_at --------------------------------------------------------------------
has_ck_at <- function(x, i)
{
  is_true_for_part_at(x, i, `==`, "ck")
}

# has_ch_or_ck_at --------------------------------------------------------------
has_ch_or_ck_at <- function(x, i) 
{
  is_true_for_part_at(x, i, `%in%`, c("ch", "ck"))
}

# has_diphthong_at -------------------------------------------------------------
has_diphthong_at <- function(x, i)
{
  is_true_for_part_at(x, i, is_diphthong)
}

# has_sz_at --------------------------------------------------------------------
has_sz_at <- function(x, i)
{
  is_true_for_part_at(x, i, `==`, replace_specials("<sz>"))
}

# is_diphthong -----------------------------------------------------------------
# Die bekanntesten Schreibungen von Diphthongen im Deutschen sind ei, au, <ae>u 
# und eu; selten sind ai, oi und ui. 
is_diphthong <- function(x)
{
  x %in% replace_specials(c("ei", "au", "<ae>u", "eu", "ai", "oi", "ui"))
}

# is_true_for_part_at ----------------------------------------------------------
is_true_for_part_at <- function(x, i, fun, ...)
{
  sapply(x, function(y) nrow(y) >= i && isTRUE(fun(y$part[i], ...)))
}

# is_upper_case ----------------------------------------------------------------
is_upper_case <- function(x)
{
  chars <- strsplit(x, "")
  sapply(chars, function(y) {
    y[1L] == toupper(y[[1L]]) && y[[1L]] != replace_specials("<sz>")
  })
}

# is_vowel ---------------------------------------------------------------------
is_vowel <- function(chars)
{
  grepl(pattern = replace_specials("[aeiou<ae><oe><ue>y]"), chars)
}

# replace_specials -------------------------------------------------------------
replace_specials <- function(x)
{
  kwb.utils::multiSubstitute(x, list(
    "<Ae>" = "\uc4",
    "<Oe>" = "\ud6", 
    "<Ue>" = "\udc", 
    "<ae>" = "\ue4",
    "<oe>" = "\uf6",
    "<ue>" = "\ufc",
    "<sz>" = "\udf"
  ))
}

# to_upper_case ----------------------------------------------------------------
to_upper_case <- function(x)
{
  if (length(x) == 0L) {
    return(character())
  }
  
  x %>%
    strsplit("") %>%
    lapply(function(y) `[<-`(y, 1L, toupper(y[1L]))) %>%
    sapply(paste0, collapse = "")
}
