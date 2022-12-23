# correct_article_guesses ------------------------------------------------------
correct_article_guesses <- function(article_guesses)
{
  corrections <- replace_specials(c(
    "die Arbeitsplatte", 
    "die Arztpraxis", 
    "die Diele",
    "die K<ue>che",
    "die Praxis", 
    "die Reihe",
    "die Sonne",
    "die Spitze",
    "die Sp<ue>le", 
    "die Stube",
    "die Tulpen", 
    "die Untersuchung", 
    "die Weide",
    "die Wiese",
    "die Zwischenzeit"
  ))
  
  get_noun <- function(x) sapply(strsplit(x, " "), "[", 2L)
  
  i <- match(get_noun(article_guesses), get_noun(corrections))
  
  is_match <- !is.na(i)
  
  article_guesses[is_match] <- corrections[i[is_match]]
  
  article_guesses
}
