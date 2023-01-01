# get_card_info ----------------------------------------------------------------
get_card_info <- function(formatted_stats, hyphenated_word_frequencies)
{
  lapply(stats::setNames(nm = names(formatted_stats)), function(syllable) {
    
    has_syllable <- grepl(
      paste0("(^|-)", syllable, "(-|$)"), 
      names(hyphenated_word_frequencies),
      ignore.case = TRUE
    )
    
    list(
      frequencies = formatted_stats[[syllable]],
      words = hyphenated_word_frequencies[has_syllable]
    )
  })
}
