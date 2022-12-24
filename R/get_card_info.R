# get_card_info ----------------------------------------------------------------
get_card_info <- function(formatted_stats, hyphenated_words)
{
  lapply(stats::setNames(nm = names(formatted_stats)), function(syllable) {
    list(
      frequencies = formatted_stats[[syllable]],
      words = sort(grep(
        paste0("(^|-)", syllable, "(-|$)"), 
        hyphenated_words, 
        value = TRUE, 
        ignore.case = TRUE
      ))
    )
  })
}
