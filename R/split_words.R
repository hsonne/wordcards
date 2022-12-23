# split_words ------------------------------------------------------------------
#' @importFrom kwb.utils findChanges moveColumnsToFront rbindAll removeColumns
#' @importFrom kwb.utils repeated renameColumns
#' @importFrom stats setNames
split_words <- function(words, style = 1L)
{
  word_chars <- stats::setNames(strsplit(tolower(words), ""), words)
  
  word_parts <- lapply(word_chars, function(chars) {
    changes <- kwb.utils::findChanges(is_vowel(chars))
    changes$part <- sapply(seq_len(nrow(changes)), function(i) {
      paste0(chars[changes$starts_at[i]:changes$ends_at[i]], collapse = "")
    })
    changes
  })
  
  word_data <- word_parts %>%
    kwb.utils::rbindAll(nameColumn = "word") %>%
    kwb.utils::renameColumns(list(starts_at = "from", ends_at = "to"))
  
  word_data$nchar <- word_data$to - word_data$from + 1L
  
  type_name <- function(n, type) {
    if (style == 1L) {
      paste0(n, type)
    } else if (style == 2L) {
      sapply(n, kwb.utils::repeated, x = type)
    }
  }
  
  word_data$type <- ifelse(
    word_data$value, 
    type_name(word_data$nchar, "v"), 
    type_name(word_data$nchar, "c")
  )
  
  parts <- word_data$part
  
  i <- which(parts %in% c("ch", "ck", "sch"))
  word_data$type[i] <- toupper(parts[i])
  
  pattern <- "^(.*)(sch)(.*)$"
  i <- which(grepl(pattern, parts) & word_data$type != "SCH")
  word_data$type[i] <- to_consonant_sequence_type(parts[i], pattern)
  
  pattern <- "^(.*)(ch|ck)(.*)$"
  i <- which(
    grepl(pattern, parts) & 
      !word_data$type %in% c("CH", "CK") & !grepl("SCH", word_data$type)
  )
  word_data$type[i] <- to_consonant_sequence_type(parts[i], pattern)
  
  i <- which(parts == replace_specials("<sz>"))
  word_data$type[i] <- "SZ"
  
  i <- which(is_diphthong(parts))
  word_data$type[i] <- "DT"
  
  word_data %>%
    kwb.utils::removeColumns("value") %>%
    kwb.utils::moveColumnsToFront("word")
}
